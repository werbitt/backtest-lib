{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Query
       ( lastHistoryVersion
       , connection
       , tradingDays
       , runMembersQuery
       , runReturnQuery
       , saveBacktestMeta
       , saveConstraints
       , saveHoldings
       , membersForDay
       , BacktestId
       ) where

import           Backtest.Db.Backtest       (Backtest' (..),
                                             BacktestCreatedAt' (..),
                                             backtestId, backtestTable)
import           Backtest.Db.Constraint     (Constraint' (..), constraintTable)
import           Backtest.Db.HistoryVersion (historyVersionId,
                                             historyVersionQuery,
                                             historyVersionUniverse)
import           Backtest.Db.Holding        (AssetClass (..), Holding' (..),
                                             holdingTable, pgAssetClass)
import           Backtest.Db.Ids            (BacktestId, BacktestId' (..),
                                             ConstraintId' (..),
                                             HistoryVersionId,
                                             HistoryVersionId' (..),
                                             HistoryVersionIdColumn,
                                             HoldingId' (..), SecurityId,
                                             SecurityId' (..), SecurityIdColumn,
                                             SecurityIdColumnNullable,
                                             pSecurityId)
import           Backtest.Db.Member         (memberDt, memberQuery,
                                             memberSecurityId, memberUniverse)
import           Backtest.Db.PriceHistory   (priceHistoryDt,
                                             priceHistoryHistoryVersion,
                                             priceHistoryQuery,
                                             priceHistorySecurityId,
                                             priceHistoryTotalReturnIndex)
import           Backtest.Types             (Asset (..), BacktestConfig,
                                             Constraints, Ticker, buffer,
                                             cutoff, frequency, getSecurityId,
                                             startDate, startValue)
import           Control.Arrow              (returnA)
import           Control.Lens               (to, (^.), _1)
import           Data.Int                   (Int64)
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text, pack)
import           Data.Time                  (Day)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye                    (Column, Query, QueryArr, aggregate,
                                             asc, constant, desc, distinct, in_,
                                             limit, max, maybeToNullable, null,
                                             orderBy, restrict, toNullable,
                                             (./=), (.<=), (.==), (.>=))
import           Opaleye.Manipulation       (runInsertMany, runInsertReturning)
import           Opaleye.PGTypes            (PGDate, PGFloat8, PGInt4, PGText)
import           Opaleye.RunQuery           (runQuery)
import           Prelude                    hiding (max, null)

-- |
-- = Database Connection

connectInfo :: PGS.ConnectInfo
connectInfo = PGS.ConnectInfo { PGS.connectHost = "localhost"
                              , PGS.connectPort = 5432
                              , PGS.connectUser = "backtest"
                              , PGS.connectPassword = ""
                              , PGS.connectDatabase = "micah" }

connection :: IO PGS.Connection
connection = PGS.connect connectInfo

restrictHistoryVersion :: HistoryVersionId -> QueryArr (Column PGInt4) ()
restrictHistoryVersion v = proc v' ->
  restrict -< v' .== constant (unHistoryVersionId v)

restrictDay :: Day -> QueryArr (Column PGDate) ()
restrictDay d = proc dt ->
  restrict -< dt .== constant d

restrictTickers :: [Ticker] -> QueryArr (Column PGText) ()
restrictTickers ts = proc t ->
  restrict -< in_ (constant <$> ts) t


--History Version----------------------------------------------------------------

lastHistoryVersionQuery :: Query HistoryVersionIdColumn
lastHistoryVersionQuery =  HistoryVersionId <$>
  aggregate max
   ((^.historyVersionId.to unHistoryVersionId) <$> historyVersionQuery)


lastHistoryVersion :: PGS.Connection -> IO HistoryVersionId
lastHistoryVersion conn = do
   result <- runQuery conn lastHistoryVersionQuery :: IO [HistoryVersionId]
   return $ head result


--Return-------------------------------------------------------------------------

lastTotalReturnIndexQuery
  :: HistoryVersionId -> Day -> SecurityId
  -> Query (Column PGDate, SecurityIdColumn, Column PGFloat8)
lastTotalReturnIndexQuery v d sid
  = limit 1 $ orderBy (desc (^._1)) $  proc () -> do
  ph <- priceHistoryQuery -< ()
  restrictHistoryVersion v -< ph^.priceHistoryHistoryVersion
  restrict -< ph^.priceHistoryDt .<= constant d
  restrict -< ph^.priceHistorySecurityId.to unSecurityId .== constant (unSecurityId sid)
  restrict -< ph^.priceHistoryTotalReturnIndex ./= 0
  returnA -< (ph^.priceHistoryDt, ph^.priceHistorySecurityId, ph^.priceHistoryTotalReturnIndex)

returnQuery :: HistoryVersionId -> Day -> Day -> SecurityId
            -> Query (SecurityIdColumn, Column PGFloat8, Column PGFloat8)
returnQuery v sd ed sid = proc () -> do
  (_, sid', startPrice) <- lastTotalReturnIndexQuery v sd sid -< ()
  (_, sid'', endPrice) <- lastTotalReturnIndexQuery v ed sid -< ()
  restrict -< unSecurityId sid' .== unSecurityId sid''
  returnA -< (sid', startPrice, endPrice)

runReturnQuery :: PGS.Connection -> HistoryVersionId -> Day -> Day -> [SecurityId]
               -> IO (M.Map SecurityId Double)
runReturnQuery conn v sd ed sids = do
  let q sid = runQuery conn (returnQuery v sd ed sid) :: IO [(SecurityId, Double, Double)]
  res <- concat <$>  mapM q sids
  return $ M.fromList (map (\(sid, sp, ep) -> (sid, (ep / sp) - 1)) res)


--Trading Days-------------------------------------------------------------------

tradingDaysQuery :: HistoryVersionId -> Day -> Query (Column PGDate)
tradingDaysQuery v sd = orderBy (asc id) $ distinct $ proc () -> do
  ph <- priceHistoryQuery -< ()
  restrict -< ph^.priceHistoryHistoryVersion .== constant (unHistoryVersionId v)
  restrict -< ph^.priceHistoryDt .>= constant sd
  returnA -< ph^.priceHistoryDt

tradingDays :: PGS.Connection -> HistoryVersionId -> Day  -> IO [Day]
tradingDays conn v d = runQuery conn (tradingDaysQuery v d) :: IO [Day]


--MembersForDay------------------------------------------------------------------

membersForDay :: HistoryVersionId -> Day -> Query SecurityIdColumn
membersForDay v d = proc () -> do
  m <- memberQuery -< ()
  u <- universeQuery v -< ()
  restrict -< m^.memberUniverse .== u
  restrictDay d -< m^.memberDt
  returnA -< m^.memberSecurityId

runMembersQuery :: PGS.Connection -> HistoryVersionId -> Day -> IO [SecurityId]
runMembersQuery conn v d = runQuery conn (membersForDay v d) :: IO [SecurityId]


--Backtest Meta------------------------------------------------------------------

saveBacktestMeta :: PGS.Connection
                 -> BacktestConfig
                 -> HistoryVersionId
                 -> IO BacktestId
saveBacktestMeta c bc v =  head <$>
  runInsertReturning c backtestTable Backtest
  { _backtestId = BacktestId Nothing
  , _backtestDesc = constant $ bc^.description
  , _backtestStartDt = constant $ bc^.startDate
  , _backtestStartValue = constant $ bc^.startValue
  , _backtestFrequency = constant . pack . show $ bc^.frequency
  , _backtestCutoff = constant $ bc^.cutoff
  , _backtestBuffer = constant $ bc^.buffer
  , _backtestCreatedAt = BacktestCreatedAt Nothing
  , _backtestHistoryVersion = constant v
  } (^.backtestId)


--Constraints--------------------------------------------------------------------

saveConstraints :: PGS.Connection -> BacktestId -> Constraints a -> IO Int64
saveConstraints c bId cts
  = runInsertMany c constraintTable constraints
  where
    constraints = flip map cts $ \(h, _, d) ->
      Constraint { _constraintId = ConstraintId Nothing
                 , _constraintDesc = constant d
                 , _constraintHook = constant h
                 , _constraintBacktestId = constant bId }


--Holdings-----------------------------------------------------------------------

saveHoldings :: PGS.Connection
             -> BacktestId
             -> Day
             -> [(Asset, Double)]
             -> IO Int64
saveHoldings c bId d hs = runInsertMany c holdingTable holdings
  where
    assetClass Cash = CashAsset
    assetClass (Equity _) = EquityAsset
    holdings = flip map hs $ \(a, v) ->
      Holding { _holdingId = HoldingId Nothing
              , _holdingBacktestId = constant bId
              , _holdingDt = constant d
              , _holdingAssetClass = constant (assetClass a)
              , _holdingSecurityId =
                  SecurityId $ maybeToNullable $
                  constant . unSecurityId <$> getSecurityId a
              , _holdingVal = constant v
              }


-- |
-- = ## Universe


universeQuery :: HistoryVersionId -> Query (Column PGText)
universeQuery v = proc () -> do
  h <- historyVersionQuery -< ()
  restrictHistoryVersion v -< h^.historyVersionId.to unHistoryVersionId
  returnA -< h^.historyVersionUniverse

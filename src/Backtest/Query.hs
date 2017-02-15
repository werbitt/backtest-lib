{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Query
       ( lastHistoryVersion
       , tradingDays
       , runMembersQuery
       , runReturnQuery
       , saveBacktestMeta
       , saveConstraints
       , saveHoldings
       , membersForDay
       , tickerOfSecurity
       , BacktestId
       ) where

import           Backtest.Db.Backtest        (Backtest' (..),
                                              BacktestCreatedAt' (..),
                                              backtestId, backtestTable)
import           Backtest.Db.Constraint      (Constraint' (..), constraintTable)
import           Backtest.Db.HistoryVersion  (historyVersionId,
                                              historyVersionQuery,
                                              historyVersionUniverse)
import           Backtest.Db.Holding         (AssetClass (..), Holding' (..),
                                              holdingTable)
import           Backtest.Db.Ids             (BacktestId, BacktestId' (..),
                                              ConstraintId' (..),
                                              HistoryVersionId,
                                              HistoryVersionId' (..),
                                              HistoryVersionIdColumn,
                                              HoldingId' (..), SecurityId,
                                              SecurityId' (..),
                                              SecurityIdColumn)
import           Backtest.Db.Member          (memberDt, memberQuery,
                                              memberSecurityId, memberUniverse)
import           Backtest.Db.PriceHistory    (priceHistoryDt,
                                              priceHistoryHistoryVersion,
                                              priceHistoryQuery,
                                              priceHistorySecurityId,
                                              priceHistoryTotalReturnIndex)
import           Backtest.Db.SecurityHistory (securityHistoryHistoryVersion,
                                              securityHistoryQuery,
                                              securityHistorySecurityId,
                                              securityHistoryTicker)
import           Backtest.Types              (Asset (..), CanDb, Constraints,
                                              HasBacktestConfig, Ticker,
                                              backtestConfig, buffer, conn,
                                              cutoff, description, frequency,
                                              getSecurityId, historyVersion,
                                              startDate, startValue)
import           Control.Arrow               (returnA)
import           Control.Lens                (to, view, (^.), _1)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Int                    (Int64)
import qualified Data.Map.Strict             as M
import           Data.Text                   (pack)
import           Data.Time                   (Day)
import qualified Database.PostgreSQL.Simple  as PGS
import           Opaleye                     (Column, Query, QueryArr,
                                              aggregate, asc, constant, desc,
                                              distinct, in_, limit, max,
                                              maybeToNullable, orderBy,
                                              restrict, (./=), (.<=), (.==),
                                              (.>=))
import           Opaleye.Manipulation        (runInsertMany, runInsertReturning)
import           Opaleye.PGTypes             (PGDate, PGFloat8, PGInt4, PGText)
import           Opaleye.RunQuery            (runQuery)
import           Prelude                     hiding (max, null)

-- |
-- = Database Connection

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
lastHistoryVersion c = do
   result <- runQuery c lastHistoryVersionQuery :: IO [HistoryVersionId]
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

runReturnQuery :: CanDb s m =>
                  Day -> Day -> [SecurityId] -> m (M.Map SecurityId Double)
runReturnQuery sd ed sids = do
  c <- view conn
  v <- view historyVersion
  let q sid = runQuery c (returnQuery v sd ed sid) :: IO [(SecurityId, Double, Double)]
  res <- concat <$> liftIO (mapM q sids)
  return $ M.fromList (map (\(sid, sp, ep) -> (sid, (ep / sp) - 1)) res)



--Trading Days-------------------------------------------------------------------

tradingDaysQuery :: HistoryVersionId -> Day -> Query (Column PGDate)
tradingDaysQuery v sd = orderBy (asc id) $ distinct $ proc () -> do
  ph <- priceHistoryQuery -< ()
  restrict -< ph^.priceHistoryHistoryVersion .== constant (unHistoryVersionId v)
  restrict -< ph^.priceHistoryDt .>= constant sd
  returnA -< ph^.priceHistoryDt

tradingDays :: CanDb r m => Day -> m [Day]
tradingDays d = do
  c <- view conn
  v <- view historyVersion
  liftIO $ runQuery c (tradingDaysQuery v d)


--MembersForDay------------------------------------------------------------------

membersForDay :: HistoryVersionId -> Day -> Query SecurityIdColumn
membersForDay v d = proc () -> do
  m <- memberQuery -< ()
  u <- universeQuery v -< ()
  restrict -< m^.memberUniverse .== u
  restrictDay d -< m^.memberDt
  returnA -< m^.memberSecurityId

runMembersQuery :: CanDb r m =>  Day -> m [SecurityId]
runMembersQuery d = do
  c <- view conn
  v <- view historyVersion
  liftIO $ runQuery c (membersForDay v d)


--Tickers------------------------------------------------------------------------

tickerOfSecurity :: QueryArr (SecurityIdColumn, HistoryVersionIdColumn)
                    (Column PGText)
tickerOfSecurity = proc (sid, hv) -> do
  sh <- securityHistoryQuery -< ()
  restrict -< sh^.securityHistorySecurityId.to unSecurityId .== unSecurityId sid
  restrict -< sh^.securityHistoryHistoryVersion.to unHistoryVersionId .==
    unHistoryVersionId hv
  returnA -< sh^.securityHistoryTicker


--Backtest Meta------------------------------------------------------------------

saveBacktestMeta :: (CanDb r m, HasBacktestConfig r) => m BacktestId
saveBacktestMeta = do
  c <- view conn
  v <- view historyVersion
  bc <- view backtestConfig
  liftIO $ head <$>
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

saveConstraints :: CanDb r m => BacktestId -> Constraints a -> m Int64
saveConstraints  bId cts  = do
  c <- view conn
  liftIO $ runInsertMany c constraintTable constraints
  where
    constraints = flip map cts $ \(h, _, d) ->
      Constraint { _constraintId = ConstraintId Nothing
                 , _constraintDesc = constant d
                 , _constraintHook = constant h
                 , _constraintBacktestId = constant bId }


--Holdings-----------------------------------------------------------------------

saveHoldings :: CanDb r m => BacktestId -> Day -> [(Asset, Double)] -> m Int64
saveHoldings bId d hs = do
  c <- view conn
  liftIO $ runInsertMany c holdingTable holdings
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

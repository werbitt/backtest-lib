{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Query
       (
         lastHistoryVersion
       , connection
       , tradingDays
       , universe
       , runMembersQuery
       , runReturnQuery
       , saveBacktestMeta
       , saveHoldings
       , BacktestMetaId
       , membersForDay
       , priceHistoryQuery
       , priceHistoryDt
       --, priceHistoryVolume
       --, priceHistoryClosePx
       ) where

import           Backtest.Db.HistoryVersion (HistoryVersion' (..),
                                             historyVersionId,
                                             historyVersionQuery,
                                             pHistoryVersion)
import           Backtest.Db.Ids            (HistoryVersionId,
                                             HistoryVersionId' (..),
                                             HistoryVersionIdColumn,
                                             PriceHistoryId,
                                             PriceHistoryId' (..),
                                             PriceHistoryIdColumn,
                                             PriceHistoryIdColumnMaybe,
                                             SecurityId, SecurityId' (..),
                                             SecurityIdColumn, pPriceHistoryId,
                                             pSecurityId)
import           Backtest.Db.PriceHistory   (priceHistoryDt,
                                             priceHistoryHistoryVersion,
                                             priceHistoryQuery,
                                             priceHistorySecurityId,
                                             priceHistoryTotalReturnIndex)
import           Backtest.Types             (Asset, GlobalId, Price, Return,
                                             Ticker, mkEquity)
import           Control.Arrow              (returnA)
import           Control.Lens               (makeLenses, to, (^.), _1)
import           Data.Int                   (Int64)
import qualified Data.Map.Strict            as M
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Data.Time                  (Day, UTCTime)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye                    (Column, Nullable, Query, QueryArr,
                                             Table (..), aggregate, asc,
                                             constant, desc, distinct, in_,
                                             limit, max, optional, orderBy,
                                             queryTable, required, restrict,
                                             (./=), (.<=), (.==), (.>=))
import           Opaleye.Manipulation       (runInsertMany, runInsertReturning)
import           Opaleye.PGTypes            (PGDate, PGFloat8, PGInt4, PGInt8,
                                             PGText, PGTimestamptz)
import           Opaleye.RunQuery           (runQuery)
import           Prelude                    hiding (max)

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

restrictHistoryVersion :: Int -> QueryArr (Column PGInt4) ()
restrictHistoryVersion v = proc v' ->
  restrict -< v' .== constant v

restrictDay :: Day -> QueryArr (Column PGDate) ()
restrictDay d = proc dt ->
  restrict -< dt .== constant d

restrictTickers :: [Ticker] -> QueryArr (Column PGText) ()
restrictTickers ts = proc t ->
  restrict -< in_ (constant <$> ts) t


--History Version----------------------------------------------------------------

lastHistoryVersionQuery :: Query HistoryVersionIdColumn
lastHistoryVersionQuery =  HistoryVersionId <$>
  (aggregate max
   ((^.historyVersionId.to unHistoryVersionId) <$> historyVersionQuery))


lastHistoryVersion :: PGS.Connection -> IO Int
lastHistoryVersion conn = do
   result <- runQuery conn lastHistoryVersionQuery :: IO [HistoryVersionId]
   return $ unHistoryVersionId . head $ result


--Return-------------------------------------------------------------------------

lastTotalReturnIndexQuery
  :: Int -> Day -> SecurityId
  -> Query (Column PGDate, SecurityIdColumn, Column PGFloat8)
lastTotalReturnIndexQuery v d sid
  = limit 1 $ orderBy (desc (^._1)) $  proc () -> do
  ph <- priceHistoryQuery -< ()
  restrictHistoryVersion v -< ph^.priceHistoryHistoryVersion
  restrict -< ph^.priceHistoryDt .<= constant d
  restrict -< ph^.priceHistorySecurityId.to unSecurityId .== constant (unSecurityId sid)
  restrict -< ph^.priceHistoryTotalReturnIndex ./= 0
  returnA -< (ph^.priceHistoryDt, ph^.priceHistorySecurityId, ph^.priceHistoryTotalReturnIndex)

returnQuery :: Int -> Day -> Day -> SecurityId
            -> Query (SecurityIdColumn, Column PGFloat8, Column PGFloat8)
returnQuery v sd ed sid = proc () -> do
  (_, sid', startPrice) <- lastTotalReturnIndexQuery v sd sid -< ()
  (_, sid'', endPrice) <- lastTotalReturnIndexQuery v ed sid -< ()
  restrict -< unSecurityId sid' .== unSecurityId sid''
  returnA -< (sid', startPrice, endPrice)

runReturnQuery :: PGS.Connection -> Int -> Day -> Day -> [SecurityId]
               -> IO (M.Map SecurityId Double)
runReturnQuery conn v sd ed sids = do
  let q sid = runQuery conn (returnQuery v sd ed sid) :: IO [(SecurityId, Double, Double)]
  res <- concat <$>  mapM q sids
  return $ M.fromList (map (\(sid, sp, ep) -> (sid, (ep / sp) - 1)) res)

-- |
-- == Trading Days

tradingDaysQuery :: Int -> Day -> Query (Column PGDate)
tradingDaysQuery v sd = orderBy (asc id) $ distinct $ proc () -> do
  ph <- priceHistoryQuery -< ()
  restrict -< ph^.priceHistoryHistoryVersion .== constant v
  restrict -< ph^.priceHistoryDt .>= constant sd
  returnA -< ph^.priceHistoryDt

tradingDays :: PGS.Connection -> Int -> Day  -> IO [Day]
tradingDays conn v d = runQuery conn (tradingDaysQuery v d) :: IO [Day]


-- |
-- = Members

data Members' a b c d = Members { _memberId         :: a
                                , _memberUniverse   :: b
                                , _memberDt         :: c
                                , _memberSecurityId :: d }

makeLenses ''Members'
makeAdaptorAndInstance "pMembers" ''Members'

type MembersColumns = Members' (Column PGInt4)
                               (Column PGText)
                               (Column PGDate)
                               SecurityIdColumn

type Members = Members' Int Text Day SecurityId

membersTable :: Table MembersColumns MembersColumns
membersTable = Table "members"
  (pMembers Members { _memberId = required "id"
                    , _memberUniverse = required "universe"
                    , _memberDt = required "dt"
                    , _memberSecurityId = pSecurityId . SecurityId $ required "security_id" })

membersQuery :: Query MembersColumns
membersQuery = queryTable membersTable

membersForDay :: Text -> Day -> Query SecurityIdColumn
membersForDay u d = proc () -> do
  m <- membersQuery -< ()
  restrict -< m^.memberUniverse .== constant u
  restrictDay d -< m^.memberDt
  returnA -< m^.memberSecurityId

runMembersQuery :: PGS.Connection -> Text -> Day -> IO [SecurityId]
runMembersQuery conn u d = runQuery conn (membersForDay u d) :: IO [SecurityId]

-- |
-- = Backtest Meta
data BacktestMetaId' a = BacktestMetaId { unBacktestMetaId :: a } deriving Show
makeAdaptorAndInstance "pBacktestMetaId" ''BacktestMetaId'
type BacktestMetaId = BacktestMetaId' Int
type BacktestMetaIdColumn = BacktestMetaId' (Column PGInt4)
type BacktestMetaIdColumnMaybe = BacktestMetaId' (Maybe (Column PGInt4))
type BacktestMetaIdColumnNullable = BacktestMetaId' (Column (Nullable PGInt4))

data BacktestMetaCreatedAt' a
  = BacktestMetaCreatedAt { unBacktestMetaCreatedAt :: a } deriving Show
makeAdaptorAndInstance "pBacktestMetaCreatedAt" ''BacktestMetaCreatedAt'
type BacktestMetaCreatedAt = BacktestMetaCreatedAt' UTCTime
type BacktestMetaCreatedAtColumn = BacktestMetaCreatedAt' (Column PGTimestamptz)
type BacktestMetaCreatedAtColumnMaybe = BacktestMetaCreatedAt' (Maybe (Column PGTimestamptz))

data BacktestMeta' a b c d e f g = BacktestMeta { _backtestMetaId             :: a
                                                , _backtestMetaStartDt        :: b
                                                , _backtestMetaStartValue     :: c
                                                , _backtestMetaFrequency      :: d
                                                , _backtestMetaWeighting      :: e
                                                , _backtestMetaCreatedAt      :: f
                                                , _backtestMetaHistoryVersion :: g
                                                }
makeLenses ''BacktestMeta'
makeAdaptorAndInstance "pBacktestMeta" ''BacktestMeta'

type BacktestMetaColumns = BacktestMeta' BacktestMetaIdColumn
                                         (Column PGDate)
                                         (Column PGFloat8) -- Change to numeric
                                         (Column PGText)
                                         (Column PGText)
                                         BacktestMetaCreatedAtColumn
                                         (Column PGInt4)

type BacktestMetaInsertColumns = BacktestMeta' BacktestMetaIdColumnMaybe
                                               (Column PGDate)
                                               (Column PGFloat8)
                                               (Column PGText)
                                               (Column PGText)
                                               BacktestMetaCreatedAtColumnMaybe
                                               (Column PGInt4)
type BacktestMeta
  = BacktestMeta' BacktestMetaId Day Double String String UTCTime Int

backtestMetaTable :: Table BacktestMetaInsertColumns BacktestMetaColumns
backtestMetaTable = Table "backtest_meta" $ pBacktestMeta BacktestMeta
  { _backtestMetaId = pBacktestMetaId . BacktestMetaId $ optional "id"
  , _backtestMetaStartDt = required "start_dt"
  , _backtestMetaStartValue = required "start_value"
  , _backtestMetaFrequency = required "frequency"
  , _backtestMetaWeighting = required "weighting"
  , _backtestMetaCreatedAt = pBacktestMetaCreatedAt . BacktestMetaCreatedAt $ optional "created_at"
  , _backtestMetaHistoryVersion = required "history_version"
  }

backtestMetaQuery :: Query BacktestMetaColumns
backtestMetaQuery = queryTable backtestMetaTable

saveBacktestMeta :: PGS.Connection -> Day -> Double -> String -> String -> Int -> IO BacktestMetaId
saveBacktestMeta c sd sv frq wgt v =  head <$>
  runInsertReturning c backtestMetaTable BacktestMeta
  { _backtestMetaId = BacktestMetaId Nothing
  , _backtestMetaStartDt = constant sd
  , _backtestMetaStartValue = constant sv
  , _backtestMetaFrequency = constant frq
  , _backtestMetaWeighting = constant wgt
  , _backtestMetaCreatedAt = BacktestMetaCreatedAt Nothing
  , _backtestMetaHistoryVersion = constant v
  } (^.backtestMetaId)

-- |
-- = Holdings
data HoldingId' a = HoldingId { unHoldingId :: a } deriving Show
$(makeAdaptorAndInstance "pHoldingId" ''HoldingId')
type HoldingId = HoldingId' Int64
type HoldingIdColumn = HoldingId' (Column PGInt8)
type HoldingIdColumnMaybe = HoldingId' (Maybe (Column PGInt8))
type HoldingIdColumnNullable = HoldingId' (Column (Nullable PGInt8))

data Holding' a b c d e = Holding { _holdingId         :: a
                                  , _holdingBacktestId :: b
                                  , _holdingDt         :: c
                                  , _holdingAsset      :: d
                                  , _holdingVal        :: e
                                  }
makeLenses ''Holding'

makeAdaptorAndInstance "pHolding" ''Holding'

type HoldingColumns = Holding' HoldingIdColumn
                               BacktestMetaIdColumn
                               (Column PGDate)
                               (Column PGText)
                               (Column PGFloat8)
type HoldingInsertColumns = Holding' HoldingIdColumnMaybe
                                     BacktestMetaIdColumn
                                     (Column PGDate)
                                     (Column PGText)
                                     (Column PGFloat8)
type Holding = Holding' HoldingId BacktestMetaId Day Asset Double

holdingTable :: Table HoldingInsertColumns HoldingColumns
holdingTable = Table "holdings" $ pHolding Holding
  { _holdingId = pHoldingId . HoldingId $ optional "id"
  , _holdingBacktestId = pBacktestMetaId . BacktestMetaId $ required "backtest_id"
  , _holdingDt = required "dt"
  , _holdingAsset = required "asset"
  , _holdingVal = required "val"
  }

holdingQuery :: Query HoldingColumns
holdingQuery = queryTable holdingTable

saveHoldings :: PGS.Connection -> BacktestMetaId -> Day -> [(Asset, Double)] -> IO Int64
saveHoldings c bId d hs = runInsertMany c holdingTable holdings
  where
    holdings = flip map hs $ \(a, v) ->
      Holding { _holdingId = HoldingId Nothing
              , _holdingBacktestId = constant bId
              , _holdingDt = constant d
              , _holdingAsset = constant a
              , _holdingVal = constant v
              }

-- holdingAll :: CanOpaleye c e m => m [holding]
-- holdingAll = liftQuery holdingQuery




-- |
-- = ## Universe


universeQuery :: Text -> Day -> Query SecurityIdColumn
universeQuery u d = proc () -> do
  m <- membersQuery -< ()
  restrict -< m^.memberUniverse .== constant u
  restrictDay d -< m^.memberDt
  returnA -< m^.memberSecurityId

--skewOfTicker :: QueryArr (Column Ticker) (Column Double)

universe :: PGS.Connection -> Text -> Day -> IO [SecurityId]
universe conn u d = runQuery conn (universeQuery u d) :: IO [SecurityId]

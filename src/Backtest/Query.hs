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
       , priceHistoryTicker
       , priceHistoryDt
       , priceHistoryVolume
       , priceHistoryClosePx
       ) where

import           Backtest.Types             (Asset, Price, Return, Ticker,
                                             mkEquity)
import           Control.Arrow              (returnA)
import           Control.Lens               (makeLenses, (^.), _1)
import           Data.Int                   (Int64)
import qualified Data.Map.Strict            as M
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
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
                                             PGNumeric, PGText, PGTimestamptz)
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


-- |
-- = History Version
--
-- This is used because we don't have a prope security master.
-- This way we can can have different security data associated with
-- different backtests.

data HistoryVersion' a = HistoryVersion { _version :: a } deriving Show

type HistoryVersion = HistoryVersion' Int
type HistoryVersionColumn = HistoryVersion' (Column PGInt4)

$(makeAdaptorAndInstance "pHistoryVersion" ''HistoryVersion')

historyVersionTable :: Table HistoryVersionColumn HistoryVersionColumn
historyVersionTable = Table "history_version"
  (pHistoryVersion HistoryVersion { _version = required "id" })

historyVersionQuery :: Query HistoryVersionColumn
historyVersionQuery = queryTable historyVersionTable

lastHistoryVersionQuery :: Query HistoryVersionColumn
lastHistoryVersionQuery = aggregate
  (pHistoryVersion HistoryVersion { _version = max })
  historyVersionQuery

lastHistoryVersion :: PGS.Connection -> IO Int
lastHistoryVersion conn = do
   result <- runQuery conn lastHistoryVersionQuery :: IO [HistoryVersion]
   return $ _version . head $ result


-- |
-- = Price History
data PriceHistoryBeta' a
  =  PriceHistoryBeta { unPriceHistoryBeta :: a } deriving Show
makeAdaptorAndInstance "pPriceHistoryBeta" ''PriceHistoryBeta'
type PriceHistoryBeta = PriceHistoryBeta' Double
type PriceHistoryBetaColumn = PriceHistoryBeta' (Column Double)
type PriceHistoryBetaColumnNullable = PriceHistoryBeta' (Column (Nullable Double))


data PriceHistory' a b c d e f g h i
  = PriceHistory { _priceHistoryDt               :: a
                 , _priceHistoryTicker           :: b
                 , _priceHistoryOpenPx           :: c
                 , _priceHistoryClosePx          :: d
                 , _priceHistoryTotalReturn      :: e
                 , _priceHistoryTotalReturnIndex :: f
                 , _priceHistoryVolume           :: g
                 , _priceHistoryBeta             :: h
                 , _priceHistoryHistoryVersion   :: i }
makeLenses ''PriceHistory'
$(makeAdaptorAndInstance "pPriceHistory" ''PriceHistory')


type PriceHistoryColumn = PriceHistory' (Column PGDate)
                                        (Column PGText)
                                        (Column PGFloat8)
                                        (Column PGFloat8)
                                        (Column PGFloat8)
                                        (Column PGFloat8)
                                        (Column PGInt8)
                                        PriceHistoryBetaColumnNullable
                                        (Column PGInt4)
type PriceHistory
  = PriceHistory' Day Ticker Price Price Return Price Double (Maybe PriceHistoryBeta) Int


priceHistoryTable :: Table PriceHistoryColumn PriceHistoryColumn
priceHistoryTable = Table "price_history"
  (pPriceHistory
   PriceHistory { _priceHistoryDt = required "dt"
                , _priceHistoryTicker = required "ticker"
                , _priceHistoryOpenPx = required "open_px"
                , _priceHistoryClosePx = required "close_px"
                , _priceHistoryTotalReturn = required "total_return"
                , _priceHistoryTotalReturnIndex = required "total_return_index"
                , _priceHistoryVolume = required "volume"
                , _priceHistoryBeta = pPriceHistoryBeta . PriceHistoryBeta $ required "beta"
                , _priceHistoryHistoryVersion = required "history_version" })

priceHistoryQuery :: Query PriceHistoryColumn
priceHistoryQuery = queryTable priceHistoryTable


-- |
-- == Return

lastTotalReturnIndexQuery
  :: Int -> Day -> Ticker -> Query (Column PGDate, Column PGText,Column PGFloat8)
lastTotalReturnIndexQuery v d t = limit 1 $ orderBy (desc (^._1)) $  proc () -> do
  ph <- priceHistoryQuery -< ()
  restrictHistoryVersion v -< ph^.priceHistoryHistoryVersion
  restrict -< ph^.priceHistoryDt .<= constant d
  restrict -< ph^.priceHistoryTicker .== constant t
  restrict -< ph^.priceHistoryTotalReturnIndex ./= 0
  returnA -< (ph^.priceHistoryDt, ph^.priceHistoryTicker, ph^.priceHistoryTotalReturnIndex)

returnQuery :: Int -> Day -> Day -> Ticker
            -> Query (Column PGText, Column PGFloat8, Column PGFloat8)
returnQuery v sd ed t = proc () -> do
  (_, t', startPrice) <- lastTotalReturnIndexQuery v sd t -< ()
  (_, t'', endPrice) <- lastTotalReturnIndexQuery v ed t -< ()
  restrict -< t' .== t''
  returnA -< (t', startPrice, endPrice)

runReturnQuery :: PGS.Connection -> Int -> Day -> Day -> [Ticker] -> IO (M.Map Asset Double)
runReturnQuery conn v sd ed ts = do
  let q t = runQuery conn (returnQuery v sd ed t) :: IO [(Ticker, Double, Double)]
  res <- concat <$>  mapM q ts
  return $ M.fromList (map (\(t, sp, ep) -> (mkEquity t, (ep / sp) - 1)) res)

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

data Members' a b c d = Members { _memberId             :: a
                                , _memberDt             :: b
                                , _memberTicker         :: c
                                , _memberHistoryVersion :: d }
makeLenses ''Members'
type Members = Members' Int64 Day Ticker Int


type MembersColumn = Members' (Column PGInt8)
                              (Column PGDate)
                              (Column PGText)
                              (Column PGInt4)

$(makeAdaptorAndInstance "pMembers" ''Members')

membersTable :: Table MembersColumn MembersColumn
membersTable = Table "members"
  (pMembers Members { _memberId = required "id"
                    , _memberDt = required "dt"
                    , _memberTicker =  required "ticker"
                    , _memberHistoryVersion = required "history_version" })

membersQuery :: Query MembersColumn
membersQuery = queryTable membersTable

membersForDay :: Int -> Day -> Query (Column PGText)
membersForDay v d = proc () -> do
  m <- membersQuery -< ()
  restrictHistoryVersion v -< m^.memberHistoryVersion
  restrictDay d -< m^.memberDt
  returnA -< m^.memberTicker

runMembersQuery :: PGS.Connection -> Int -> Day -> IO [Ticker]
runMembersQuery conn v d = runQuery conn (membersForDay v d) :: IO [Ticker]

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


universeQuery :: Int -> Day -> Query (Column PGText)
universeQuery v d = proc () -> do
  Members _ d' t v' <- membersQuery -< ()
  restrictHistoryVersion v -< v'
  restrictDay d -< d'
  returnA -< t

--skewOfTicker :: QueryArr (Column Ticker) (Column Double)

universe :: PGS.Connection -> Int -> Day -> IO [Ticker]
universe conn v d = runQuery conn (universeQuery v d) :: IO [Ticker]

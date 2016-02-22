{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Rd.Query
       (
         lastHistoryVersion
       , connection
       , tradingDays
       , universe
       , runMembersQuery
       ) where

import           Control.Arrow              (returnA)
import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time                  (Day)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye                    (Column, Nullable, Query, QueryArr,
                                             Table (..), aggregate, asc,
                                             constant, distinct, max, optional,
                                             orderBy, queryTable, required,
                                             restrict, (.==), (.>=))
import           Opaleye.PGTypes            (PGDate, PGFloat8, PGInt4, PGInt8,
                                             PGNumeric, PGText)
import           Opaleye.RunQuery           (QueryRunnerColumnDefault (..),
                                             fieldQueryRunnerColumn, runQuery)
import           Prelude                    hiding (max)
import           Rd.Types                   (Price, Return, Ticker, mkTicker)

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

data PriceHistory' a b c d e f g h
  = PriceHistory { _dt               :: a
                 , _ticker           :: b
                 , _openPx           :: c
                 , _closePx          :: d
                 , _totalReturn      :: e
                 , _totalReturnIndex :: f
                 , _beta             :: g
                 , _historyVersion   :: h }

type PriceHistory
  = PriceHistory' Day Ticker Price Price Return Price Double Int
type PriceHistoryColumn = PriceHistory' (Column PGDate)
                                        (Column PGText)
                                        (Column PGNumeric)
                                        (Column PGNumeric)
                                        (Column PGNumeric)
                                        (Column PGNumeric)
                                        (Column (Nullable PGNumeric))
                                        (Column PGInt4)

$(makeAdaptorAndInstance "pPriceHistory" ''PriceHistory')

priceHistoryTable :: Table PriceHistoryColumn PriceHistoryColumn
priceHistoryTable = Table "price_history"
  (pPriceHistory PriceHistory { _dt = required "dt"
                              , _ticker = required "ticker"
                              , _openPx = required "open_px"
                              , _closePx = required "close_px"
                              , _totalReturn = required "total_return"
                              , _totalReturnIndex = required "total_return_index"
                              , _beta = required "beta"
                              , _historyVersion = required "history_version" })

priceHistoryQuery :: Query PriceHistoryColumn
priceHistoryQuery = queryTable priceHistoryTable

-- |
-- == Trading Days

tradingDaysQuery :: Int -> Day -> Query (Column PGDate)
tradingDaysQuery v sd = orderBy (asc id) $ distinct $ proc () -> do
  PriceHistory dt _ _ _ _ _ _ v' <- priceHistoryQuery -< ()
  restrict -< v' .== constant v
  restrict -< dt .>= constant sd
  returnA -< dt

tradingDays :: PGS.Connection -> Int -> Day  -> IO [Day]
tradingDays conn v d = runQuery conn (tradingDaysQuery v d) :: IO [Day]


-- |
-- = Members

data Members' a b c d = Members { _memberId             :: a
                                , _memberDt             :: b
                                , _memberTicker         :: c
                                , _memberHistoryVersion :: d }
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
  Members _ dt t v' <- membersQuery -< ()
  restrictHistoryVersion v -< v'
  restrictDay d -< dt
  returnA -< t

runMembersQuery :: PGS.Connection -> Int -> Day -> IO [Ticker]
runMembersQuery conn v d = runQuery conn (membersForDay v d) :: IO [Ticker]


-- |
-- = Skew
data SkewHistory' a b c d = SkewHistory { _skewDt             :: a
                                        , _skewTicker         :: b
                                        , _skewSkew           :: c
                                        , _skewHistoryVersion :: d}
type SkewHistory = SkewHistory' Day Ticker Double
type SkewHistoryColumn = SkewHistory' (Column PGDate)
                                      (Column PGText)
                                      (Column PGFloat8)
                                      (Column PGInt4)
$(makeAdaptorAndInstance "pSkewHistory" ''SkewHistory')

skewHistoryTable :: Table SkewHistoryColumn SkewHistoryColumn
skewHistoryTable = Table "skew_history"
  (pSkewHistory SkewHistory { _skewDt = required "dt"
                            , _skewTicker = required "ticker"
                            , _skewSkew = required "skew_history.skew"
                            , _skewHistoryVersion = required "history_version"})
skewHistoryQuery :: Query SkewHistoryColumn
skewHistoryQuery = queryTable skewHistoryTable

skewQuery :: Int -> Day -> Query (Column PGText, Column PGFloat8)
skewQuery v d = proc () ->  do
  SkewHistory dt t sk v' <- skewHistoryQuery -< ()
  restrictHistoryVersion v -< v'
  restrictDay d -< dt
  returnA -< (t, sk)

runSkewQuery :: PGS.Connection -> Int -> Day -> IO[(Ticker, Double)]
runSkewQuery conn v d = runQuery conn (skewQuery v d) :: IO [(Ticker, Double)]

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

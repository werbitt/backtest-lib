{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Backtest.Example
       (run') where

import qualified Backtest.Backtest          as B
import           Backtest.Constraint        (addGlobalFilter, addShortFilter)
import qualified Backtest.Db.Ids            as ID
import           Backtest.Optimize          (optimize)
import qualified Backtest.Query             as Q
import           Backtest.Types             (AppConfig (..), Asset, Backtest,
                                             BacktestConfig (..), CanDb,
                                             Constraints, DbConfig (..),
                                             DbEnv (..), Env (..), Filter (..),
                                             HasAsset, HasBacktestConfig,
                                             Ordinal (..), PortfolioW,
                                             Strategy (..), Ticker,
                                             Weekday (..), asset, conn,
                                             dbConnectInfo, historyVersion,
                                             mkEquity, mkFrequency, unBacktest,
                                             unTicker)
import           Control.Arrow              (returnA)
import           Control.Lens               (view, (^.))
import           Control.Monad.Logger       (runStdoutLoggingT)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.Trans        (liftIO)
import           Data.Char                  (toLower)
import           Data.List                  (sortBy)
import           Data.Time                  (Day, fromGregorian)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye                    as O


data ExampleData = ED { edAsset  :: Asset
                      , edTicker :: Ticker }

instance HasAsset ExampleData where
  asset = edAsset

instance HasTicker ExampleData where
  getTicker = Just . edTicker

class HasTicker a where
  getTicker :: a -> Maybe Ticker

alpha :: CanDb r m => Strategy m ExampleData
alpha = Strategy query $ sortBy (\_ _ -> EQ)

exampleQuery :: ID.HistoryVersionId -> Day -> O.Query (ID.SecurityIdColumn, (O.Column O.PGText))
exampleQuery v d = proc () -> do
  sid <- Q.membersForDay v d -< ()
  t <- Q.tickerOfSecurity -< (sid, O.constant v)
  returnA -< (sid, t)

runExampleQuery :: PGS.Connection -> ID.HistoryVersionId -> Day -> IO [(ID.SecurityId, Ticker)]
runExampleQuery c v d = O.runQuery c (exampleQuery v d)

query :: CanDb r m => Day -> m [ExampleData]
query d' = do
  conn <- view conn
  v <- view historyVersion
  res <- liftIO $ runExampleQuery conn v d'
  return $ map (\(sid, t) -> ED (mkEquity sid) t) res


constraintNoSecondB :: HasTicker a =>  a -> Filter
constraintNoSecondB a = maybe Include (hasB . map toLower . unTicker) (getTicker a)
  where
    hasB (_:'b':_) = Exclude
    hasB _ = Include

constraintNoZ :: ExampleData -> Filter
constraintNoZ a = maybe Include (hasB . map toLower . unTicker) (Just $ edTicker a)
  where
    hasB ('z':_) = Exclude
    hasB _ = Include

constraints :: Constraints ExampleData
constraints = addGlobalFilter constraintNoSecondB "No Second B" $
  addShortFilter constraintNoZ "No Z" []

d :: Day
d = fromGregorian 2016 1 26

run' :: Backtest a -> IO a
run' m = do
  let config =
        AppConfig { _acDbConfig = DbConfig
                                  PGS.ConnectInfo { PGS.connectHost = "localhost"
                                                   , PGS.connectPort = 5432
                                                   , PGS.connectUser = "backtest"
                                                   , PGS.connectPassword = ""
                                                   , PGS.connectDatabase = "backtest" }
                  , _acBacktestConfig = BacktestConfig { _description = "Alphabetical"
                                                       , _startDate = fromGregorian 2006 1 1
                                                       , _startValue = 2000000
                                                       , _frequency = mkFrequency Third Friday 2
                                                       , _cutoff = 0.05
                                                       , _buffer = 0 }}
  c <- PGS.connect $ config ^. dbConnectInfo
  v <- Q.lastHistoryVersion c
  runStdoutLoggingT $ runReaderT (unBacktest m) (Env config (DbEnv c v))

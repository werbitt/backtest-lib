module Backtest.Runner where

import           Backtest.Query             (lastHistoryVersion)
import           Backtest.Types             (AppConfig, Backtest, DbEnv (..),
                                             Env (..), dbConnectInfo,
                                             unBacktest)
import           Control.Lens               ((^.))
import           Control.Monad.Logger       (runStderrLoggingT)
import           Control.Monad.Reader       (runReaderT)
import           Database.PostgreSQL.Simple (connect)

run :: AppConfig -> Backtest a -> IO a
run config m = do
  c <- connect $ config ^. dbConnectInfo
  v <- lastHistoryVersion c
  runStderrLoggingT $ runReaderT (unBacktest m) (Env config (DbEnv c v))

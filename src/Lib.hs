{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      run
    ) where

import qualified Backtest.Query             as Q
import           Backtest.Types             (AppConfig, Backtest, DbEnv (..),
                                             Env (..), conn, dbConfig,
                                             dbConnectInfo, historyVersion,
                                             unBacktest)
import           Control.Lens               (set, (.=), (^.))
import           Control.Monad.Logger       (runStderrLoggingT)
import           Control.Monad.Reader       (runReaderT)
import           Database.PostgreSQL.Simple (connect)


run :: AppConfig -> Backtest a -> IO a
run config m = do
  c <- connect $ config ^. dbConnectInfo
  v <- Q.lastHistoryVersion c
  runStderrLoggingT $ runReaderT (unBacktest m) (Env config (DbEnv c v))

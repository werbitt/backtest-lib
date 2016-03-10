{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      run
    ) where

import qualified Backtest.Query       as Q
import           Backtest.Types       (AppConfig, Backtest, unBacktest)
import           Control.Monad.Reader (runReaderT)




run :: AppConfig -> Backtest a -> IO a
run config m = do
  conn <- Q.connection
  version <- Q.lastHistoryVersion conn
  -- update conn and version in config
  runReaderT (unBacktest m) config

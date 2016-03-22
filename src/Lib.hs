{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      run
    ) where

import qualified Backtest.Query       as Q
import           Backtest.Types       (AppConfig, Backtest, connection,
                                       dbConfig, historyVersion, unBacktest)
import           Control.Lens         (set)
import           Control.Monad.Reader (runReaderT)



run :: AppConfig -> Backtest a -> IO a
run config m = do
  conn <- Q.connection
  version <- Q.lastHistoryVersion conn
  let config' =  set (dbConfig . connection) conn config
  let config'' = set (dbConfig . historyVersion) version config'
  runReaderT (unBacktest m) config''

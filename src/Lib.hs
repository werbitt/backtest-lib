{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      run
    ) where

import           Control.Monad.Reader (runReaderT)
import           Rd.Query             (connection, lastHistoryVersion)
import           Rd.Types             (Backtest, Env (..))

run :: Backtest a -> IO a
run m = do
  conn <- connection
  version <- lastHistoryVersion conn
  runReaderT m $ Env conn version

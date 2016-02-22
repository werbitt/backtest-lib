{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      run
    ) where

import           Control.Monad.Reader (runReaderT)
import qualified Rd.Query             as Q
import           Rd.Types             (Backtest, Params, mkEnv, unBacktest)

run :: Params -> Backtest a -> IO a
run params m = do
  conn <- Q.connection
  version <- Q.lastHistoryVersion conn
  runReaderT (unBacktest m) (mkEnv conn version params)

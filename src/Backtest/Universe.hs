module Backtest.Universe
       (
         universe
       ) where

import qualified Backtest.Query      as Q
import           Backtest.Types      (Backtest, Ticker, connection,
                                      historyVersion)
import           Control.Lens        (view)
import           Control.Monad.Trans (liftIO)
import           Data.Time           (Day)

universe :: Day -> Backtest [Ticker]
universe d = do
  conn <- view connection
  v <- view historyVersion
  liftIO $ Q.universe conn v d

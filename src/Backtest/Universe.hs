module Rd.Universe
       (
         universe
       ) where

import           Control.Lens        (view)
import           Control.Monad.Trans (liftIO)
import           Data.Time           (Day)
import qualified Rd.Query            as Q
import           Rd.Types            (Backtest, Ticker, connection,
                                      historyVersion)

universe :: Day -> Backtest [Ticker]
universe d = do
  conn <- view connection
  v <- view historyVersion
  liftIO $ Q.universe conn v d

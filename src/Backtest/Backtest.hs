module Rs.Backtest
       (
       ) where

import           Control.Lens         (view)
import           Control.Monad.Reader (ask)
import qualified Data.Set             as S
import           Data.Time            (Day)
import           Pipes                (Pipe, Producer, await)
import           Rd.Dates             (rebalanceDays, tradingDays)
import           Rd.Optimize          (optimize)
import           Rd.Portfolio         (fromWeighted, marketValue)
import           Rd.Types             (Backtest, Portfolio, frequency, params)


-- Needs
-- - Start Day
-- - Start Value
-- - Optimization Parameters
--   - Rebalance Frequency
--   - High/Low Cutoff
--   - Cash Buffer

backtest' =  do
  (d:ds) <- tradingDays
  freq   <- view (params . frequency)
  let rds = rebalanceDays freq ds
  op <- optimize d
  return undefined



backtest :: Pipe (Day, Portfolio) (Day, Portfolio) Backtest ()
backtest = do
  (d', p') <- await
  return undefined

rebalance :: Day -> S.Set Day -> Portfolio -> Backtest Portfolio
rebalance d ds p = if S.member d ds
                   then do
                        opt <- optimize d
                        let mv = marketValue p
                        return $ fromWeighted opt mv
                   else
                     return p

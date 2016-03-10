{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Backtest
       (
       ) where

import           Backtest.Dates         (rebalanceDays, tradingDays)
import           Backtest.Optimize      (optimize)
import           Backtest.Portfolio     (fromWeighted, marketValue)
import           Backtest.Types         (Backtest, Constraints, HasAsset,
                                         HasBacktestConfig, HasDbConfig,
                                         Portfolio, Strategy, frequency,
                                         startValue)
import           Control.Lens           (view)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Control.Monad.Trans    (lift)
import qualified Data.Set               as S
import           Data.Time              (Day)
import           Pipes                  (Pipe, Producer, await, yield)


-- Needs
-- - Start Day
-- - Start Value
-- - Optimization Parameters
--   - Rebalance Frequency
--   - High/Low Cutoff
--   - Cash Buffer

backtest'
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     , HasBacktestConfig r
     , HasAsset a
     )
     => Strategy m a
     -> Constraints a
     ->  m b
backtest' strat cs =  do
  (d:ds) <- tradingDays
  freq   <- view frequency
  let rds = rebalanceDays freq ds
  op <- optimize strat cs d
  mv <- view startValue
  let p = fromWeighted op mv
  return undefined



backtest
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     , HasBacktestConfig r
     , HasAsset a
     )
     => Pipe (Strategy m a, Constraints a, [Day], S.Set Day,  Day, Portfolio) (Day, Portfolio) m ()
backtest = do
  (strat, cts, d:ds, rebalDays,  d', p') <- await
  p'' <- lift $ rebalance strat cts d rebalDays p'
  yield (d, p'')

rebalance :: (MonadIO m, MonadReader r m, HasAsset a, HasBacktestConfig r, HasDbConfig r)
  =>  Strategy m a -> Constraints a -> Day -> S.Set Day -> Portfolio
  -> m Portfolio
rebalance strat cts d ds p = if S.member d ds
                             then do
                               opt <- optimize strat cts d
                               let mv = marketValue p
                               return $ fromWeighted opt mv
                             else
                               return p

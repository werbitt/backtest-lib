{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Backtest
       (
         backtest
       ) where

import           Backtest.Dates      (rebalanceDays, tradingDays)
import           Backtest.Optimize   (optimize)
import           Backtest.Portfolio  (empty, flow, fromWeighted,
                                      getAndApplyReturns, marketValue, toList)
import           Backtest.Query      (BacktestId, saveBacktestMeta,
                                      saveConstraints, saveHoldings)
import           Backtest.Types      (Backtest, CanDb, Constraints, HasAsset,
                                      HasBacktestConfig, Portfolio, Strategy,
                                      frequency, startValue, startValue)
import           Control.Lens        (view)
import           Control.Monad       (forever, void)
import           Control.Monad.Trans (lift)
import qualified Data.Set            as S
import           Data.Time           (Day)
import           Pipes               (Consumer, Producer, await, runEffect,
                                      yield, (>->))


-- | `backtest` takes a strategy and constraints and returns a Backtest.
-- In this case a backtest is basically a Portfolio over a series of days.
--
-- The backtest configuration is kept in the reader portion of the Backtest.
-- I wonder if it should be made explicit.

backtest
  :: (CanDb r Backtest, HasAsset a, HasBacktestConfig r) =>
     Strategy Backtest a -> Constraints a -> Backtest ()
backtest strat cts  = do
  bId <- saveBacktestMeta
  void (saveConstraints bId cts)
  runEffect $ start strat cts >-> save bId

start
  :: ( CanDb r m
     , HasBacktestConfig r
     , HasAsset a
     )
     => Strategy m a
     -> Constraints a
     -> Producer (Day, Portfolio) m ()
start strat cts =  do
  (d:ds) <- tradingDays
  freq   <- view frequency
  sv <- view startValue
  p <- lift $ rebalance strat cts d (flow empty sv)
  let rds = rebalanceDays freq ds
  yield (d, p)
  step strat cts rds d p ds

step
  :: ( CanDb r m, HasBacktestConfig r, HasAsset a )
     => Strategy m a
     -> Constraints a
     -> S.Set Day
     -> Day
     -> Portfolio
     -> [Day]
     -> Producer (Day, Portfolio) m ()
step strat cts rds d p (d':ds)  = do
  p' <- lift $ if S.member d rds
               then rebalance strat cts d' p
               else pure p
  p'' <- lift $ applyReturns d d' p'
  yield (d', p'')
  step strat cts rds d' p'' ds
step _ _ _ _ _ [] = pure ()

rebalance
  :: ( CanDb r m,  HasAsset a, HasBacktestConfig r)
     =>  Strategy m a
     -> Constraints a
     -> Day
     -> Portfolio
     -> m Portfolio
rebalance strat cts d p = do
  let mv = marketValue p
  opt <- optimize strat cts mv d
  return $ fromWeighted opt mv

applyReturns
  :: CanDb r m => Day -> Day -> Portfolio -> m Portfolio
applyReturns = getAndApplyReturns

save :: CanDb r m => BacktestId -> Consumer (Day, Portfolio) m ()
save bId  = forever $ do
  (d, p) <- await
  void $ saveHoldings bId d (toList p)

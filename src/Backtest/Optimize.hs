{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Optimize
       (
         optimize
       ) where

import           Backtest.Constraint      (globalFilters, longFilters,
                                           runFilters, shortFilters)
import           Backtest.Optimize.Weight (mkWeights)
import           Backtest.Types           (CanDb, Constraints, HasAsset,
                                           HasBacktestConfig, PortfolioW,
                                           Strategy, asset, backtestConfig,
                                           buffer, cutoff, getData, mkCash,
                                           mkPortfolio, rank)
import           Control.Lens             (view, (^.))
import           Data.Bifunctor           (bimap, first)
import           Data.Time                (Day)

optimize :: (CanDb r m, HasAsset a,  HasBacktestConfig r)
         => Strategy m a
         -> Constraints a
         -> Double
         -> Day
         -> m PortfolioW
optimize strat cts mv d = do
  allData <- (strat^.getData) d
  let filtered = runFilters (globalFilters cts) allData
  let ranked = strat^.rank $ filtered
  let longs = runFilters (longFilters cts) ranked
  let shorts = runFilters (shortFilters cts) (reverse ranked)
  totWgt <- view (backtestConfig . buffer) >>= \b -> return $ 1 - b
  n <- view (backtestConfig.cutoff) >>= \c -> return $ floor $ c * realToFrac (length ranked)
  let lngWgts = map (first asset) $ mkWeights cts mv n totWgt longs
  let shtWgts = map (bimap asset negate) $ mkWeights cts mv n totWgt shorts
  let cshWgt = 1 - (sum (map snd shtWgts) +  sum (map snd lngWgts))
  return $ mkPortfolio $ (mkCash, cshWgt) : lngWgts ++ shtWgts

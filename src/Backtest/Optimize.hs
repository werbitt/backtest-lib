{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Optimize
       (
         optimize
       ) where

import           Backtest.Optimize.Weight (mkWeights)
import           Backtest.Types           (CanDb, Constrain (..), Constraints,
                                           FullConstraint, HasAsset,
                                           HasBacktestConfig, PortfolioW,
                                           Strategy, asset, backtestConfig,
                                           buffer, cutoff, getData, global,
                                           long, mkPortfolio, rank, short,
                                           volume)
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
  let filtered = runFullConstraints (cts^.global) allData
  let ranked = strat^.rank $ filtered
  let longs = runFullConstraints (cts^.long) ranked
  let shorts = runFullConstraints (cts^.short) (reverse ranked)
  totWgt <- view (backtestConfig . buffer) >>= \b -> return $ 1 - b
  n <- view (backtestConfig.cutoff) >>= \c -> return $ floor $ c * realToFrac (length ranked)
  let lngWgts = map (first asset) $ mkWeights (cts^.volume) mv n totWgt longs
  let shtWgts = map (bimap asset negate) $ mkWeights (cts^.volume) mv n totWgt shorts
  return $ mkPortfolio $ lngWgts ++ shtWgts


runFullConstraints :: [FullConstraint a] -> [a] -> [a]
runFullConstraints _           []     = []
runFullConstraints constraints (x:xs) = if all (== Include) (constraints <*> pure x)
                                        then x : runFullConstraints constraints xs
                                        else runFullConstraints constraints xs

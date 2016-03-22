{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Optimize
       (
         optimize
       ) where

import           Backtest.Types       (Asset, Buffer, CanDb, Constrain (..),
                                       Constraint, Constraints, HasAsset,
                                       HasBacktestConfig, PortfolioW, Strategy,
                                       Weight, backtestConfig, buffer, cutoff,
                                       getAsset, getData, global, long, mkCash,
                                       mkPortfolio, rank, short)
import           Control.Lens         (view, (^.))
import           Control.Monad.Reader (MonadReader)
import           Data.Time            (Day)


optimize :: (CanDb r m, HasAsset a, HasBacktestConfig r) =>
            Strategy m a -> Constraints a -> Day -> m PortfolioW
optimize strategy constraints d = do
  allData <- (strategy ^. getData) d
  let filtered = runConstraints (constraints ^. global) allData
  let ranked = rankBy strategy filtered
  longs <- takeCut $ runConstraints (constraints ^. long) ranked
  shorts <- takeCut . reverse $ runConstraints (constraints ^. short) ranked
  buf <- view (backtestConfig . buffer)
  let weights = mkWeights buf longs shorts
  return (mkPortfolio weights)

runConstraints :: [Constraint a] -> [a] -> [a]
runConstraints _           []     = []
runConstraints constraints (x:xs) = if all (== Include) (constraints <*> pure x)
                                    then x : runConstraints constraints xs
                                    else runConstraints constraints xs

rankBy :: Strategy m a -> [a] -> [a]
rankBy strategy = strategy ^. rank

takeCut :: (MonadReader r  m, HasBacktestConfig r) => [a] -> m [a]
takeCut xs = do
  cut <- view (backtestConfig . cutoff)
  let n = floor $ cut * fromIntegral (length xs)
  return $ take n xs

mkWeights :: (HasAsset a) => Buffer -> [a] -> [a] -> [(Asset, Weight)]
mkWeights b longs shorts = (mkCash, 1) : longs' ++ shorts'
  where
    lWgt = equalWeight b (length longs)
    longs' = map (\x -> (getAsset x, lWgt)) longs
    sWgt = negate $ equalWeight b (length shorts)
    shorts' = map (\x -> (getAsset x, sWgt)) shorts

equalWeight :: Fractional a => Buffer -> Int -> a
equalWeight b n = realToFrac $ (1 - b) / fromIntegral n

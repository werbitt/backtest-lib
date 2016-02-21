{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rd.Optimize
       (
         optimize
       ) where

import           Control.Lens           (view, (^.))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader)
import           Data.Time              (Day)
import           Rd.Types               (Asset, Buffer, Constraint, Constraints,
                                         Env, HasAsset, PortfolioW, Strategy,
                                         Weight, buffer, cutoff, getAsset,
                                         global, long, mkCash, mkPortfolio,
                                         params, rank, short)


optimize :: (MonadIO m, MonadReader Env m, HasAsset a) =>
            (Day -> m [a]) -> Strategy a -> Constraints a -> Day -> m PortfolioW
optimize query strategy constraints d = do
  allData <- query d
  let filtered = runConstraints (constraints ^. global) allData
  let ranked = rankBy strategy filtered
  longs <- takeCut $ runConstraints (constraints ^. long) ranked
  shorts <- takeCut . reverse $ runConstraints (constraints ^. short) ranked
  buf <- view (params . buffer)
  let weights = mkWeights buf longs shorts
  return (mkPortfolio weights)

runConstraints :: [Constraint a] -> [a] -> [a]
runConstraints _           []     = []
runConstraints constraints (x:xs) = if and (constraints <*> pure x)
                                    then x : runConstraints constraints xs
                                    else runConstraints constraints xs

rankBy :: Strategy a -> [a] -> [a]
rankBy strategy = strategy ^. rank

takeCut :: (MonadReader Env  m) => [a] -> m [a]
takeCut xs = do
  cut <- view (params . cutoff)
  let n = floor $ cut * fromIntegral (length xs)
  return $ take n xs

mkWeights :: (HasAsset a) => Buffer -> [a] -> [a] -> [(Asset, Weight)]
mkWeights b longs shorts = (mkCash, 1) : longs' ++ shorts'
  where
    lWgt = equalWeight b (length longs)
    longs' = map (\x -> (getAsset x, lWgt)) longs
    sWgt = equalWeight b (length shorts)
    shorts' = map (\x -> (getAsset x, sWgt)) shorts

equalWeight :: Fractional a => Buffer -> Int -> a
equalWeight b n = realToFrac $ (1 - b) / fromIntegral n

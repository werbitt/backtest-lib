module Backtest.Constraint
       (
         mkVolumeConstraint
       , mkVolumeConstraint'
       , HasVolume (..)
       ) where

import           Backtest.Types (WeightConstraint, WeightConstraint')

-- | Volume should be in notional terms
class HasVolume a where
  volume :: a -> Double

mkVolumeConstraint:: HasVolume a =>
                     Double                 -- Max % of Volume
                -- -> Double                 -- Marketalue
                  -> WeightConstraint a
mkVolumeConstraint pov (mv, x) = (pov * volume x) / mv


mkVolumeConstraint' :: HasVolume a => Double -> WeightConstraint' a
mkVolumeConstraint' pov mv x = (pov * volume x) / mv

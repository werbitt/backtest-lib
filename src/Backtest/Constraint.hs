module Backtest.Constraint
       ( mkVolumeConstraint
       , HasVolume (..)
       ) where

import           Backtest.Types (NotionalConstraint)

-- | Volume should be in notional terms
class HasVolume a where
  volume :: a -> Double

mkVolumeConstraint :: HasVolume a => Double -> NotionalConstraint a
mkVolumeConstraint pov =  (pov *) . volume

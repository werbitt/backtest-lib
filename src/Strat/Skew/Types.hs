{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}

module Strat.Skew.Types
       (
         ImpliedVol
       , Volume
       , Skew
       , SkewData (..)
       , skewDataAsset
       , skewDataSkew
       )where

import           Backtest.Constraint (HasVolume (..))
import           Backtest.Types      (Asset, HasAsset (..))
import           Control.Lens        (makeLenses, (^.))

type ImpliedVol = Double
type Skew = Double
type Volume = Double
data SkewData = SkewData { _skewDataAsset  :: Asset
                         , _skewDataSkew   :: Skew
                         , _skewDataVolume :: Volume }

makeLenses ''SkewData

instance HasAsset SkewData where
  asset = (^.skewDataAsset)

instance HasVolume SkewData where
  volume = (^.skewDataVolume)

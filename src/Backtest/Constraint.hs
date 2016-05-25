{-# LANGUAGE OverloadedStrings #-}

module Backtest.Constraint
       ( addGlobalFilter
       , addLongFilter
       , addShortFilter
       , addWeightConstraint
       , addValueConstraint
       , globalFilters
       , longFilters
       , shortFilters
       , weightConstraints
       , valueConstraints
       , runFilters
       , HasVolume (..)
       , addVolumeConstraint
       ) where

import           Backtest.Types (Constraint (..), ConstraintHook (..),
                                 Constraints, Filter (..), Value, Weight)
import           Data.Monoid    ((<>))
import           Data.Text      (Text, pack)


addGlobalFilter :: (a -> Filter) -> Text -> Constraints a -> Constraints a
addGlobalFilter f d = (:) (GlobalFilters, FilterConstraint f, d)

addLongFilter :: (a -> Filter) -> Text -> Constraints a -> Constraints a
addLongFilter f d = (:) (LongFilters, FilterConstraint f, d)

addShortFilter :: (a -> Filter) -> Text -> Constraints a -> Constraints a
addShortFilter f d = (:) (ShortFilters, FilterConstraint f, d)

addWeightConstraint :: (a -> Weight) -> Text -> Constraints a -> Constraints a
addWeightConstraint f d = (:) (WeightConstraints, WeightConstraint f, d)

addValueConstraint :: (a -> Value) -> Text -> Constraints a -> Constraints a
addValueConstraint f d = (:) (ValueConstraints, ValueConstraint f, d)


globalFilters :: Constraints a -> [a -> Filter]
globalFilters []                                         = []
globalFilters ((GlobalFilters, FilterConstraint f, _):cs) = f : globalFilters cs
globalFilters (_:cs)                                     = globalFilters cs

longFilters :: Constraints a -> [a -> Filter]
longFilters [] = []
longFilters ((LongFilters, FilterConstraint f, _):cs) = f : longFilters cs
longFilters (_:cs) = longFilters cs

shortFilters :: Constraints a -> [a -> Filter]
shortFilters [] = []
shortFilters ((LongFilters, FilterConstraint f, _):cs) = f : shortFilters cs
shortFilters (_:cs) = shortFilters cs

weightConstraints :: Constraints a -> [a -> Weight]
weightConstraints [] = []
weightConstraints ((WeightConstraints, WeightConstraint f, _):cs)
  = f : weightConstraints cs
weightConstraints (_:cs) = weightConstraints cs

valueConstraints :: Constraints a -> [a -> Value]
valueConstraints [] = []
valueConstraints ((ValueConstraints, ValueConstraint f, _):cs)
  = f : valueConstraints cs
valueConstraints (_:cs) = valueConstraints cs


runFilters :: [a -> Filter] -> [a] -> [a]
runFilters _           []     = []
runFilters fs (x:xs) = if all (== Include) (fs <*> pure x)
                       then x : runFilters fs xs
                       else runFilters fs xs

-- | Volume should be in notional terms
class HasVolume a where
  volume :: a -> Double

addVolumeConstraint :: HasVolume a => Double -> Constraints a -> Constraints a
addVolumeConstraint pov = addValueConstraint f d
  where
    f = (pov *) . volume
    d = "Volume constrained to max % of ADV: " <> (pack . show) pov

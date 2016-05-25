{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Backtest.Optimize.Weight
       (
         mkWeights
       ) where

import           Backtest.Constraint (valueConstraints, weightConstraints)
import           Backtest.Types      (Constraints, Weight)
import           Control.Lens        (makeLenses, (&), (+~), (.~), (^.), _1)
import           Data.List           (sortBy, sortOn)
import           Data.Maybe          (catMaybes)

data WithWeight a = WithWeight { _wwAsset     :: a
                               , _wwWeight    :: Weight
                               , _wwMaxWeight :: Maybe Weight
                               }

makeLenses ''WithWeight

mkWeights
  :: Constraints a
  -> Double                    -- market value
  -> Int                       -- target number of names
  -> Weight                    -- total target weight
  -> [a]
  -> [(a, Weight)]
mkWeights cts mv n totWgt xs
  = let tgtWgt = totWgt / fromIntegral n
        wws = map (applyValueConstraints mv cts .
                   applyWeightConstraints cts .
                   targetWeight tgtWgt) xs
    in map (\x -> (x^.wwAsset, x^.wwWeight)) $ takeWeight totWgt wws

targetWeight :: Weight -> a -> WithWeight a
targetWeight w a = WithWeight a w Nothing

applyWeightConstraints :: Constraints a -> WithWeight a -> WithWeight a
applyWeightConstraints cts x =
  let wcs = weightConstraints cts
      maxWgt' = minimum' $ wcs <*> [x^.wwAsset]
      maxWgt = minimum' . catMaybes $ [maxWgt', x^.wwMaxWeight]
      wgt = maybe (x^.wwWeight) (min (x^.wwWeight)) maxWgt
  in x & wwWeight .~ wgt & wwMaxWeight .~ maxWgt


applyValueConstraints
  ::  Double -> Constraints a -> WithWeight a -> WithWeight a
applyValueConstraints mv cts x =
  let vcs = valueConstraints cts
      maxNotional = minimum' $ vcs <*> [x^.wwAsset]
      maxWgt = minimum' . catMaybes $ [(/ mv) <$> maxNotional, x^.wwMaxWeight]
      wgt = maybe (x^.wwWeight) (min (x^.wwWeight)) maxWgt
  in x & wwWeight .~ wgt & wwMaxWeight .~ maxWgt


-- | takeWeight takes a total target weight and list of weighted assets and
-- takes from the head of the list until we reach the target weight. The
-- weight of the last asset gets truncated.
takeWeight :: Weight -> [WithWeight a] -> [WithWeight a]
takeWeight _ [] = []
takeWeight 0 _  = []
takeWeight w (x:xs) = let w' = x^.wwWeight
                      in if w' < w
                         then x : takeWeight (w - w') xs
                         else [x & wwWeight .~ w]


-- | SpreadWeight takes an amount of weight to spread across a list of assets
-- If the amount of weight to spread exceeds the available weight then
-- return Left otherwise return Right.
spreadWeight :: Weight -> [WithWeight a] -> Either [WithWeight a] [WithWeight a]
spreadWeight w xs = if wgtLeft' > 0
                    then Left $ map fst (sortOn snd result')
                    else Right $ map fst (sortOn snd result')
  where
    (_, wgtLeft', _, result') = foldr go (length' - 1, w, w/ length', []) xs'

    go x (nLeft, wgtLeft, incWgt, result) =
      case capped x incWgt of
        Just (maxWgt, room) -> ( nLeft - 1
                               , wgtLeft - room
                               , incWgt + ((incWgt - room) / nLeft)
                               , (x & (_1.wwWeight) .~ maxWgt) : result )
        Nothing -> ( nLeft - 1
                   , wgtLeft - incWgt
                   , incWgt
                   , (x & (_1.wwWeight) +~ incWgt) : result)


    capped (x,_) incWgt = case x^.wwMaxWeight of
      Nothing     -> Nothing
      Just maxWgt -> let room = maxWgt - x^.wwWeight
                     in if room < incWgt
                        then Just (maxWgt, room)
                        else Nothing


    xs' = sortBy (\(x,_) (y,_) -> compareByRoom x y)(zip xs ([1..] :: [Int]))
    length' = fromIntegral $ length xs

minimum' :: Ord a => [a] -> Maybe a
minimum' [] = Nothing
minimum' xs = Just (minimum xs)

compareByRoom :: WithWeight a -> WithWeight a -> Ordering
compareByRoom x@(WithWeight _ w mw) y@(WithWeight _ w' mw') =
  let room z =  z^.wwMaxWeight >>= pure . (+ (-z^.wwWeight))
      rx = room x
      ry = room y
      compare' Nothing   _       = GT
      compare' _         Nothing = LT
      compare' (Just m) (Just n) = compare m n
  in compare' rx ry

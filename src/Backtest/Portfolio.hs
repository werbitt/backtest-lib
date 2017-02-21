{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Portfolio
       (
         fromWeighted
       , marketValue
       , flow
       , empty
       , mapWithAsset
       , getAndApplyReturns
       , toList
       ) where

import           Backtest.Db.Ids (SecurityId)
import           Backtest.Query  (runReturnQuery)
import           Backtest.Types  (Asset (..), CanDb, Portfolio, PortfolioF (..),
                                  PortfolioW, Value, mkCash, mkPortfolio)
import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe, mapMaybe)
import           Data.Monoid     (Sum (..), getSum, (<>))
import           Data.Time       (Day)

fromWeighted :: PortfolioW -> Value -> Portfolio
fromWeighted p mv = toValue <$> p
  where
    toValue w = w * mv

marketValue :: Portfolio -> Value
marketValue p = getSum $ foldMap Sum p

flow :: Portfolio -> Value -> Portfolio
flow p v = p <> mkPortfolio [(mkCash, v)]

empty :: (Num a, Ord a) => PortfolioF a
empty = mempty

mapWithAsset :: (Asset -> a -> b) -> PortfolioF a -> PortfolioF b
mapWithAsset f (PortfolioF m) = PortfolioF $ M.mapWithKey f m

assets :: PortfolioF a -> [Asset]
assets (PortfolioF m) = M.keys m

toList :: PortfolioF a -> [(Asset, a)]
toList (PortfolioF m) = M.toList m

getSecurityId :: Asset -> Maybe SecurityId
getSecurityId (Equity sid) = Just sid
getSecurityId _ = Nothing

getSecurityIds :: Portfolio -> [SecurityId]
getSecurityIds = mapMaybe getSecurityId . assets

getReturns :: CanDb r m => Day -> Day -> Portfolio -> m (M.Map SecurityId Double)
getReturns sd ed p = runReturnQuery sd ed (getSecurityIds p)

applyReturns :: M.Map SecurityId Double -> Portfolio -> Portfolio
applyReturns returns = mapWithAsset applyReturn
  where
    applyReturn k v = v * return' k
    return' k = 1 + fromMaybe 0 (getSecurityId k >>= \sid -> M.lookup sid returns)

getAndApplyReturns :: CanDb r m => Day -> Day -> Portfolio -> m Portfolio
getAndApplyReturns sd ed p = do
  rs <- getReturns sd ed p
  return $ applyReturns rs p

{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Portfolio
       (
         fromWeighted
       , marketValue
       , flow
       , empty
       , mapWithAsset
       , getEquityTickers
       , getAndApplyReturns
       , toList
       ) where

import           Backtest.Query      (runReturnQuery)
import           Backtest.Types      (Asset, CanDb, Portfolio, PortfolioF (..),
                                      PortfolioW, Ticker, Value, connection,
                                      getTicker, historyVersion, mkCash,
                                      mkPortfolio)
import           Control.Lens        (view)
import           Control.Monad.Trans (liftIO)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Monoid         (Sum (..), getSum, (<>))
import           Data.Time           (Day)

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

getEquityTickers :: Portfolio -> [Ticker]
getEquityTickers = mapMaybe getTicker . assets

getReturns :: CanDb r m => Day -> Day -> Portfolio -> m (M.Map Asset Double)
getReturns sd ed p = do
  c <- view connection
  v <- view historyVersion
  let ts = getEquityTickers p
  liftIO $ runReturnQuery c v sd ed ts

applyReturns :: M.Map Asset Double -> Portfolio -> Portfolio
applyReturns returns = mapWithAsset applyReturn
  where
    applyReturn k v = v * return' k
    return' k = 1 + fromMaybe 0 (M.lookup k returns)

getAndApplyReturns :: CanDb r m => Day -> Day -> Portfolio -> m Portfolio
getAndApplyReturns sd ed p = do
  rs <- getReturns sd ed p
  return $ applyReturns rs p

module Backtest.Portfolio
       (
         fromWeighted
       , marketValue
       ) where

import           Backtest.Types (Portfolio, PortfolioW, Value)
import           Data.Monoid    (Sum (..), getSum)

fromWeighted :: PortfolioW -> Value -> Portfolio
fromWeighted p mv = toValue <$> p
  where
    toValue w = w * mv

marketValue :: Portfolio -> Value
marketValue p = getSum $ foldMap Sum p

module Rd.Portfolio
       (
         fromWeighted
       , marketValue
       ) where

import           Data.Monoid (Sum (..), getSum)
import           Rd.Types    (Portfolio, PortfolioW, Value)

fromWeighted :: PortfolioW -> Value -> Portfolio
fromWeighted p mv = toValue <$> p
  where
    toValue w = w * mv

marketValue :: Portfolio -> Value
marketValue p = getSum $ foldMap Sum p

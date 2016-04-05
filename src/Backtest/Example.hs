{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Example
       (run') where

import qualified Backtest.Backtest    as B
import           Backtest.Optimize    (optimize)
import qualified Backtest.Query       as Q
import           Backtest.Types       (AppConfig (..), Asset, Backtest,
                                       BacktestConfig (..), CanDb,
                                       Constrain (..), Constraints (..),
                                       DbConfig (..), FullConstraint, HasAsset,
                                       HasBacktestConfig, Ordinal (..),
                                       PortfolioW, Strategy (..), Weekday (..),
                                       asset, connection, getTicker,
                                       historyVersion, mkConstraints, mkEquity,
                                       mkFrequency, unBacktest, unTicker)
import           Control.Lens         (view)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans  (liftIO)
import           Data.Char            (toLower)
import           Data.List            (sortBy)
import           Data.Time            (Day, fromGregorian)

alpha :: CanDb r m => Strategy m Asset
alpha = Strategy query $ sortBy (\_ _ -> EQ)

query :: CanDb r m => Day -> m [Asset]
query d' = do
  conn <- view connection
  v <- view historyVersion
  ts <- liftIO $ Q.runMembersQuery conn v d'
  return $ map mkEquity ts


constraintNoSecondB :: HasAsset a => FullConstraint a
constraintNoSecondB x = check $ asset x
  where
    check a = maybe Include (hasB . map toLower . unTicker) (getTicker a)
    hasB (_:'b':_) = Exclude
    hasB _ = Include

constraintNoZ :: HasAsset a => FullConstraint a
constraintNoZ x = check $ asset x
  where
    check a = maybe Include (hasB . map toLower . unTicker) (getTicker a)
    hasB ('z':_) = Exclude
    hasB _ = Include

constraints :: (HasAsset a) => Constraints a
constraints = mkConstraints [constraintNoSecondB][constraintNoZ][] []

d :: Day
d = fromGregorian 2016 1 26

run' :: Backtest a -> IO a
run' m = do
  conn <- Q.connection
  version <- Q.lastHistoryVersion conn
  let config =
        AppConfig { appDbConfig = DbConfig { _connection = conn
                                           , _historyVersion = version }
                  , appBacktestConfig = BacktestConfig { _startDate = fromGregorian 2006 1 1
                                                       , _startValue = 2000000
                                                       , _frequency = mkFrequency Third Friday 2
                                                       , _cutoff = 0.05
                                                       , _buffer = 0 }}

  runStdoutLoggingT $ runReaderT (unBacktest m) config

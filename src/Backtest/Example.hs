{-# LANGUAGE FlexibleContexts #-}

module Backtest.Example where

import           Control.Lens           (view)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, runReaderT)
import           Control.Monad.Trans    (liftIO)
import           Data.Char              (toLower)
import           Data.List              (sortBy)
import           Data.Time              (Day, fromGregorian)
import           Backtest.Optimize            (optimize)
import qualified Backtest.Query               as Q
import           Backtest.Types               (Asset, Backtest, Constrain (..),
                                         Constraints, HasAsset,
                                         Ordinal (..), Params, PortfolioW,
                                         Strategy, Wait, Weekday (..),
                                         connection, getAsset, getTicker,
                                         historyVersion, mkConstraints,
                                         mkEquity, mkParams, mkStrategy,
                                         unBacktest, unTicker, mkFrequency)

dbConfig = DbConfig { _connection = Q.connection }
backtestConfig = BacktestConfig { _startDate = fromGregorian 2006 1 1
                                , _startValue = 1000000
                                , _frequency = mkFrequency Second Friday 2
                                , _cutoff = 0.05
                                , _buffer = 0.10 }
strategyConfig = StrategyConfig { _strategy = alpha
                                , _constraints = mkConstraints
                                  [constraintNoSecondB]
                                  [constraintNoZ]
                                  [] }



alpha :: (MonadIO m, MonadReader Env m) => Strategy m Asset
alpha = mkStrategy query $ sortBy (\_ _ -> EQ)

query :: (MonadIO m,  MonadReader Env m) => Day -> m [Asset]
query d' = do
  conn <- view connection
  v <- view historyVersion
  ts <- liftIO $ Q.runMembersQuery conn v d'
  return $ map mkEquity ts


constraintNoSecondB :: HasAsset a => a -> Constrain
constraintNoSecondB x = check $ getAsset x
  where
    check a = maybe Include (hasB . map toLower . unTicker) (getTicker a)
    hasB (_:'b':_) = Exclude
    hasB _ = Include

constraintNoZ :: HasAsset a => a -> Constrain
constraintNoZ x = check $ getAsset x
  where
    check a = maybe Include (hasB . map toLower . unTicker) (getTicker a)
    hasB ('z':_) = Exclude
    hasB _ = Include




constraints :: (HasAsset a) => Constraints a
constraints =

d :: Day
d = fromGregorian 2016 1 26

optimize' :: (MonadIO m, MonadReader Env m) => m PortfolioW
optimize' = optimize alpha constraints d

run :: Backtest a -> IO a
run m = do
  let sd = fromGregorian 2006 1 1
  let f = mkFrequency Second Friday 2
  let p = mkParams sd 1000000 f 0.05 0.1
  conn <- Q.connection
  version <- Q.lastHistoryVersion conn
  runReaderT (unBacktest m) (mkEnv conn version p)

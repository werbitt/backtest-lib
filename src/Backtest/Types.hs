{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Backtest.Types
       ( Backtest
       , unBacktest
       , AppConfig (..)
       , BacktestConfig (..)
       , backtestConfig
       , HasBacktestConfig
       , dbConfig
       , HasDbConfig
         -- * Reader Environment
--       , Env
--       , mkEnv
       , connection
       , historyVersion
--       , params
         -- * Optimization Parameters
--       , Params
--       , mkParams
       , startDate
       , startValue
       , frequency
       , cutoff
       , buffer
       , Buffer
         -- * Assets
       , Asset
       , mkCash
       , mkEquity
       , unTicker
       , Ticker
       , mkTicker
       , getTicker
       , Value
       , Price
       , Return
       , HasAsset
       , getAsset
         -- * Portfolios
       , Portfolio
       , mkPortfolio
       , PortfolioW
       , Weight
         -- * Strategy
       , Strategy
       , getData
       , rank
         -- * Constraints
       , Constraints
       , global
       , long
       , short
       , Constraint
       , mkConstraints
       , Constrain (..)
         -- * Rebalance Frequency
       , Frequency
       , mkFrequency
       , ordinal
       , weekday
       , wait
       , Ordinal(..)
       , Weekday(..)
       , Wait
       , ordToInt
       , weekdayToInt) where

import           Control.Lens.TH            (makeClassy, makeLenses)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Time                  (Day)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye.PGTypes            (PGText)
import           Opaleye.RunQuery           (QueryRunnerColumnDefault)


------------------------------------------------------------------------
-- | Dates
------------------------------------------------------------------------
data Ordinal = First
             | Second
             | Third
             | Fourth deriving (Show)

type Wait = Int


ordToInt :: Num a => Ordinal -> a
ordToInt First  = 1
ordToInt Second = 2
ordToInt Third  = 3
ordToInt Fourth = 4

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday deriving (Show)

weekdayToInt :: Num a => Weekday -> a
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

data Frequency = Monthly { _ordinal :: Ordinal
                         , _weekday :: Weekday
                         , _wait    :: Wait } deriving (Show)
makeLenses ''Frequency

mkFrequency :: Ordinal -> Weekday -> Wait -> Frequency
mkFrequency = Monthly


------------------------------------------------------------------------
-- | Assets
------------------------------------------------------------------------
newtype Ticker = Ticker String deriving (Show, Eq, Ord)
deriving instance QueryRunnerColumnDefault PGText Ticker

mkTicker :: String -> Ticker
mkTicker = Ticker

unTicker :: Ticker -> String
unTicker (Ticker s) = s

data Asset = Cash | Equity Ticker deriving (Show, Eq, Ord)

mkCash :: Asset
mkCash = Cash

mkEquity :: Ticker -> Asset
mkEquity = Equity

getTicker :: Asset -> Maybe Ticker
getTicker (Equity t) = Just t
getTicker _          = Nothing


type Price = Double
type Return = Double

class HasAsset a where
  getAsset :: a -> Asset

instance HasAsset Asset where
  getAsset = id


------------------------------------------------------------------------
-- | Portfolios
------------------------------------------------------------------------
type Value = Double
type Weight = Double

data PortfolioF a = PortfolioF (Map Asset a) deriving (Functor, Foldable, Show)
type Portfolio = PortfolioF Value
type PortfolioW = PortfolioF Weight

mkPortfolio :: (Num a, Eq a) => [(Asset, a)] -> PortfolioF a
mkPortfolio = PortfolioF . M.fromList


------------------------------------------------------------------------
-- | Stragegies
------------------------------------------------------------------------
data Strategy m a = Strategy
                  { _getData :: Day -> m [a]
                  , _rank    :: [a] -> [a]  }

makeLenses ''Strategy


------------------------------------------------------------------------
-- | Constraints
------------------------------------------------------------------------
data Constrain = Include | Exclude deriving (Eq)
type Constraint a = a -> Constrain

data Constraints a = Constraints { _global :: [Constraint a]
                                 , _short  :: [Constraint a]
                                 , _long   :: [Constraint a] }

makeLenses ''Constraints

mkConstraints :: [Constraint a] -> [Constraint a] -> [Constraint a] -> Constraints a
mkConstraints = Constraints


------------------------------------------------------------------------
-- | Config
------------------------------------------------------------------------
data DbConfig = DbConfig { _connection     :: Connection
                         , _historyVersion :: Int }
makeClassy ''DbConfig


type Cutoff = Double
type Buffer = Double
data BacktestConfig = BacktestConfig { _startDate  :: Day
                                     , _startValue :: Value
                                     , _frequency  :: Frequency
                                     , _cutoff     :: Cutoff
                                     , _buffer     :: Buffer }

makeClassy ''BacktestConfig

data StrategyConfig m a = StrategyConfig { _strategy    :: Strategy m a
                                         , _constraints :: Constraints a }


data AppConfig  = AppConfig { appDbConfig       :: DbConfig
                            , appBacktestConfig :: BacktestConfig }
makeClassy ''AppConfig




------------------------------------------------------------------------
-- | Application Monad Stack
------------------------------------------------------------------------
newtype Backtest a = Backtest { unBacktest :: ReaderT AppConfig IO a } deriving
                     (
                       Functor
                     , Applicative
                     , Monad
                     , MonadReader AppConfig
                     , MonadIO
                     )

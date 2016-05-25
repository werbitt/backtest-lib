
{-# LANGUAGE ConstraintKinds            #-}
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
       , DbConfig (..)
       , dbConfig
       , HasDbConfig
       , CanDb
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
       , Ticker
       , GlobalId
       , mkTicker
       , unTicker
       , getTicker
       , Value
       , Price
       , Return
       , HasAsset
       , asset
         -- * Portfolios
       , Portfolio
       , mkPortfolio
       , PortfolioW
       , PortfolioF(..)
       , Weight
         -- * Strategy
       , Strategy (..)
       , getData
       , rank
         -- * Constraints
       , ConstraintHook(..)
       , Constraints
       , Constraint (..)
       , Filter(..)
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

import           Control.Lens                    (lens)
import           Control.Lens.TH                 (makeClassy, makeLenses)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Logger            (LoggingT)
import           Control.Monad.Reader            (MonadReader, ReaderT)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as M
import           Data.Profunctor.Product.Default (Default, def)
import           Data.Text                       (Text)
import           Data.Time                       (Day)
import           Database.PostgreSQL.Simple      (Connection)
import           Opaleye.Column                  (Column)
import           Opaleye.Constant                (Constant (..))
import           Opaleye.PGTypes                 (PGText, pgStrictText,
                                                  pgString)
import           Opaleye.RunQuery                (QueryRunnerColumnDefault)

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

newtype Ticker = Ticker {unTicker :: String } deriving (Show, Eq, Ord)
deriving instance QueryRunnerColumnDefault PGText Ticker

instance Default Constant Ticker (Column PGText) where
  def = Constant $ pgString . unTicker

mkTicker :: String -> Ticker
mkTicker = Ticker

newtype GlobalId = GlobalId { unGlobalId :: Text } deriving (Show, Eq, Ord)
deriving instance QueryRunnerColumnDefault PGText GlobalId

instance Default Constant GlobalId (Column PGText) where
  def = Constant $ pgStrictText . unGlobalId

data Asset = Cash | Equity Ticker deriving (Show, Eq, Ord)

instance Default Constant Asset (Column PGText) where
  def = Constant $ pgString . assetToString

mkCash :: Asset
mkCash = Cash

mkEquity :: Ticker -> Asset
mkEquity = Equity

getTicker :: Asset -> Maybe Ticker
getTicker (Equity t) = Just t
getTicker _          = Nothing

assetFromString :: String -> Asset
assetFromString "Cash" = Cash
assetFromString s      = mkEquity (mkTicker s)

assetToString :: Asset -> String
assetToString Cash       = "Cash"
assetToString (Equity t) = unTicker t


type Price = Double
type Return = Double

class HasAsset a where
  asset :: a -> Asset

instance HasAsset Asset where
  asset = id


------------------------------------------------------------------------
-- | Portfolios
------------------------------------------------------------------------
type Value = Double
type Weight = Double

data PortfolioF a = PortfolioF (Map Asset a) deriving (Functor, Foldable, Show)
type Portfolio = PortfolioF Value
type PortfolioW = PortfolioF Weight

instance (Num a, Eq a) => Monoid (PortfolioF a) where
  mempty  = mkPortfolio []
  mappend (PortfolioF m1) (PortfolioF m2) = PortfolioF $
    M.filter (/= 0) (M.unionWith (+) m1 m2)

mkPortfolio :: (Num a, Eq a) => [(Asset, a)] -> PortfolioF a
mkPortfolio = PortfolioF . M.fromListWith (+)


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


data Filter = Include | Exclude deriving (Eq, Show)

data ConstraintHook = GlobalFilters
                    | LongFilters
                    | ShortFilters
                    | WeightConstraints
                    | ValueConstraints deriving (Eq, Show)

data Constraint a = FilterConstraint (a -> Filter)
                  | WeightConstraint (a -> Weight)
                  | ValueConstraint (a -> Value)

type Constraints a = [(ConstraintHook, Constraint a, Text)]


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

data AppConfig  = AppConfig { appDbConfig       :: DbConfig
                            , appBacktestConfig :: BacktestConfig }
makeClassy ''AppConfig

instance HasDbConfig AppConfig where
  dbConfig = lens appDbConfig (\app db -> app { appDbConfig = db })

instance HasBacktestConfig AppConfig where
  backtestConfig = lens appBacktestConfig (\a b -> a { appBacktestConfig = b })

type CanDb r m = ( MonadReader r m, HasDbConfig r, MonadIO m )

------------------------------------------------------------------------
-- | Application Monad Stack
------------------------------------------------------------------------
newtype Backtest a = Backtest { unBacktest :: ReaderT AppConfig (LoggingT IO) a } deriving
                     (
                       Functor
                     , Applicative
                     , Monad
                     , MonadReader AppConfig
                     , MonadIO
                     )

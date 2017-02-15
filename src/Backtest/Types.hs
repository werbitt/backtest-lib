{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Backtest.Types
       ( Backtest
       , unBacktest
       -- * App Config
       , AppConfig (..)
       , BacktestConfig (..)
       , backtestConfig
       , HasBacktestConfig
       , DbConfig (..)
       , dbConfig
       , dbConnectInfo
       , HasDbConfig
        -- * App Env
       , Env (..)
       , DbEnv (..)
       , conn
       , historyVersion
       , CanDb
         -- * Optimization Parameters
       , description
       , startDate
       , startValue
       , frequency
       , cutoff
       , buffer
       , Buffer
       , Cutoff
         -- * Assets
       , Asset(..)
       , mkCash
       , mkEquity
       , getSecurityId
       , Ticker
       , GlobalId
       , mkTicker
       , unTicker
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

import           Backtest.Db.Ids                 (HistoryVersionId, SecurityId)
import           Control.Lens                    (lens)
import           Control.Lens.TH                 (makeClassy, makeLenses)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Logger            (LoggingT)
import           Control.Monad.Reader            (MonadReader, ReaderT)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as M
import           Data.Profunctor.Product.Default (Default, def)
import           Data.String                     (IsString (..))
import           Data.Text                       (Text)
import           Data.Time                       (Day)
import           Database.PostgreSQL.Simple      (ConnectInfo, Connection)
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

instance IsString Ordinal where
  fromString "First"  = First
  fromString "Second" = Second
  fromString "Third"  = Third
  fromString "Fourth" = Fourth
  fromString e        = error ("fromString: " ++ e ++ " is not a valid Ordinal")

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

instance IsString Weekday where
  fromString "Monday"    = Monday
  fromString "TuesDay"   = Tuesday
  fromString "Wednesday" = Wednesday
  fromString "Thursday"  = Thursday
  fromString "Friday"    = Friday
  fromString "Saturday"  = Saturday
  fromString "Sunday"    = Sunday
  fromString e           = error ("fromString: " ++ e ++ " is not a valid weekday")

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

data Asset = Cash | Equity SecurityId deriving (Show, Eq, Ord)

mkCash :: Asset
mkCash = Cash

mkEquity :: SecurityId -> Asset
mkEquity = Equity

getSecurityId :: Asset -> Maybe SecurityId
getSecurityId (Equity sid) = Just sid
getSecurityId _          = Nothing

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
-- | Environment
------------------------------------------------------------------------


-- | Database
------------------------------------------------------------------------
data DbConfig = DbConfig { _dbConnectInfo :: ConnectInfo }
makeClassy ''DbConfig

data DbEnv = DbEnv { _conn           :: Connection
                   , _historyVersion :: HistoryVersionId }
makeClassy ''DbEnv


-- | Backtest Settings
------------------------------------------------------------------------
type Cutoff = Double
type Buffer = Double
data BacktestConfig = BacktestConfig { _description :: Text
                                     , _startDate   :: Day
                                     , _startValue  :: Value
                                     , _frequency   :: Frequency
                                     , _cutoff      :: Cutoff
                                     , _buffer      :: Buffer } deriving Show

makeClassy ''BacktestConfig

-- | App Config
data AppConfig = AppConfig { _acBacktestConfig :: BacktestConfig
                           , _acDbConfig       :: DbConfig }

makeClassy ''AppConfig

instance HasDbConfig AppConfig where
  dbConfig = acDbConfig

instance HasBacktestConfig AppConfig where
  backtestConfig = acBacktestConfig


-- | App Environment
------------------------------------------------------------------------
data Env = Env { _envConfig :: AppConfig
               , _envDb     :: DbEnv }

makeLenses ''Env

instance HasDbConfig Env where
  dbConfig = envConfig . acDbConfig

instance HasBacktestConfig Env where
  backtestConfig = envConfig . acBacktestConfig


instance HasDbEnv Env where
  dbEnv = envDb

type CanDb r m = ( MonadReader r m, HasDbEnv r, MonadIO m )

------------------------------------------------------------------------
-- | Application Monad Stack
------------------------------------------------------------------------

newtype Backtest a
  = Backtest { unBacktest :: ReaderT Env (LoggingT IO) a } deriving
                     (
                       Functor
                     , Applicative
                     , Monad
                     , MonadReader Env
                     , MonadIO
                     )

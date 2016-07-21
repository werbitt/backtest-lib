{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Backtest.Config
       (
       ) where

import           Backtest.Types          (Buffer, Cutoff, Frequency,
                                          Ordinal (..), Value, Wait,
                                          Weekday (..), mkFrequency)
import           Control.Lens            (makeLenses)
import           Data.Configurator       as C
import           Data.Configurator.Types (Config, Worth (..))
import           Data.Function           (on)
import           Data.List               (foldl1')
import           Data.Maybe              (catMaybes, listToMaybe)
import           Data.Monoid             (Last (..), getLast, mappend, (<>))
import           Data.Text               (Text)
import           Data.Time               (Day, defaultTimeLocale, fromGregorian,
                                          parseTimeM)
import           System.FilePath         ((</>))

--------------------------------------------------------------------------------
-- | Data structure for holding all backtest configuration

data AppConfig = AppConfig { _AppDbConfig       :: DbConfig
                           , _AppBacktestConfig :: BacktestConfig }

--------------------------------------------------------------------------------
-- | Load configuration from a file using
-- <http://github.com/bos/configurator Configurator>.
loadConfig :: String
           -- ^ The name of the config file
           -> FilePath
           -- ^ The directory to look in
           -> IO Config
loadConfig file dir =  C.load $ [Required (dir </> file)]


--------------------------------------------------------------------------------
-- | Data structure for holdings configuration for connecting to
-- Postgres database
data DbConfig = DbConfig { _dbConfigHost     :: Maybe String
                         , _dbConfigPort     :: Maybe Int
                         , _dbConfigUser     :: Maybe String
                         , _dbConfigPassword :: Maybe String
                         , _dbConfigDatabase :: Maybe String }

--------------------------------------------------------------------------------
-- | Monoid instance for DbConfig. If there is a value on the RHS it will
-- overwrite.
instance Monoid DbConfig where
  mempty = DbConfig { _dbConfigHost     = Nothing
                    , _dbConfigPort     = Nothing
                    , _dbConfigUser     = Nothing
                    , _dbConfigPassword = Nothing
                    , _dbConfigDatabase = Nothing }
  a `mappend` b = DbConfig
      { _dbConfigHost     = ov _dbConfigHost
      , _dbConfigPort     = ov _dbConfigPort
      , _dbConfigUser     = ov _dbConfigUser
      , _dbConfigPassword = ov _dbConfigPassword
      , _dbConfigDatabase = ov _dbConfigDatabase
      }
    where
      ov f = getLast $! (mappend `on` (Last . f)) a b

--------------------------------------------------------------------------------
-- | Default settings for Postgres connection
defaultDbConfig :: DbConfig
defaultDbConfig = DbConfig { _dbConfigHost     = Just "localhost"
                           , _dbConfigPort     = Just 5432
                           , _dbConfigUser     = Just "backtest"
                           , _dbConfigPassword = Just ""
                           , _dbConfigDatabase = Just "backtest" }

--------------------------------------------------------------------------------
-- | Transform Configurator db.connection group into DbConfig
getDbConfig :: Config -> IO DbConfig
getDbConfig cfg = do
  host <- C.lookup cfg "db.connectInfo.host"
  port <- C.lookup cfg "db.connectInfo.port"
  user <- C.lookup cfg "db.connectInfo.user"
  pass <- C.lookup cfg "db.connectInfo.password"
  db   <- C.lookup cfg "db.connectInfo.database"
  return $ DbConfig host port user pass db

--------------------------------------------------------------------------------
-- | Combine default, file, and source configuration
initDbConfig :: Maybe DbConfig
             -- ^ Database config from file
             -> Maybe DbConfig
             -- ^ Database config in source
             -> DbConfig
initDbConfig cfg cfg' =  foldl1' mappend $
  defaultDbConfig : catMaybes [cfg, cfg']

--------------------------------------------------------------------------------
-- | Data structure for common backtest configuration
data BacktestConfig
  = BacktestConfig { _bcDescription :: Maybe Text
                   , _bcStartDate   :: Maybe Day
                   , _bcStartValue  :: Maybe Value
                   , _bcFrequency   :: Maybe Frequency
                   , _bcCutoff      :: Maybe Cutoff
                   , _bcBuffer      :: Maybe Buffer
                   }

--------------------------------------------------------------------------------
-- | Default backtest configuration
defaultBacktestConfig :: BacktestConfig
defaultBacktestConfig
  = BacktestConfig { _bcDescription = Just "Backtest"
                   , _bcStartDate   = Just $ fromGregorian 2016 1 1
                   , _bcStartValue  = Just 1000000
                   , _bcFrequency   = Just $ mkFrequency Third Friday 2
                   , _bcCutoff      = Just 0.5
                   , _bcBuffer      = Just 0
                   }

--------------------------------------------------------------------------------
-- | Tranform configurator backtest group into Backtest config
getBacktestConfig :: Config -> IO BacktestConfig
getBacktestConfig cfg = do
  desc <- C.lookup cfg "backtest.description"
  sd <- do
    sd' <- C.lookup cfg "backtest.start-date"
    return $ sd' >>= parseDay
  sv <- C.lookup cfg "backtest.start-value"
  freq <- C.lookup cfg "backtest.frequency"
  cut <- C.lookup cfg "backtest.cutoff"
  buf <- C.lookup cfg "backtest.buffer"
  return $ BacktestConfig desc sd sv freq cut buf

parseDay :: String -> Maybe Day
parseDay s = firstJust tryParsers
  where
    firstJust = listToMaybe . catMaybes
    formats = [ "%Y-%m-%d"
              , "%m/%d/%y"
              , "%m/%d/%Y"
              ]
    parseWith fmt = parseTimeM True defaultTimeLocale fmt s
    tryParsers = parseWith <$> formats

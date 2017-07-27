{-# LANGUAGE OverloadedStrings #-}

{-}
Loading configuration for a backtest. Configuration includes database connection
info and backtest configuration. Configuation may have a default, then they will
be loaded from the app.cfg file, then they can be overwritten by commnand line
args.
-}

module Backtest.Config
       ( DbConfigSource (..)
       , BacktestConfigSource(..)
       ) where

import           Backtest.Types          (Buffer, Cutoff, DbConfig, Frequency,
                                          Ordinal (..), Value, Weekday (..),
                                          mkFrequency)

import           Data.Configurator       as C
import           Data.Configurator.Types (Config)
import           Data.Function           (on)
import           Data.List               (foldl1')
import           Data.Maybe              (catMaybes, listToMaybe)
import           Data.Semigroup          (Last (..), Semigroup, getLast, (<>))
import           Data.String             (fromString)
import           Data.Text               (Text)
import           Data.Time               (Day, defaultTimeLocale, fromGregorian,
                                          parseTimeM)
import           System.FilePath         ((</>))


--------------------------------------------------------------------------------
-- | Load configuration from a file using
-- <http://github.com/bos/configurator Configurator>.
loadConfig :: String
           -- ^ The name of the config file
           -> FilePath
           -- ^ The directory to look in
           -> IO Config
loadConfig file dir =  C.load [Required (dir </> file)]


class Semigroup a => ConfigSource a where
  def :: a
  extract :: Config -> IO a

  -- | Start with default config, then update with any config in the first
  -- parameter, this is from the config file for instance, then update from
  -- anything from the second parameter, these are command line options for
  -- instance
  init :: Maybe a -> Maybe a -> a
  init c c' = foldl1' (<>) $
    def : catMaybes [c, c']


--------------------------------------------------------------------------------
-- | Data structure for holdings possible configuration for connecting to
-- Postgres database
data DbConfigSource
  = DbConfigSource { _dbConfigHost     :: Maybe String
                   , _dbConfigPort     :: Maybe Int
                   , _dbConfigUser     :: Maybe String
                   , _dbConfigPassword :: Maybe String
                   , _dbConfigDatabase :: Maybe String }


-- | Monoid instance for DbConfig. If there is a value on the RHS it will
-- overwrite.
instance Semigroup DbConfigSource where
  a <> b = DbConfigSource { _dbConfigHost     = ov _dbConfigHost
                          , _dbConfigPort     = ov _dbConfigPort
                          , _dbConfigUser     = ov _dbConfigUser
                          , _dbConfigPassword = ov _dbConfigPassword
                          , _dbConfigDatabase = ov _dbConfigDatabase }
    where
      ov f = getLast $ ((<>) `on` (Last . f)) a b

instance ConfigSource DbConfigSource where
  def = DbConfigSource { _dbConfigHost     = Just "localhost"
                       , _dbConfigPort     = Just 5432
                       , _dbConfigUser     = Just "backtest"
                       , _dbConfigPassword = Just ""
                       , _dbConfigDatabase = Just "backtest" }

  extract c = do
    host <- C.lookup c "db.connectInfo.host"
    port <- C.lookup c "db.connectInfo.port"
    user <- C.lookup c "db.connectInfo.user"
    pass <- C.lookup c "db.connectInfo.password"
    db   <- C.lookup c "db.connectInfo.database"
    return $ DbConfigSource host port user pass db


--------------------------------------------------------------------------------
-- | Data structure for common backtest configuration
data BacktestConfigSource
  = BacktestConfigSource { _bcDescription :: Maybe Text
                         , _bcStartDate   :: Maybe Day
                         , _bcStartValue  :: Maybe Value
                         , _bcFrequency   :: Maybe Frequency
                         , _bcCutoff      :: Maybe Cutoff
                         , _bcBuffer      :: Maybe Buffer
                         }

instance Semigroup BacktestConfigSource where
  a <> b = BacktestConfigSource { _bcDescription = ov _bcDescription
                                , _bcStartDate = ov _bcStartDate
                                , _bcStartValue = ov _bcStartValue
                                , _bcFrequency = ov _bcFrequency
                                , _bcCutoff = ov _bcCutoff
                                , _bcBuffer = ov _bcBuffer
                                }
    where
      ov f = getLast $ ((<>) `on` (Last . f)) a b


instance ConfigSource BacktestConfigSource where
  def = BacktestConfigSource { _bcDescription = Just "Backtest"
                             , _bcStartDate   = Just $ fromGregorian 2016 1 1
                             , _bcStartValue  = Just 1000000
                             , _bcFrequency   = Just $ mkFrequency Third Friday 2
                             , _bcCutoff      = Just 0.5
                             , _bcBuffer      = Just 0
                             }

  extract c = do
    desc <- C.lookup c "backtest.description"
    sd <- do
      sd' <- C.lookup c "backtest.start-date"
      return $ sd' >>= parseDay

    sv <- C.lookup c "backtest.start-value"
    o  <- C.lookup c "backtest.frequency.ordinal"
    wd <- C.lookup c "backtest.frequency.weekday"
    wt <- C.lookup c "backtest.frequency.wait"
    let freq = case (o, wd, wt) of
          (Just o', Just wd', Just wt') -> Just $ parseFrequency o' wd' wt'
          _ -> Nothing

    cut <- C.lookup c "backtest.cutoff"
    buf <- C.lookup c "backtest.buffer"
    return $ BacktestConfigSource desc sd sv freq cut buf


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

parseFrequency :: String
               -- ^ Ordinal
               -> String
               -- ^ Weekday
               -> Int
               -- ^ Wait
               -> Frequency
parseFrequency ordinal weekday
  = mkFrequency (fromString ordinal) (fromString weekday)

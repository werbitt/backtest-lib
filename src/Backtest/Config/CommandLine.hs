module Backtest.Config.CommandLine where

import           Backtest.Config     (DbConfigSource (..), BacktestConfigSource (..))
import           Data.Monoid         ((<>))
import           Options.Applicative (Parser, auto, help, long, metavar, option,
                                      showDefault, str, strOption, value)

dbOpts :: Parser DbConfigSource
dbOpts = DbConfigSource
  <$> option (Just <$> str) ( long "db-host"
                           <> metavar "HOSTNAME"
                           <> value Nothing
                           <> showDefault
                           <> help "The hostname for the DB connection" )
  <*> option (Just <$> auto) ( long "db-port"
                            <> metavar "PORT"
                            <> value Nothing
                            <> showDefault
                            <> help "The port for the DB connection" )
  <*> option (Just <$> str) ( long "db-user"
                           <> metavar "USERNAME"
                           <> value Nothing
                           <> showDefault
                           <> help "Username for the DB connection" )
  <*> option (Just <$> str) ( long "db-pass"
                           <> metavar "PASS"
                           <> value Nothing
                           <> showDefault
                           <> help "The password for the DB connection" )
  <*> option (Just <$> str) ( long "db-name"
                           <> metavar "DBNAME"
                           <> value Nothing
                           <> showDefault
                           <> help "The database name for the DB connection" )

backtestOpts :: Parser BacktestConfigSource
backtestOpts = BacktestConfigSource
  <$> option (Just <$> str) ( long "description"
                           <> short 'd'
                           <> value pure
                           <>

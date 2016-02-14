module Rd.Types
       ( Env(..)
       , Backtest ) where

import           Control.Monad.Reader
import           Database.PostgreSQL.Simple (Connection)

type Backtest a = ReaderT Env IO a

data Env = Env { _conn    :: Connection
               , _version :: Int }

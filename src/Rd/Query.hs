{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Rd.Query
       (
         lastHistoryVersion
       , connection
       ) where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye                    as O

-- |
-- = Database Connection

connectInfo :: PGS.ConnectInfo
connectInfo = PGS.ConnectInfo { PGS.connectHost = "localhost"
                              , PGS.connectPort = 5432
                              , PGS.connectUser = "backtest"
                              , PGS.connectPassword = ""
                              , PGS.connectDatabase = "micah" }

connection :: IO PGS.Connection
connection = PGS.connect connectInfo


-- |
-- = History Version
--
-- This is used because we don't have a prope security master.
-- This way we can can have different security data associated with
-- different backtests.

data HistoryVersion' a = HistoryVersion { _version :: a } deriving Show

type HistoryVersion = HistoryVersion' Int
type HistoryVersionColumn = HistoryVersion' (O.Column O.PGInt4)

$(makeAdaptorAndInstance "pHistoryVersion" ''HistoryVersion')

historyVersionTable :: O.Table HistoryVersionColumn HistoryVersionColumn
historyVersionTable = O.Table "history_version"
  (pHistoryVersion HistoryVersion { _version = O.required "id" })

historyVersionQuery :: O.Query HistoryVersionColumn
historyVersionQuery = O.queryTable historyVersionTable

lastHistoryVersionQuery :: O.Query HistoryVersionColumn
lastHistoryVersionQuery = O.aggregate
  (pHistoryVersion HistoryVersion { _version = O.max })
  historyVersionQuery

lastHistoryVersion :: PGS.Connection -> IO Int
lastHistoryVersion conn = do
   result <- O.runQuery conn lastHistoryVersionQuery :: IO [HistoryVersion]
   return $ _version . head $ result

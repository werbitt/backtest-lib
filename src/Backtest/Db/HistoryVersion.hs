{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.HistoryVersion
       (
         historyVersionId
       , historyVersionQuery
       , historyVersionUniverse
       , pHistoryVersion
       , HistoryVersion'(..)
       ) where

import           Backtest.Db.Ids            (HistoryVersionId,
                                             HistoryVersionId' (..),
                                             HistoryVersionIdColumn,
                                             HistoryVersionIdColumnMaybe,
                                             pHistoryVersionId)
import           Control.Lens               (makeLenses)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Opaleye                    (Column, PGText, PGTimestamptz,
                                             Query, Table (..), optional,
                                             queryTable, required)
import           Prelude                    hiding (max)

--Created At---------------------------------------------------------------------

data HistoryVersionCreatedAt' a
  = HistoryVersionCreatedAt { unHistoryVersionCreatedAt :: a } deriving Show
makeAdaptorAndInstance "pHistoryVersionCreatedAt" ''HistoryVersionCreatedAt'
type HistoryVersionCreatedAt = HistoryVersionCreatedAt' UTCTime
type HistoryVersionCreatedAtColumn
  = HistoryVersionCreatedAt' (Column PGTimestamptz)
type HistoryVersionCreatedAtColumnMaybe
  = HistoryVersionCreatedAt' (Maybe (Column PGTimestamptz))


--History Version----------------------------------------------------------------

data HistoryVersion' a b c
  = HistoryVersion { _historyVersionId        :: a
                   , _historyVersionUniverse  :: b
                   , _historyVersionCreatedAt :: c } deriving Show
makeLenses ''HistoryVersion'

type HistoryVersionColumns = HistoryVersion' HistoryVersionIdColumn
                                             (Column PGText)
                                             HistoryVersionCreatedAtColumn

type HistoryVersionInsertColumns = HistoryVersion' HistoryVersionIdColumnMaybe
                                                   (Column PGText)
                                                   HistoryVersionCreatedAtColumnMaybe
type HistoryVersion = HistoryVersion' HistoryVersionId Text HistoryVersionCreatedAt

makeAdaptorAndInstance "pHistoryVersion" ''HistoryVersion'

historyVersionTable :: Table HistoryVersionInsertColumns HistoryVersionColumns
historyVersionTable = Table "history_version"
  (pHistoryVersion
   HistoryVersion { _historyVersionId = pHistoryVersionId . HistoryVersionId $ optional "id"
                  , _historyVersionUniverse = required "universe"
                  , _historyVersionCreatedAt =
                      pHistoryVersionCreatedAt . HistoryVersionCreatedAt $ optional "created_at"})

historyVersionQuery :: Query HistoryVersionColumns
historyVersionQuery = queryTable historyVersionTable

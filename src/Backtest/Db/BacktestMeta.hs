{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.BacktestMeta
       ( BacktestMetaCreatedAt'(..)
       , BacktestMeta'(..)
       , backtestMetaId
       , backtestMetaStartDt
       , backtestMetaStartValue
       , backtestMetaFrequency
       , backtestMetaWeighting
       , backtestMetaCreatedAt
       , backtestMetaHistoryVersion
       , backtestMetaTable
       ) where

import           Backtest.Db.Ids            (BacktestMetaId,
                                             BacktestMetaId' (..),
                                             BacktestMetaIdColumn,
                                             BacktestMetaIdColumnMaybe,
                                             HistoryVersionId,
                                             HistoryVersionId' (..),
                                             HistoryVersionIdColumn,
                                             pBacktestMetaId, pHistoryVersionId)
import           Control.Lens               (makeLenses)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Data.Time                  (Day, UTCTime)
import           Opaleye                    (Column, PGDate, PGFloat8, PGText,
                                             PGTimestamptz, Query, Table (..),
                                             optional, queryTable, required)


--Created At---------------------------------------------------------------------

data BacktestMetaCreatedAt' a
  = BacktestMetaCreatedAt { unBacktestMetaCreatedAt :: a } deriving Show
makeAdaptorAndInstance "pBacktestMetaCreatedAt" ''BacktestMetaCreatedAt'
type BacktestMetaCreatedAt = BacktestMetaCreatedAt' UTCTime
type BacktestMetaCreatedAtColumn = BacktestMetaCreatedAt' (Column PGTimestamptz)
type BacktestMetaCreatedAtColumnMaybe
  = BacktestMetaCreatedAt' (Maybe (Column PGTimestamptz))


--Backtest Meta------------------------------------------------------------------

data BacktestMeta' a b c d e f g
  = BacktestMeta { _backtestMetaId             :: a
                 , _backtestMetaStartDt        :: b
                 , _backtestMetaStartValue     :: c
                 , _backtestMetaFrequency      :: d
                 , _backtestMetaWeighting      :: e
                 , _backtestMetaCreatedAt      :: f
                 , _backtestMetaHistoryVersion :: g }

makeLenses ''BacktestMeta'
makeAdaptorAndInstance "pBacktestMeta" ''BacktestMeta'

type BacktestMetaColumns = BacktestMeta' BacktestMetaIdColumn
                                         (Column PGDate)
                                         (Column PGFloat8) -- Change to numeric
                                         (Column PGText)
                                         (Column PGText)
                                         BacktestMetaCreatedAtColumn
                                         HistoryVersionIdColumn

type BacktestMetaInsertColumns = BacktestMeta' BacktestMetaIdColumnMaybe
                                               (Column PGDate)
                                               (Column PGFloat8)
                                               (Column PGText)
                                               (Column PGText)
                                               BacktestMetaCreatedAtColumnMaybe
                                               HistoryVersionIdColumn
type BacktestMeta
  = BacktestMeta' BacktestMetaId
                  Day
                  Double
                  Text
                  Text
                  BacktestMetaCreatedAt
                  HistoryVersionId

backtestMetaTable :: Table BacktestMetaInsertColumns BacktestMetaColumns
backtestMetaTable = Table "backtest_meta" $ pBacktestMeta BacktestMeta
  { _backtestMetaId = pBacktestMetaId . BacktestMetaId $ optional "id"
  , _backtestMetaStartDt = required "start_dt"
  , _backtestMetaStartValue = required "start_value"
  , _backtestMetaFrequency = required "frequency"
  , _backtestMetaWeighting = required "weighting"
  , _backtestMetaCreatedAt = pBacktestMetaCreatedAt . BacktestMetaCreatedAt $
    optional "created_at"
  , _backtestMetaHistoryVersion = pHistoryVersionId . HistoryVersionId $
    required "history_version"
  }

backtestMetaQuery :: Query BacktestMetaColumns
backtestMetaQuery = queryTable backtestMetaTable

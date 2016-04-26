{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Backtest
       ( BacktestCreatedAt'(..)
       , Backtest'(..)
       , backtestId
       , backtestStartDt
       , backtestStartValue
       , backtestFrequency
       , backtestWeighting
       , backtestCreatedAt
       , backtestHistoryVersion
       , backtestTable
       ) where

import           Backtest.Db.Ids            (BacktestId, BacktestId' (..),
                                             BacktestIdColumn,
                                             BacktestIdColumnMaybe,
                                             HistoryVersionId,
                                             HistoryVersionId' (..),
                                             HistoryVersionIdColumn,
                                             pBacktestId, pHistoryVersionId)
import           Control.Lens               (makeLenses)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Data.Time                  (Day, UTCTime)
import           Opaleye                    (Column, PGDate, PGFloat8, PGText,
                                             PGTimestamptz, Query, Table (..),
                                             optional, queryTable, required)


--Created At---------------------------------------------------------------------

data BacktestCreatedAt' a
  = BacktestCreatedAt { unBacktestCreatedAt :: a } deriving Show
makeAdaptorAndInstance "pBacktestCreatedAt" ''BacktestCreatedAt'
type BacktestCreatedAt = BacktestCreatedAt' UTCTime
type BacktestCreatedAtColumn = BacktestCreatedAt' (Column PGTimestamptz)
type BacktestCreatedAtColumnMaybe
  = BacktestCreatedAt' (Maybe (Column PGTimestamptz))


--Backtest-----------------------------------------------------------------------

data Backtest' a b c d e f g
  = Backtest { _backtestId             :: a
             , _backtestStartDt        :: b
             , _backtestStartValue     :: c
             , _backtestFrequency      :: d
             , _backtestWeighting      :: e
             , _backtestCreatedAt      :: f
             , _backtestHistoryVersion :: g }

makeLenses ''Backtest'
makeAdaptorAndInstance "pBacktest" ''Backtest'

type BacktestColumns = Backtest' BacktestIdColumn
                                 (Column PGDate)
                                 (Column PGFloat8) -- Change to numeric
                                 (Column PGText)
                                 (Column PGText)
                                 BacktestCreatedAtColumn
                                 HistoryVersionIdColumn

type BacktestInsertColumns = Backtest' BacktestIdColumnMaybe
                                       (Column PGDate)
                                       (Column PGFloat8)
                                       (Column PGText)
                                       (Column PGText)
                                       BacktestCreatedAtColumnMaybe
                                       HistoryVersionIdColumn
type Backtest
  = Backtest' BacktestId
              Day
              Double
              Text
              Text
              BacktestCreatedAt
              HistoryVersionId

backtestTable :: Table BacktestInsertColumns BacktestColumns
backtestTable = Table "backtest" $ pBacktest Backtest
  { _backtestId = pBacktestId . BacktestId $ optional "id"
  , _backtestStartDt = required "start_dt"
  , _backtestStartValue = required "start_value"
  , _backtestFrequency = required "frequency"
  , _backtestWeighting = required "weighting"
  , _backtestCreatedAt = pBacktestCreatedAt . BacktestCreatedAt $
    optional "created_at"
  , _backtestHistoryVersion = pHistoryVersionId . HistoryVersionId $
    required "history_version"
  }

backtestQuery :: Query BacktestColumns
backtestQuery = queryTable backtestTable

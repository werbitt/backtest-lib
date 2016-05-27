{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Backtest
       ( BacktestCreatedAt'(..)
       , Backtest'(..)
       , Backtest
       , backtestId
       , backtestStartDt
       , backtestStartValue
       , backtestFrequency
       , backtestCutoff
       , backtestBuffer
       , backtestCreatedAt
       , backtestHistoryVersion
       , backtestTable
       , backtestQuery
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

data Backtest' a b c d e f g h i
  = Backtest { _backtestId             :: a
             , _backtestDesc           :: b
             , _backtestStartDt        :: c
             , _backtestStartValue     :: d
             , _backtestFrequency      :: e
             , _backtestCutoff         :: f
             , _backtestBuffer         :: g
             , _backtestCreatedAt      :: h
             , _backtestHistoryVersion :: i }

makeLenses ''Backtest'
makeAdaptorAndInstance "pBacktest" ''Backtest'

type BacktestColumns = Backtest' BacktestIdColumn
                                 (Column PGText)
                                 (Column PGDate)
                                 (Column PGFloat8) -- Change to numeric
                                 (Column PGText)
                                 (Column PGFloat8)
                                 (Column PGFloat8)
                                 BacktestCreatedAtColumn
                                 HistoryVersionIdColumn

type BacktestInsertColumns = Backtest' BacktestIdColumnMaybe
                                       (Column PGText)
                                       (Column PGDate)
                                       (Column PGFloat8)
                                       (Column PGText)
                                       (Column PGFloat8)
                                       (Column PGFloat8)
                                       BacktestCreatedAtColumnMaybe
                                       HistoryVersionIdColumn
type Backtest
  = Backtest' BacktestId
              Text
              Day
              Double
              Text
              Double
              Double
              BacktestCreatedAt
              HistoryVersionId

backtestTable :: Table BacktestInsertColumns BacktestColumns
backtestTable = Table "backtest" $ pBacktest Backtest
  { _backtestId = pBacktestId . BacktestId $ optional "id"
  , _backtestDesc = required "description"
  , _backtestStartDt = required "start_dt"
  , _backtestStartValue = required "start_value"
  , _backtestFrequency = required "frequency"
  , _backtestCutoff = required "cutoff"
  , _backtestBuffer = required "buffer"
  , _backtestCreatedAt = pBacktestCreatedAt . BacktestCreatedAt $
    optional "created_at"
  , _backtestHistoryVersion = pHistoryVersionId . HistoryVersionId $
    required "history_version"
  }

backtestQuery :: Query BacktestColumns
backtestQuery = queryTable backtestTable

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Holding
       ( Holding'(..)
       , pHolding
       , Holding
       , holdingId
       , holdingBacktestId
       , holdingDt
       , holdingAsset
       , holdingVal
       , holdingTable
       ) where

import           Backtest.Db.Ids            (BacktestId, BacktestId' (..),
                                             BacktestIdColumn, HoldingId,
                                             HoldingId' (..), HoldingIdColumn,
                                             HoldingIdColumnMaybe, pBacktestId,
                                             pHoldingId)
import           Backtest.Types             (Asset)
import           Control.Lens               (makeLenses)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time                  (Day)
import           Opaleye                    (Column, PGDate, PGFloat8, PGText,
                                             Query, Table (..), optional,
                                             queryTable, required)

data Holding' a b c d e = Holding { _holdingId         :: a
                                  , _holdingBacktestId :: b
                                  , _holdingDt         :: c
                                  , _holdingAsset      :: d
                                  , _holdingVal        :: e
                                  }
makeLenses ''Holding'

makeAdaptorAndInstance "pHolding" ''Holding'

type HoldingColumns = Holding' HoldingIdColumn
                               BacktestIdColumn
                               (Column PGDate)
                               (Column PGText)
                               (Column PGFloat8)
type HoldingInsertColumns = Holding' HoldingIdColumnMaybe
                                     BacktestIdColumn
                                     (Column PGDate)
                                     (Column PGText)
                                     (Column PGFloat8)

type Holding = Holding' HoldingId BacktestId Day Asset Double

holdingTable :: Table HoldingInsertColumns HoldingColumns
holdingTable = Table "holdings" $ pHolding Holding
  { _holdingId = pHoldingId . HoldingId $ optional "id"
  , _holdingBacktestId = pBacktestId . BacktestId $ required "backtest_id"
  , _holdingDt = required "dt"
  , _holdingAsset = required "asset"
  , _holdingVal = required "val"
  }

holdingQuery :: Query HoldingColumns
holdingQuery = queryTable holdingTable

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

import           Backtest.Db.Ids            (BacktestMetaId,
                                             BacktestMetaId' (..),
                                             BacktestMetaIdColumn, HoldingId,
                                             HoldingId' (..), HoldingIdColumn,
                                             HoldingIdColumnMaybe,
                                             pBacktestMetaId, pHoldingId)
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
                               BacktestMetaIdColumn
                               (Column PGDate)
                               (Column PGText)
                               (Column PGFloat8)
type HoldingInsertColumns = Holding' HoldingIdColumnMaybe
                                     BacktestMetaIdColumn
                                     (Column PGDate)
                                     (Column PGText)
                                     (Column PGFloat8)

type Holding = Holding' HoldingId BacktestMetaId Day Asset Double

holdingTable :: Table HoldingInsertColumns HoldingColumns
holdingTable = Table "holdings" $ pHolding Holding
  { _holdingId = pHoldingId . HoldingId $ optional "id"
  , _holdingBacktestId = pBacktestMetaId . BacktestMetaId $ required "backtest_id"
  , _holdingDt = required "dt"
  , _holdingAsset = required "asset"
  , _holdingVal = required "val"
  }

holdingQuery :: Query HoldingColumns
holdingQuery = queryTable holdingTable

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.PriceHistory
       (
         priceHistoryQuery
       , priceHistoryId
       , priceHistoryDt
       , priceHistorySecurityId
       , priceHistoryOpenPx
       , priceHistoryClosePx
       , priceHistoryTotalReturn
       , priceHistoryTotalReturnIndex
       , priceHistoryVolume
       , priceHistoryBeta
       , priceHistoryHistoryVersion
       , unPriceHistoryBeta
       ) where

import           Backtest.Db.Ids            (PriceHistoryId' (..),
                                             PriceHistoryIdColumn,
                                             PriceHistoryIdColumnMaybe,
                                             SecurityId, SecurityId' (..),
                                             SecurityIdColumn, pPriceHistoryId,
                                             pSecurityId)
import           Backtest.Types             (Price, Return)
import           Control.Lens               (makeLenses)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time                  (Day)
import           Opaleye                    (Column, Nullable, PGDate, PGFloat8,
                                             PGInt4, PGInt8, Query, Table (..),
                                             optional, queryTable, required)

--Beta---------------------------------------------------------------------------

data PriceHistoryBeta' a
  =  PriceHistoryBeta { unPriceHistoryBeta :: a } deriving Show
makeAdaptorAndInstance "pPriceHistoryBeta" ''PriceHistoryBeta'
type PriceHistoryBeta = PriceHistoryBeta' Double
type PriceHistoryBetaColumn = PriceHistoryBeta' (Column PGFloat8)
type PriceHistoryBetaColumnMaybe
  = PriceHistoryBeta' (Maybe (Column (Nullable PGFloat8)))
type PriceHistoryBetaColumnNullable
  = PriceHistoryBeta' (Column (Nullable PGFloat8))


--Price History------------------------------------------------------------------

data PriceHistory' a b c d e f g h i j
  = PriceHistory { _priceHistoryId               :: a
                 , _priceHistoryDt               :: b
                 , _priceHistorySecurityId       :: c
                 , _priceHistoryOpenPx           :: d
                 , _priceHistoryClosePx          :: e
                 , _priceHistoryTotalReturn      :: f
                 , _priceHistoryTotalReturnIndex :: g
                 , _priceHistoryVolume           :: h
                 , _priceHistoryBeta             :: i
                 , _priceHistoryHistoryVersion   :: j }

makeLenses ''PriceHistory'
makeAdaptorAndInstance "pPriceHistory" ''PriceHistory'

type PriceHistoryColumns = PriceHistory' PriceHistoryIdColumn
                                        (Column PGDate)
                                        SecurityIdColumn
                                        (Column PGFloat8)
                                        (Column PGFloat8)
                                        (Column PGFloat8)
                                        (Column PGFloat8)
                                        (Column PGInt8)
                                        PriceHistoryBetaColumnNullable
                                        (Column PGInt4)

type PriceHistoryInsertColumns = PriceHistory' PriceHistoryIdColumnMaybe
                                               (Column PGDate)
                                               SecurityIdColumn
                                               (Column PGFloat8)
                                               (Column PGFloat8)
                                               (Column PGFloat8)
                                               (Column PGFloat8)
                                               (Column PGInt8)
                                               PriceHistoryBetaColumnMaybe
                                               (Column PGInt4)


type PriceHistory
  = PriceHistory' Day SecurityId Price Price Return Price Double (Maybe PriceHistoryBeta) Int


priceHistoryTable :: Table PriceHistoryInsertColumns PriceHistoryColumns
priceHistoryTable = Table "price_history"
  (pPriceHistory
   PriceHistory { _priceHistoryId = pPriceHistoryId . PriceHistoryId $ optional "id"
                , _priceHistoryDt = required "dt"
                , _priceHistorySecurityId = pSecurityId . SecurityId $ required "security_id"
                , _priceHistoryOpenPx = required "open_px"
                , _priceHistoryClosePx = required "close_px"
                , _priceHistoryTotalReturn = required "total_return"
                , _priceHistoryTotalReturnIndex = required "total_return_index"
                , _priceHistoryVolume = required "volume"
                , _priceHistoryBeta = pPriceHistoryBeta . PriceHistoryBeta $ optional "beta"
                , _priceHistoryHistoryVersion = required "history_version" })

priceHistoryQuery :: Query PriceHistoryColumns
priceHistoryQuery = queryTable priceHistoryTable

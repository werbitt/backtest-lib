{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Ids
       ( HistoryVersionId'(..)
       , HistoryVersionId
       , HistoryVersionIdColumn
       , HistoryVersionIdColumnMaybe
       , SecurityId' (..)
       , pSecurityId
       , SecurityId
       , SecurityIdColumn
       , SecurityIdColumnMaybe
--     , SecurityIdColumnNullable
       , PriceHistoryId' (..)
       , pPriceHistoryId
       , PriceHistoryId
       , PriceHistoryIdColumn
       , PriceHistoryIdColumnMaybe
       , pHistoryVersionId
       ) where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Column, PGInt4)

--History Version----------------------------------------------------------------

data HistoryVersionId' a = HistoryVersionId { unHistoryVersionId :: a }
                         deriving (Show)
makeAdaptorAndInstance "pHistoryVersionId" ''HistoryVersionId'

type HistoryVersionId = HistoryVersionId' Int
type HistoryVersionIdColumn = HistoryVersionId' (Column PGInt4)
type HistoryVersionIdColumnMaybe = HistoryVersionId' (Maybe (Column PGInt4))


--Security-----------------------------------------------------------------------

data SecurityId' a = SecurityId { unSecurityId :: a } deriving (Show, Eq, Ord)
makeAdaptorAndInstance "pSecurityId" ''SecurityId'

type SecurityId = SecurityId' Int
type SecurityIdColumn = SecurityId' (Column PGInt4)
type SecurityIdColumnMaybe = SecurityId' (Maybe (Column PGInt4))
--type SecurityIdColumnNullable = SecurityId' (Column (Nullable PGInt4))

--Price History------------------------------------------------------------------

data PriceHistoryId' a = PriceHistoryId { unPriceHistoryId :: a } deriving Show
makeAdaptorAndInstance "pPriceHistoryId" ''PriceHistoryId'

type PriceHistoryId = PriceHistoryId' Int
type PriceHistoryIdColumn = PriceHistoryId' (Column PGInt4)
type PriceHistoryIdColumnMaybe = PriceHistoryId' (Maybe (Column PGInt4))

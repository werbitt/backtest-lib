{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Ids
       ( SecurityId' (..)
       , pSecurityId
       , SecurityId
       , SecurityIdColumn
       , SecurityIdColumnMaybe
--     , SecurityIdColumnNullable
       , MemberId' (..)
       , pMemberId
       , MemberId
       , MemberIdColumn
       , MemberIdColumnMaybe
       , HistoryVersionId'(..)
       , HistoryVersionId
       , HistoryVersionIdColumn
       , HistoryVersionIdColumnMaybe
       , PriceHistoryId' (..)
       , pPriceHistoryId
       , PriceHistoryId
       , PriceHistoryIdColumn
       , PriceHistoryIdColumnMaybe
       , pHistoryVersionId
       , BacktestId'(..)
       , pBacktestId
       , BacktestId
       , BacktestIdColumn
       , BacktestIdColumnMaybe
       , HoldingId'(..)
       , pHoldingId
       , HoldingId
       , HoldingIdColumn
       , HoldingIdColumnMaybe
       ) where

import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Column, PGInt4, PGInt8)


--Security-----------------------------------------------------------------------

data SecurityId' a = SecurityId { unSecurityId :: a } deriving (Show, Eq, Ord)
makeAdaptorAndInstance "pSecurityId" ''SecurityId'

type SecurityId = SecurityId' Int
type SecurityIdColumn = SecurityId' (Column PGInt4)
type SecurityIdColumnMaybe = SecurityId' (Maybe (Column PGInt4))
--type SecurityIdColumnNullable = SecurityId' (Column (Nullable PGInt4))


--Member-------------------------------------------------------------------------

data MemberId' a = MemberId { unMemberId :: a } deriving Show
makeAdaptorAndInstance "pMemberId" ''MemberId'

type MemberId = MemberId' Int64
type MemberIdColumn = MemberId' (Column PGInt8)
type MemberIdColumnMaybe = MemberId' (Maybe (Column PGInt8))


--History Version----------------------------------------------------------------

data HistoryVersionId' a = HistoryVersionId { unHistoryVersionId :: a }
                         deriving (Show)
makeAdaptorAndInstance "pHistoryVersionId" ''HistoryVersionId'

type HistoryVersionId = HistoryVersionId' Int
type HistoryVersionIdColumn = HistoryVersionId' (Column PGInt4)
type HistoryVersionIdColumnMaybe = HistoryVersionId' (Maybe (Column PGInt4))


--Price History------------------------------------------------------------------

data PriceHistoryId' a = PriceHistoryId { unPriceHistoryId :: a } deriving Show
makeAdaptorAndInstance "pPriceHistoryId" ''PriceHistoryId'

type PriceHistoryId = PriceHistoryId' Int
type PriceHistoryIdColumn = PriceHistoryId' (Column PGInt4)
type PriceHistoryIdColumnMaybe = PriceHistoryId' (Maybe (Column PGInt4))


--Backtest-----------------------------------------------------------------------

data BacktestId' a = BacktestId { unBacktestId :: a } deriving Show
makeAdaptorAndInstance "pBacktestId" ''BacktestId'

type BacktestId = BacktestId' Int
type BacktestIdColumn = BacktestId' (Column PGInt4)
type BacktestIdColumnMaybe = BacktestId' (Maybe (Column PGInt4))


--Holding------------------------------------------------------------------------

data HoldingId' a = HoldingId { unHoldingId :: a } deriving Show
makeAdaptorAndInstance "pHoldingId" ''HoldingId'

type HoldingId = HoldingId' Int64
type HoldingIdColumn = HoldingId' (Column PGInt8)
type HoldingIdColumnMaybe = HoldingId' (Maybe (Column PGInt8))

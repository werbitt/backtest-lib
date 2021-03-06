{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Ids
       ( SecurityId' (..)
       , pSecurityId
       , SecurityId
       , SecurityIdColumn
       , SecurityIdColumnMaybe
       , SecurityIdColumnNullable
       , SecurityIdColumnMaybeNullable
       , MemberId' (..)
       , pMemberId
       , MemberId
       , MemberIdColumn
       , MemberIdColumnMaybe
       , HistoryVersionId'(..)
       , HistoryVersionId
       , HistoryVersionIdColumn
       , HistoryVersionIdColumnMaybe
       , SecurityHistoryId'(..)
       , pSecurityHistoryId
       , SecurityHistoryId
       , SecurityHistoryIdColumn
       , SecurityHistoryIdColumnMaybe
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
       , ConstraintId'(..)
       , pConstraintId
       , ConstraintId
       , ConstraintIdColumn
       , ConstraintIdColumnMaybe
       , HoldingId'(..)
       , pHoldingId
       , HoldingId
       , HoldingIdColumn
       , HoldingIdColumnMaybe
       ) where

import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Column, Nullable, PGInt4, PGInt8)


--Security-----------------------------------------------------------------------

data SecurityId' a = SecurityId { unSecurityId :: a } deriving (Show, Eq, Ord)
makeAdaptorAndInstance "pSecurityId" ''SecurityId'

type SecurityId = SecurityId' Int
type SecurityIdColumn = SecurityId' (Column PGInt4)
type SecurityIdColumnMaybe = SecurityId' (Maybe (Column PGInt4))
type SecurityIdColumnNullable = SecurityId' (Column (Nullable PGInt4))
type SecurityIdColumnMaybeNullable = SecurityId' (Maybe (Column (Nullable PGInt4)))

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


--Security History---------------------------------------------------------------

data SecurityHistoryId' a = SecurityHistoryId {unSecurityHistoryId :: a }
                          deriving Show

makeAdaptorAndInstance "pSecurityHistoryId" ''SecurityHistoryId'

type SecurityHistoryId = SecurityHistoryId' Int
type SecurityHistoryIdColumn = SecurityHistoryId' (Column PGInt4)
type SecurityHistoryIdColumnMaybe = SecurityHistoryId' (Maybe (Column PGInt4))

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


--Constraint---------------------------------------------------------------------

data ConstraintId' a = ConstraintId {unConstraintId :: a } deriving Show
makeAdaptorAndInstance "pConstraintId" ''ConstraintId'

type ConstraintId = ConstraintId' Int
type ConstraintIdColumn = ConstraintId' (Column PGInt4)
type ConstraintIdColumnMaybe = ConstraintId' (Maybe (Column PGInt4))

--Holding------------------------------------------------------------------------

data HoldingId' a = HoldingId { unHoldingId :: a } deriving Show
makeAdaptorAndInstance "pHoldingId" ''HoldingId'

type HoldingId = HoldingId' Int64
type HoldingIdColumn = HoldingId' (Column PGInt8)
type HoldingIdColumnMaybe = HoldingId' (Maybe (Column PGInt8))

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Holding
       ( AssetClass(..)
       , pgAssetClass
       , Holding'(..)
       , pHolding
       , Holding
       , holdingId
       , holdingBacktestId
       , holdingDt
       , holdingAssetClass
       , holdingSecurityId
       , holdingVal
       , holdingTable
       , holdingQuery
       ) where

import           Backtest.Db.Ids                      (BacktestId,
                                                       BacktestId' (..),
                                                       BacktestIdColumn,
                                                       HoldingId,
                                                       HoldingId' (..),
                                                       HoldingIdColumn,
                                                       HoldingIdColumnMaybe,
                                                       SecurityId,
                                                       SecurityId' (..), SecurityIdColumnMaybeNullable,
                                                       SecurityIdColumnNullable,
                                                       pBacktestId, pHoldingId,
                                                       pSecurityId)
import           Control.Lens                         (makeLenses)
import           Data.Profunctor.Product.Default      (Default (..))
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Time                            (Day)
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       ResultError (..),
                                                       returnError, typename)
import           Opaleye                              (Column, Constant (..),
                                                       PGDate, PGFloat8, Query,
                                                       QueryRunnerColumnDefault,
                                                       Table (..), optional,
                                                       queryTable, required)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PGTypes             as IPT
import qualified Opaleye.Internal.RunQuery            as IRQ


--Asset Class---------------------------------------------------------------------

data AssetClass = CashAsset | EquityAsset

data PGAssetClass
pgAssetClass :: AssetClass -> Column PGAssetClass
pgAssetClass CashAsset = IPT.literalColumn . HPQ.StringLit $ "cash"
pgAssetClass EquityAsset = IPT.literalColumn . HPQ.StringLit $ "equity"

instance Default Constant AssetClass (Column PGAssetClass) where
  def = Constant pgAssetClass

instance FromField AssetClass where
  fromField f Nothing =
    returnError ConversionFailed f "Nothing returned for AssetClass"
  fromField f (Just x) = do
    tn <- typename f
    if tn == "asset_class"
      then case x of
      "cash" -> pure CashAsset
      "equity" -> pure EquityAsset
      v -> returnError ConversionFailed f
        ("Wrong value for AssetClass, saw: " ++ show v)
      else returnError Incompatible f
           ("Wrong database type for AssetClass, saw: " ++ show tn)

instance QueryRunnerColumnDefault PGAssetClass AssetClass where
  queryRunnerColumnDefault = IRQ.fieldQueryRunnerColumn


--Holding------------------------------------------------------------------------

data Holding' a b c d e f = Holding { _holdingId         :: a
                                    , _holdingBacktestId :: b
                                    , _holdingDt         :: c
                                    , _holdingAssetClass :: d
                                    , _holdingSecurityId :: e
                                    , _holdingVal        :: f
                                    }
makeLenses ''Holding'

makeAdaptorAndInstance "pHolding" ''Holding'

type HoldingColumns = Holding' HoldingIdColumn
                               BacktestIdColumn
                               (Column PGDate)
                               (Column PGAssetClass)
                               SecurityIdColumnNullable
                               (Column PGFloat8)
type HoldingInsertColumns = Holding' HoldingIdColumnMaybe
                                     BacktestIdColumn
                                     (Column PGDate)
                                     (Column PGAssetClass)
                                     SecurityIdColumnNullable
                                     (Column PGFloat8)

type Holding = Holding' HoldingId
                        BacktestId
                        Day
                        AssetClass
                        (Maybe SecurityId)
                        Double

holdingTable :: Table HoldingInsertColumns HoldingColumns
holdingTable = Table "holdings" $ pHolding Holding
  { _holdingId = pHoldingId . HoldingId $ optional "id"
  , _holdingBacktestId = pBacktestId . BacktestId $ required "backtest_id"
  , _holdingDt = required "dt"
  , _holdingAssetClass = required "asset_class"
  , _holdingSecurityId = pSecurityId . SecurityId $ required "security_id"
  , _holdingVal = required "val"
  }

holdingQuery :: Query HoldingColumns
holdingQuery = queryTable holdingTable

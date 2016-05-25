{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Constraint
       (
         constraintTable
       , Constraint'(..)
       , pgConstraintHook
       ) where

import           Backtest.Db.Ids                      (BacktestId,
                                                       BacktestId' (..),
                                                       BacktestIdColumn,
                                                       ConstraintId,
                                                       ConstraintId' (..),
                                                       ConstraintIdColumn,
                                                       ConstraintIdColumnMaybe,
                                                       pBacktestId,
                                                       pConstraintId)
import           Backtest.Types                       (ConstraintHook (..))
import           Control.Lens                         (makeLenses)
import           Data.Profunctor.Product.Default      (Default (..))
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Text                            (Text)
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       ResultError (..),
                                                       returnError, typename)
import           Opaleye                              (Column, Constant (..),
                                                       PGText, Table (..),
                                                       optional, required)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PGTypes             as IPT
import qualified Opaleye.Internal.RunQuery            as IRQ
import           Opaleye.RunQuery                     (QueryRunnerColumnDefault)

--Constraint Hook----------------------------------------------------------------

data PGConstraintHook
pgConstraintHook :: ConstraintHook -> Column PGConstraintHook
pgConstraintHook GlobalFilters = IPT.literalColumn . HPQ.OtherLit $ "global_filter"
pgConstraintHook LongFilters = IPT.literalColumn . HPQ.OtherLit $ "long_filter"
pgConstraintHook ShortFilters = IPT.literalColumn . HPQ.OtherLit $ "short_filter"
pgConstraintHook WeightConstraints = IPT.literalColumn . HPQ.OtherLit $ "global_weight"
pgConstraintHook ValueConstraints = IPT.literalColumn . HPQ.OtherLit $ "global_value"

instance Default Constant ConstraintHook (Column PGConstraintHook) where
  def = Constant pgConstraintHook

instance FromField ConstraintHook where
  fromField f Nothing =
    returnError ConversionFailed f "Nothing returned for ConstraintHook"
  fromField f (Just x) = do
    tn <- typename f
    if tn == "constraint_hook"
      then case x of
      "global_filter" -> pure GlobalFilters
      "long_filter" -> pure LongFilters
      "short_filter" -> pure ShortFilters
      "global_weight" -> pure WeightConstraints
      "global_value" -> pure ValueConstraints
      v -> returnError ConversionFailed f ("Wrong value for ConstraintHook, saw: " ++ show v)
      else returnError Incompatible f
           ("Wrong database type for ConstraintHook, saw: " ++ show tn)

instance QueryRunnerColumnDefault PGConstraintHook ConstraintHook where
  queryRunnerColumnDefault = IRQ.fieldQueryRunnerColumn


--Constraint---------------------------------------------------------------------

data Constraint' a b c d
  = Constraint { _constraintId         :: a
               , _constraintDesc       :: b
               , _constraintHook       :: c
               , _constraintBacktestId :: d}

makeLenses ''Constraint'
makeAdaptorAndInstance "pConstraint" ''Constraint'

type ConstraintColumns = Constraint' ConstraintIdColumn
                                     (Column PGText)
                                     (Column PGConstraintHook)
                                     BacktestIdColumn

type ConstraintInsertColumns = Constraint' ConstraintIdColumnMaybe
                                           (Column PGText)
                                           (Column PGConstraintHook)
                                           BacktestIdColumn

type Constraint = Constraint' ConstraintId
                              Text
                              ConstraintHook
                              BacktestId

constraintTable :: Table ConstraintInsertColumns ConstraintColumns
constraintTable = Table "backtest_constraint" $ pConstraint Constraint
  { _constraintId = pConstraintId . ConstraintId $ optional "id"
  , _constraintDesc = required "description"
  , _constraintHook = required "hook"
  , _constraintBacktestId = pBacktestId . BacktestId $ required "backtest_id"}

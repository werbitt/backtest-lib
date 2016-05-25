{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.SecurityHistory
       ( securityHistoryId
       , securityHistorySecurityId
       , securityHistoryTicker
       , securityHistoryGicsSector
       , securityHistoryGicsIndustry
       , securityHistoryBbSector
       , securityHistoryBbIndustry
       , securityHistoryHistoryVersion
       , securityHistoryQuery
       ) where

import qualified Backtest.Db.Ids            as ID
import           Backtest.Types             (Ticker)
import           Control.Lens               (makeLenses)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Opaleye                    (Column, Nullable, PGText, Query,
                                             Table (..), optional, queryTable,
                                             required)

data SecurityHistory' a b c d e f g h =
  SecurityHistory { _securityHistoryId             :: a
                  , _securityHistorySecurityId     :: b
                  , _securityHistoryTicker         :: c
                  , _securityHistoryGicsSector     :: d
                  , _securityHistoryGicsIndustry   :: e
                  , _securityHistoryBbSector       :: f
                  , _securityHistoryBbIndustry     :: g
                  , _securityHistoryHistoryVersion :: h }

makeLenses ''SecurityHistory'
makeAdaptorAndInstance "pSecurityHistory" ''SecurityHistory'

type SecurityHistoryColumns
  = SecurityHistory' ID.SecurityHistoryIdColumn
                     ID.SecurityIdColumn
                     (Column PGText)
                     (Column (Nullable PGText))
                     (Column (Nullable PGText))
                     (Column (Nullable PGText))
                     (Column (Nullable PGText))
                     ID.HistoryVersionIdColumn

type SecurityHistoryInsertColumns
  = SecurityHistory' ID.SecurityHistoryIdColumnMaybe
                     ID.SecurityIdColumn
                     (Column PGText)
                     (Maybe (Column (Nullable PGText)))
                     (Maybe (Column (Nullable PGText)))
                     (Maybe (Column (Nullable PGText)))
                     (Maybe (Column (Nullable PGText)))
                     ID.HistoryVersionIdColumn

type SecurityHistory
  = SecurityHistory' ID.SecurityHistoryId
                     ID.SecurityId
                     Ticker
                     (Maybe Text)
                     (Maybe Text)
                     (Maybe Text)
                     (Maybe Text)
                     ID.HistoryVersionId

securityHistoryTable :: Table SecurityHistoryInsertColumns SecurityHistoryColumns
securityHistoryTable
  = Table "security_history" $ pSecurityHistory SecurityHistory
  { _securityHistoryId = ID.pSecurityHistoryId . ID.SecurityHistoryId $
    optional "id"
  , _securityHistorySecurityId = ID.pSecurityId . ID.SecurityId $
    required "security_id"
  , _securityHistoryTicker = required "ticker"
  , _securityHistoryGicsSector = optional "gics_sector"
  , _securityHistoryGicsIndustry = optional "gics_industry"
  , _securityHistoryBbSector = optional "bb_industry_sector"
  , _securityHistoryBbIndustry = optional "bb_industry_group"
  , _securityHistoryHistoryVersion = ID.pHistoryVersionId . ID.HistoryVersionId $
    required "history_version" }

securityHistoryQuery :: Query SecurityHistoryColumns
securityHistoryQuery = queryTable securityHistoryTable

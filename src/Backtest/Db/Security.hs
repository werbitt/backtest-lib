{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Security
       (
       ) where

import           Backtest.Db.Ids            (SecurityId, SecurityId' (..),
                                             SecurityIdColumn,
                                             SecurityIdColumnMaybe, pSecurityId)
import           Control.Lens               (makeLenses)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Opaleye                    (Column, PGText, Table (..),
                                             optional, required)

data Security' a b = Security { _securityId       :: a
                              , _securityGlobalId :: b } deriving Show

makeLenses ''Security'
makeAdaptorAndInstance "pSecurity" ''Security'

type SecurityColumns = Security' SecurityIdColumn (Column PGText)
type SecurityInsertColumns = Security' SecurityIdColumnMaybe (Column PGText)
type Security = Security' SecurityId Text

securityTable :: Table SecurityInsertColumns SecurityColumns
securityTable = Table "security" $ pSecurity Security
  { _securityId = pSecurityId . SecurityId $ optional "id"
  , _securityGlobalId = required "id_bb_gobal" }

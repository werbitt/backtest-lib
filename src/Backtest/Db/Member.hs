{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Backtest.Db.Member
       ( memberId
       , memberUniverse
       , memberDt
       , memberSecurityId
       , memberQuery
       ) where

import           Backtest.Db.Ids            (MemberId' (..), MemberIdColumn,
                                             MemberIdColumnMaybe, SecurityId,
                                             SecurityId' (..), SecurityIdColumn,
                                             pMemberId, pSecurityId)
import           Control.Lens               (makeLenses)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Data.Time                  (Day)
import           Opaleye                    (Column, PGDate, PGText, Query,
                                             Table (..), optional, queryTable,
                                             required)

data Member' a b c d = Member { _memberId         :: a
                              , _memberUniverse   :: b
                              , _memberDt         :: c
                              , _memberSecurityId :: d }

makeLenses ''Member'
makeAdaptorAndInstance "pMember" ''Member'

type MemberColumns = Member' MemberIdColumn
                             (Column PGText)
                             (Column PGDate)
                             SecurityIdColumn

type MemberInsertColumns = Member' MemberIdColumnMaybe
                                   (Column PGText)
                                   (Column PGDate)
                                   SecurityIdColumn

type Member = Member' Int Text Day SecurityId

memberTable :: Table MemberInsertColumns MemberColumns
memberTable = Table "members"
  (pMember Member { _memberId = pMemberId . MemberId $ optional "id"
                  , _memberUniverse = required "universe"
                  , _memberDt = required "dt"
                  , _memberSecurityId = pSecurityId . SecurityId $ required "security_id" })

memberQuery :: Query MemberColumns
memberQuery = queryTable memberTable

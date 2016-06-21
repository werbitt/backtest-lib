{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Strat.Skew.Db
       (
         runSkewQuery
       )where

import qualified Backtest.Db.Ids            as ID
import           Backtest.Db.PriceHistory   (priceHistoryClosePx,
                                             priceHistoryDt,
                                             priceHistoryHistoryVersion,
                                             priceHistoryQuery,
                                             priceHistorySecurityId,
                                             priceHistoryVolume)
import           Backtest.Query             (membersForDay)
import           Backtest.Types             (mkEquity)
import           Control.Arrow              (returnA)
import           Control.Lens               (makeLenses, to, (^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time                  (Day)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye                    (Column, PGDate, PGFloat8, PGInt8,
                                             Query, QueryArr, Table (..),
                                             constant, queryTable, required,
                                             restrict, runQuery, (.==))
import           Strat.Skew.Types           (ImpliedVol, SkewData (..))

data SkewHistory' a b c d e f = SkewHistory { _skewDt             :: a
                                            , _skewSecurityId     :: b
                                            , _skewPut25d         :: c
                                            , _skewCall25d        :: d
                                            , _skewCall50d        :: e
                                            , _skewHistoryVersion :: f
                                            }
makeLenses ''SkewHistory'
makeAdaptorAndInstance "pSkewHistory" ''SkewHistory'

type SkewHistoryColumns = SkewHistory' (Column PGDate)
                                       ID.SecurityIdColumn
                                       (Column PGFloat8)
                                       (Column PGFloat8)
                                       (Column PGFloat8)
                                       ID.HistoryVersionIdColumn

type SkewHistory
  = SkewHistory' Day
                 ID.SecurityId
                 ImpliedVol
                 ImpliedVol
                 ImpliedVol
                 ID.HistoryVersionId

skewHistoryTable :: Table SkewHistoryColumns SkewHistoryColumns
skewHistoryTable = Table "skew_history" $ pSkewHistory SkewHistory
  { _skewDt = required "dt"
  , _skewSecurityId = ID.pSecurityId . ID.SecurityId $ required "security_id"
  , _skewPut25d = required "ivol_25d_put"
  , _skewCall25d = required "ivol_25d_call"
  , _skewCall50d = required "ivol_50d_call"
  , _skewHistoryVersion = ID.pHistoryVersionId . ID.HistoryVersionId $
    required "history_version" }

skewHistoryQuery :: Query SkewHistoryColumns
skewHistoryQuery = queryTable skewHistoryTable

skewHistoryPriceVolumeJoin
  :: QueryArr SkewHistoryColumns (Column PGFloat8, Column PGInt8)
skewHistoryPriceVolumeJoin = proc (sh) -> do
  ph <- priceHistoryQuery -< ()
  restrict -< sh^.skewSecurityId.to ID.unSecurityId .==
    ph^.priceHistorySecurityId.to ID.unSecurityId
  restrict -< sh^.skewDt .== ph^.priceHistoryDt
  restrict -< sh^.skewHistoryVersion.to ID.unHistoryVersionId .==
    ph^.priceHistoryHistoryVersion
  returnA -< (ph^.priceHistoryClosePx, ph^.priceHistoryVolume)


skewQuery :: ID.HistoryVersionId -> Day -> Query ( ID.SecurityIdColumn
                                                 , Column PGFloat8
                                                 , Column PGFloat8
                                                 , Column PGFloat8
                                                 , Column PGFloat8
                                                 , Column PGInt8)
skewQuery v d = proc () -> do
  sh <- skewHistoryQuery -< ()
  (px, vol) <- skewHistoryPriceVolumeJoin -< sh
  m <- membersForDay v d -< ()
  restrict -< sh^.skewDt .== constant d
  restrict -<
    sh^.skewHistoryVersion.to ID.unHistoryVersionId .== constant (ID.unHistoryVersionId v)
  restrict -< ID.unSecurityId m .== sh^.skewSecurityId.to ID.unSecurityId
  returnA -< (sh^.skewSecurityId, sh^.skewPut25d, sh^.skewCall25d, sh^.skewCall50d, px, vol)

runSkewQuery ::  MonadIO m => PGS.Connection -> ID.HistoryVersionId -> Day -> m [SkewData]
runSkewQuery c v d = do
  results <- liftIO
    (runQuery c (skewQuery v d)
     :: IO [(ID.SecurityId, ImpliedVol, ImpliedVol, ImpliedVol, Double, Int64)])
  return $ (\(t, p25d, c25d, c50d, px, vol) ->
             SkewData (mkEquity t) ((p25d - c25d) / c50d) (px * fromIntegral vol)) <$> results

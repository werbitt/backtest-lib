{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Strat.Skew.Db
       (
         runSkewQuery
       )where

import           Backtest.Query             (membersForDay, priceHistoryDt,
                                             priceHistoryQuery,
                                             priceHistoryTicker,
                                             priceHistoryVolume)
import           Backtest.Types             (Ticker, mkEquity)
import           Control.Arrow              (returnA)
import           Control.Lens               (makeLenses, (^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time                  (Day)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye                    (Column, PGDate, PGFloat8, PGInt4,
                                             PGText, Query, QueryArr,
                                             Table (..), constant, queryTable,
                                             required, restrict, runQuery,
                                             (.==))
import           Strat.Skew.Types           (ImpliedVol, SkewData (..), Volume)

data SkewHistory' a b c d e f = SkewHistory { _skewDt             :: a
                                            , _skewTicker         :: b
                                            , _skewPut25d         :: c
                                            , _skewCall25d        :: d
                                            , _skewCall50d        :: e
                                            , _skewHistoryVersion :: f
                                            }
makeLenses ''SkewHistory'
makeAdaptorAndInstance "pSkewHistory" ''SkewHistory'

type SkewHistoryColumns = SkewHistory' (Column PGDate)
                                       (Column PGText)
                                       (Column PGFloat8)
                                       (Column PGFloat8)
                                       (Column PGFloat8)
                                       (Column PGInt4)

type SkewHistory = SkewHistory' Day Ticker ImpliedVol ImpliedVol ImpliedVol Int

skewHistoryTable :: Table SkewHistoryColumns SkewHistoryColumns
skewHistoryTable = Table "skew_history" $ pSkewHistory SkewHistory
  { _skewDt = required "dt"
  , _skewTicker = required "ticker"
  , _skewPut25d = required "ivol_25d_put"
  , _skewCall25d = required "ivol_25d_call"
  , _skewCall50d = required "ivol_50d_call"
  , _skewHistoryVersion = required "history_version" }

skewHistoryQuery :: Query SkewHistoryColumns
skewHistoryQuery = queryTable skewHistoryTable

skewHistoryVolumeJoin :: QueryArr SkewHistoryColumns (Column PGFloat8)
skewHistoryVolumeJoin = proc (sh) -> do
  ph <- priceHistoryQuery -< ()
  restrict -< sh^.skewTicker .== ph^.priceHistoryTicker
  restrict -< sh^.skewDt .== ph^.priceHistoryDt
  returnA -< ph^.priceHistoryVolume


skewQuery :: Int -> Day -> Query ( Column PGText
                                 , Column PGFloat8
                                 , Column PGFloat8
                                 , Column PGFloat8
                                 , Column PGFloat8 )
skewQuery v d = proc () -> do
  sh <- skewHistoryQuery -< ()
  vol <- skewHistoryVolumeJoin -< (sh)
  m <- membersForDay v d -< ()
  restrict -< sh^.skewDt .== constant d
  restrict -< sh^.skewHistoryVersion .== constant v
  restrict -< m .== sh^.skewTicker
  returnA -< (sh^.skewTicker, sh^.skewPut25d, sh^.skewCall25d, sh^.skewCall50d, vol)

runSkewQuery ::  MonadIO m => PGS.Connection -> Int -> Day -> m [SkewData]
runSkewQuery c v d = do
  results <- liftIO
    (runQuery c (skewQuery v d) :: IO [(Ticker, ImpliedVol, ImpliedVol, ImpliedVol, Volume)])
  return $ (\(t, p25d, c25d, c50d, vol') ->
             SkewData (mkEquity t) ((p25d - c25d) / c50d) vol') <$> results

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}


module Strat.Skew where


import           Backtest.Backtest      (run)
import           Backtest.Constraint    (HasVolume, addVolumeConstraint)
import           Backtest.Example       (run')
import           Backtest.Types         (CanDb, Constraints, Strategy (..),
                                         connection, historyVersion)
import           Control.Lens           (view, (^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (sortOn)
import           Data.Ord               (Down (..))
import           Data.Time              (Day)
import           Strat.Skew.Db          (runSkewQuery)
import           Strat.Skew.Types       (SkewData, skewDataSkew)

skew :: CanDb r m => Strategy m SkewData
skew = Strategy getSkews rankSkews


getSkews :: CanDb r m => Day -> m [SkewData]
getSkews d = do
  c <- view connection
  v <- view historyVersion
  liftIO $ runSkewQuery c v d


rankSkews :: [SkewData] -> [SkewData]
rankSkews = sortOn (Down . (^.skewDataSkew))

constraints :: HasVolume a => Constraints a
constraints = addVolumeConstraint 0.3 []

go :: IO ()
go = run' $ run skew constraints

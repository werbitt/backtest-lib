{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib
    (
      run
    ) where

import           Control.Lens           (view)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, runReaderT)
import           Control.Monad.Trans    (lift, liftIO)
import           Data.List              (sortBy)
import           Data.Time              (Day, fromGregorian)
import           Rd.Optimize            (optimize)
import qualified Rd.Query               as Q
import           Rd.Types               (Asset, Backtest, Constraints, Env,
                                         HasAsset, Ordinal (..), Params,
                                         PortfolioW, Strategy, Wait,
                                         Weekday (..), connection,
                                         historyVersion, mkConstraints, mkEnv,
                                         mkEquity, mkParams, mkStrategy,
                                         unBacktest)

run :: Params -> Backtest a -> IO a
run params m = do
  conn <- Q.connection
  version <- Q.lastHistoryVersion conn
  runReaderT (unBacktest m) (mkEnv conn version params)


alpha :: Strategy a
alpha = mkStrategy $ sortBy (\_ _ -> EQ)

query :: (MonadIO m,  MonadReader Env m) => Day -> m [Asset]
query d = do
  conn <- view connection
  v <- view historyVersion
  ts <- liftIO $ Q.runMembersQuery conn v d
  return $ map mkEquity ts

constraints = mkConstraints [] [] []

d = fromGregorian 2016 1 26

go :: (MonadIO m, MonadReader Env m) => m PortfolioW
go = optimize query alpha constraints d

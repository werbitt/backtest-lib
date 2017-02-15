{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backtest.Dates
       (
         tradingDays
       , rebalanceDays
       ) where

import qualified Backtest.Query              as Q
import           Backtest.Types              (CanDb, Frequency,
                                              HasBacktestConfig, Ordinal,
                                              Weekday, ordToInt, ordinal,
                                              startDate, wait, weekday,
                                              weekdayToInt)
import           Control.Lens                (view, (^.))
import           Data.List                   (groupBy)
import           Data.Maybe                  (listToMaybe, mapMaybe)
import           Data.Set                    (Set, fromList)
import           Data.Time                   (Day, fromGregorian, toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)

tradingDays :: (CanDb r m, HasBacktestConfig r) => m [Day]
tradingDays =  Q.tradingDays =<< view startDate

rebalanceDays :: Frequency -> [Day] -> Set Day
rebalanceDays f ds = fromList $ mapMaybe getDay (groupBy sameMonth ds)
  where
    getDay []       = Nothing
    getDay ds'@(d:_) =  (listToMaybe . delay (f ^. wait) . after (nthDay' d)) ds'
    nthDay' d = nthDay (f ^. ordinal) (f ^. weekday) (year d) (month d)

sameMonth :: Day -> Day -> Bool
sameMonth d1 d2 = let (y1, m1, _) = toGregorian d1
                      (y2, m2, _) = toGregorian d2
                  in y1 == y2 && m1 == m2

nthDay :: Ordinal
       -> Weekday
       -> Integer    -- Year
       -> Int        -- Month
       -> Day
nthDay ord day y m = fromGregorian y m $ getFirst + (ord' - 1) * 7
  where
    ord' = ordToInt ord
    day' = weekdayToInt day
    (_, _, startDay) = toWeekDate $ fromGregorian y m 1
    getFirst  = if startDay < day' + 1
                 then day' + 1  - startDay
                 else 8 + day' - startDay

after :: Day -> [Day] -> [Day]
after d = dropWhile (< d)

delay :: Int -> [Day] -> [Day]
delay = drop

month :: Day -> Int
month d = m
  where
     (_, m, _) = toGregorian d

year :: Day -> Integer
year d = y
  where
   (y, _, _) = toGregorian d

module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import           Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq)

nextDay :: Day -> Day
nextDay day = case day of
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

afterDays :: Natural -> Day -> Day
afterDays n day
  | n == 0 = day
  | n > 7 = afterDays (mod n 7) day
  | otherwise = afterDays (n-1) (nextDay day)

isWeekend :: Day -> Bool
isWeekend day = (day == Saturday || day == Sunday)

daysToParty :: Day -> Natural
daysToParty day = case day of
  Friday -> 0
  x      -> daysToParty (nextDay x) + 1

module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day (ModifiedJulianDay, toModifiedJulianDay), addDays, fromGregorian)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Show, Eq, Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

type MonthOfYear = Int
type Year = Integer

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay First weekday year month = let weekMap = weekDayPair $ getNthDaysOfMonth 1 year month
                                      in maybe (ModifiedJulianDay 0) id (lookup weekday weekMap)

meetupDay Second weekday year month = let weekMap = weekDayPair $ getNthDaysOfMonth 2 year month
                                      in maybe (ModifiedJulianDay 0) id (lookup weekday weekMap)

meetupDay Third weekday year month = let weekMap = weekDayPair $ getNthDaysOfMonth 3 year month
                                      in maybe (ModifiedJulianDay 0) id (lookup weekday weekMap)

meetupDay Fourth weekday year month = let weekMap = weekDayPair $ getNthDaysOfMonth 4 year month
                                      in maybe (ModifiedJulianDay 0) id (lookup weekday weekMap)

meetupDay Last weekday year month = let weekMap = weekDayPair $ lastWeek year month
                                      in maybe (ModifiedJulianDay 0) id (lookup weekday weekMap)                                       

meetupDay Teenth weekday year month = let weekMap = weekDayPair $ teenthDays year month
                                      in maybe (ModifiedJulianDay 0) id (lookup weekday weekMap)                                                                                                                                                      


getFirstDayOfMonth :: Year -> MonthOfYear -> Day
getFirstDayOfMonth year month = fromGregorian year month 1

getNthDaysOfMonth :: Integer -> Year -> MonthOfYear -> [Day]
getNthDaysOfMonth 0 _ _ = []
getNthDaysOfMonth n y m = let d1 = getFirstDayOfMonth y m 
                              ds = [(7*(n-1)) `addDays` d1  .. (7+7*(n-1)) `addDays` d1]
                           in ds


isLeapYear :: Integer -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4   == 0 = True
    | otherwise        = False


weekDayPair :: Functor f => f Day -> f (Weekday, Day)
weekDayPair w = (\d -> (getWeekday d, d)) <$> w
        where getWeekday :: Day -> Weekday
              getWeekday d = toEnum . fromInteger $ mod (toModifiedJulianDay d + 2) 7 

getDaysOfMonth :: Year -> MonthOfYear -> [Day]
getDaysOfMonth y 2 | isLeapYear y = let d1 = getFirstDayOfMonth y 2
                                     in [d1 .. 28 `addDays` d1]
                   | otherwise = let d1 = getFirstDayOfMonth y 2
                                  in [d1 .. 27 `addDays` d1]

getDaysOfMonth y m | m `elem` [9,4,6,11] = [d .. 29 `addDays` d]
                   | otherwise = [d .. 30 `addDays` d]
                        where d = getFirstDayOfMonth y m

teenthDays :: Year -> MonthOfYear -> [Day]
teenthDays y m = take 7 . drop 12 $ monthDays
                where monthDays = getDaysOfMonth y m

lastWeek :: Year -> MonthOfYear -> [Day]
lastWeek y m = take 7 . reverse $ monthDays
                where monthDays = getDaysOfMonth y m 

module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString  String
                  | DbNumber Integer
                  | DbDate    UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime
                        (fromGregorian 1911 5 1)
                        (secondsToDiffTime 34123))
                , DbNumber 9001
                , DbString "Hello, world!"
                , DbDate (UTCTime
                         (fromGregorian 1921 5 1)
                         (secondsToDiffTime 34123))
               ]

-- 1

filterDbDate :: [DatabaseItem] -> [UTCTime]

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _          = False

getDbDateFromDatabaseItem :: DatabaseItem -> UTCTime
getDbDateFromDatabaseItem (DbDate x) = x

filterDbDateWithMap xs = map getDbDateFromDatabaseItem $ filter isDbDate xs

filterDbDate xs = foldr (\x y -> if isDbDate x then [getDbDateFromDatabaseItem x] ++ y else y) [] xs


-- 2

filterDbNumber :: [DatabaseItem] -> [Integer]

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _          = False

getDbNumberFromDatabaseItem :: DatabaseItem -> Integer
getDbNumberFromDatabaseItem (DbNumber x) = x

filterDbNumber xs = foldr (\x y -> if isDbNumber x then [getDbNumberFromDatabaseItem x] ++ y else y) [] xs

-- 3 

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = maximum $ filterDbDate xs

-- 4

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (\x y -> if isDbNumber x then getDbNumberFromDatabaseItem x + y else y) 0 xs

-- 5

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromInteger (sumDb xs) / fromIntegral (length $ filterDbNumber xs)

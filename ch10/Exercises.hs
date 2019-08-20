module Exercises where

import Data.Time
import Data.Bool

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbNumber 4
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f (DbDate x) xs = x:xs
    f _ xs = xs


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where
    f :: DatabaseItem -> [Integer] -> [Integer]
    f (DbNumber x) xs = x:xs
    f _ xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = total / count
  where
    total = fromIntegral . sumDb $ xs
    count = fromIntegral . length . filterDbNumber $ xs

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

stopVowelStops :: [(Char, Char, Char)]
stopVowelStops = [(s, v, s') | s <- stops, v <- vowels, s' <- stops]

stopVowelStopsB :: [(Char, Char, Char)]
stopVowelStopsB = [(s, v, s') | s <- stops, s=='p', v <- vowels, s' <- stops]

-- get average length of a word (integral result)
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x)))
                    (length (words x))


avgWordLength :: String -> Double
avgWordLength x = (fromIntegral . sum . map length . words $ x)
                  / (fromIntegral . length . words $ x)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> x == a || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> bool b (a:b) (f a)) []

squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> bool b a (f a b == GT)) (last xs) xs


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> bool b a (f a b == LT)) (last xs) xs

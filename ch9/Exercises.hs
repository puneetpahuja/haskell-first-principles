module Exercises where

import           Data.Bool

eft :: Enum a => a -> a -> [a]
eft from to
  | (fromEnum from) > (fromEnum to) = []
  | (fromEnum to) == (fromEnum from) = [from]
  | otherwise = from : (eft (succ from) to)

tokenize :: Eq a => a -> [a] -> [[a]]
tokenize _ [] = []
tokenize separator xs = token : tokenize separator restXs
  where
    token = takeWhile (/= separator) xs
    restXs = dropWhile (== separator) . dropWhile (/= separator) $ xs

myWords :: String -> [String]
myWords = tokenize ' '

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
\ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = tokenize '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

test :: Bool
test = myLines sentences == shouldEqual

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = (x == y) || myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []       = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x]      = x
myMaximumBy f (x:y:xs) = myMaximumBy f $ bool y x (f x y == GT) : xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x]      = x
myMinimumBy f (x:y:xs) = myMinimumBy f $ bool y x (f x y == LT) : xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

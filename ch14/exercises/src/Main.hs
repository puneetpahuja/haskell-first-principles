module Main where

import           Data.Char       (toUpper)
import           Data.List       (sort)
import           Test.QuickCheck (Arbitrary (..), elements, frequency,
                                  quickCheck, verboseCheck)

main :: IO ()
main = runQc

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = x == halfIdentity x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, t)       = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered = listOrdered . sort

prop_plusAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: (Integral a, Eq a) => a -> a -> Bool
prop_plusCommutative x y = x + y == y + x

prop_exponentiationAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
prop_exponentiationAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_exponentiationCommutative :: (Integral a, Eq a) => a -> a -> Bool
prop_exponentiationCommutative x y = x ^ y == y ^ x

prop_multiplyAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
prop_multiplyAssociative x y z = x * (y * z) == (x * y) * z

prop_multiplyCommutative :: (Integral a, Eq a) => a -> a -> Bool
prop_multiplyCommutative x y = x * y == y * x

prop_quotRem :: (Integral a, Eq a) => a -> a -> Bool
prop_quotRem _ 0 = True
prop_quotRem x y = (quot x y) * y + (rem x y) == x

prop_divMod :: (Integral a, Eq a) => a -> a -> Bool
prop_divMod _ 0 = True
prop_divMod x y = (div x y) * y + (mod x y) == x

prop_doubleReverseIdentity :: (Eq a) => [a] -> Bool
prop_doubleReverseIdentity x = (reverse . reverse $ x) == x

prop_dollar :: (Eq b) => (a -> b) -> a -> Bool
prop_dollar f x = (f $ x) == f x

prop_compose :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_compose f g x = (f . g $ x) == f (g x)

prop_foldr1 :: (Eq a) => [a] -> [a] -> Bool
prop_foldr1 xs ys = foldr (:) xs ys == (++) xs ys

prop_foldr2 :: (Eq a) => [[a]] -> Bool
prop_foldr2 xss = foldr (++) [] xss == concat xss

prop_take :: Int -> [a] -> Bool
prop_take n xs = length (take n xs) == n

prop_readShow :: (Show a, Read a, Eq a) => a -> Bool
prop_readShow x = (read (show x)) == x

square :: Num a => a -> a
square x = x * x

squareIdentity :: Floating a => a -> a
squareIdentity = square . sqrt

prop_squareIdentity :: (Eq a, Floating a) => a -> Bool
prop_squareIdentity x = squareIdentity x == x

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord = map toUpper

prop_idempotenceCapitalizeWord :: String -> Bool
prop_idempotenceCapitalizeWord x =
  (capitalizeWord x == twice capitalizeWord x) &&
  (capitalizeWord x == fourTimes capitalizeWord x)

prop_idempotenceSort :: (Eq a, Ord a) => [a] -> Bool
prop_idempotenceSort x =
  (sort x == twice sort x) && (sort x == fourTimes sort x)

runQc :: IO ()
runQc = do
  quickCheck (prop_halfIdentity :: Double -> Bool)
  quickCheck (prop_listOrdered :: [Double] -> Bool)
  quickCheck (prop_plusAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_plusCommutative :: Integer -> Integer -> Bool)
  quickCheck (prop_multiplyAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_multiplyCommutative :: Integer -> Integer -> Bool)
  quickCheck (prop_quotRem :: Integer -> Integer -> Bool)
  quickCheck (prop_divMod :: Integer -> Integer -> Bool)
  -- quickCheck
  --   (prop_exponentiationAssociative :: Integer -> Integer -> Integer -> Bool)
  -- quickCheck (prop_exponentiationCommutative :: Integer -> Integer -> Bool)
  quickCheck (prop_doubleReverseIdentity :: [Int] -> Bool)
  quickCheck (prop_dollar (^ 2) :: Int -> Bool)
  quickCheck (prop_compose (+ 1) (* 10) :: Int -> Bool)
  -- verboseCheck (prop_foldr1 :: [Int] -> [Int] -> Bool)
  quickCheck (prop_foldr1 :: [Int] -> [Int] -> Bool)
  quickCheck (prop_foldr2 :: [[Int]] -> Bool)
  quickCheck (prop_take :: Int -> [Int] -> Bool)
  quickCheck (prop_readShow :: String -> Bool)
  verboseCheck (prop_squareIdentity :: Double -> Bool)
  quickCheck (prop_idempotenceSort :: [Int] -> Bool)
  quickCheck (prop_idempotenceCapitalizeWord)

-- Make gen
data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = elements [Fulse, Frue]

data Fool'
  = Fulse'
  | Frue'
  deriving (Eq, Show)

instance Arbitrary Fool' where
  arbitrary = frequency [(2, return $ Fulse'), (1, return $ Frue')]
-- sample (arbitrary :: Gen Fool)
-- sample' (arbitrary :: Gen Fool')

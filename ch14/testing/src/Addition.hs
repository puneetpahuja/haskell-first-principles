module Addition where

import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main =
  hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ do (1 + 1) > 1 `shouldBe` True
      it "2 + 2 is equal to 4" $ do 2 + 2 `shouldBe` 4
      it "15 divided by 3 is 5" $ do dividedBy 15 3 `shouldBe` (5, 0)
      it
        "22 divided by 5 is\
      \ 4 remainder 2" $ do dividedBy 22 5 `shouldBe` (4, 2)
      it "4 multiplied by 6 is 24" $ do mult 4 6 `shouldBe` 24
      it "5 multiplied by 0 is 0" $ do mult 5 0 `shouldBe` 0
      it "x + 1 is always greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

mult :: (Eq a, Num a) => a -> a -> a
mult 0 _ = 0
mult 1 x = x
mult n x = x + mult (n - 1) x

trivialInt :: Gen Int
trivialInt = return 1

-- sample trivialInt
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

-- sample' oneThroughThree
oneThroughThree' :: Gen Int
oneThroughThree' = elements $ replicate 4 2 ++ [1, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

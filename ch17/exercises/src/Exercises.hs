module Exercises where

import Control.Applicative (liftA3)
import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck (arbitrary, frequency)
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative)

-- Lookups
added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x' :: Maybe Int
x' = elemIndex 3 [1 .. 5]

y' :: Maybe Int
y' = elemIndex 4 [1 .. 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

xs = [1 .. 3]

ys = [4 .. 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

testLookups :: IO ()
testLookups = do
  print $ added == Just 9
  print $ y == Just 6
  print $ z == Just 5
  print $ tupled == Just (6, 5)
  print $ x' == Just 2
  print $ y' == Just 3
  print $ maxed == Just 3
  print $ x'' == Just 6
  print $ y'' == Just 5
  print $ summed == Just 5

-- Identity Instance
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

testIdentity :: IO ()
testIdentity = do
  print $ pure 4 == Identity 4
  print $ ((+) <$> Identity 4 <*> Identity 6) == Identity 10

-- Constant Instance
newtype Constant a b =
  Constant
    { getConstant :: a
    }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = (Constant x)

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant (x <> y)

testConstant :: IO ()
testConstant = do
  let f = Constant (Sum 1)
      g = Constant (Sum 2)
  print $ (f <*> g) == Constant (Sum 3)
  print $ (pure 1 :: Constant String Int) == Constant ""

-- Fixer Upper
testFixerUpper :: IO ()
testFixerUpper = do
  print $ (const <$> Just "Hello" <*> pure "World") == Just "Hello"
  print $
    ((,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1 .. 3]) ==
    Just (90, 10, "Tierness", [1 .. 3])

-- List Applicative
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> xs = xs
  Cons x xs <> ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return $ Cons x Nil), (1, return Nil)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

testList :: IO ()
testList = do
  quickBatch $ applicative (Cons (1 :: Int, "ab", 'a') Nil)

-- ZipList Applicative
newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take 3000 l
      ys' =
        let (ZipList' l) = ys
         in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat x
  (ZipList' fs) <*> (ZipList' xs) = ZipList' . zipWith ($) fs $ xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

testZipList' :: IO ()
testZipList' = do
  let z = ZipList' [(+ 9), (* 2), (+ 8)]
      z' = ZipList' [1, 2, 3]
  print $ (z <*> z') == ZipList' [10, 4, 11]
  let z1' = pure 1
  print $ (z <*> z1') == ZipList' [10, 2, 9]
  let z2' = ZipList' [1, 2]
  print $ (pure id <*> z2') == ZipList' [1, 2]
  quickBatch $ applicative (ZipList' [(1 :: Int, "ab", 'a')])

-- Variations on either
data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure err) = Failure err
  fmap f (Success val) = Success (f val)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Failure err <*> Failure err' = Failure $ err <> err'
  Failure err <*> _ = Failure err
  Success f <*> x = fmap f x

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency [(1, return . Failure $ e), (3, return . Success $ a)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

testValidation :: IO ()
testValidation =
  quickBatch $
  applicative
    (Success (1 :: Sum Int, "ab", 4 :: Product Int) :: Validation [String] ( Sum Int
                                                                           , String
                                                                           , Product Int))

-- Chapter Exercises
-- 1. type is []
--    pure :: a -> [a]
--    (<*>) :: [a -> b] -> [a] -> [b]
-- 2. type is IO
--    pure :: a -> IO a
--    (<*>) :: IO (a -> b) -> IO a -> IO b
-- 3. type is (,) z
--    pure :: a -> (z, a)
--    (<*>) :: (z, a -> b) -> (z, a) -> (z, b)
-- 4. type is (->) e
--    pure :: a -> (e -> a)
--    (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  Pair f1 f2 <*> Pair x1 x2 = Pair (f1 x1) (f2 x2)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two a f <*> Two a' x = Two (a <> a') (f x)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' x = Three (a <> a') (b <> b') (f x)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y1 y2) = Three' x (f y1) (f y2)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f1 f2 <*> Three' a' x1 x2 = Three' (a <> a') (f1 x1) (f2 x2)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z q) = Four x y z (f q)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' x = Four (a <> a') (b <> b') (c <> c') (f x)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x1 x2 x3 y) = Four' x1 x2 x3 (f y)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a1 a2 a3 f <*> Four' a1' a2' a3' x =
    Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f x)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    b3 <- arbitrary
    return $ Four' a b1 b2 b3

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple = (,,)

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 makeTriple

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

stopsVowelsStops :: [(Char, Char, Char)]
stopsVowelsStops = combos stops vowels stops

main :: IO ()
main = do
  let dummy = ("abc", "abc", "abc")
  quickBatch $ applicative (Pair dummy dummy)
  quickBatch $ applicative (Two dummy dummy)
  quickBatch $ applicative (Three dummy dummy dummy)
  quickBatch $ applicative (Three' dummy dummy dummy)
  quickBatch $ applicative (Four dummy dummy dummy dummy)
  quickBatch $ applicative (Four' dummy dummy dummy dummy)

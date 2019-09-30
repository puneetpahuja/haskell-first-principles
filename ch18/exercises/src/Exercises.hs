module Exercises where

import           Control.Applicative       (liftA3)
import           Control.Monad             (join)
import           Test.QuickCheck           (arbitrary, frequency)
import           Test.QuickCheck.Arbitrary (Arbitrary)
import           Test.QuickCheck.Checkers  (EqProp, eq, quickBatch, (=-=))
import           Test.QuickCheck.Classes   (applicative, functor, monad)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

testBind :: IO ()
testBind = print $ bind (\x -> [x, 1]) [4, 5, 6] == [4, 1, 5, 1, 6, 1]

experiment :: IO ()
experiment = do
  let x = putStrLn <$> getLine
  y <- x
  y

experiment' :: IO ()
experiment' = do
  x <- putStrLn <$> getLine
  x

twiceWhenEven :: [Int] -> [Int]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

f :: Int -> Maybe Int
f 0 = Nothing
f n = Just n

g :: Int -> Maybe Int
g i =
  if even i
    then Just (i + 1)
    else Nothing

h :: Int -> Maybe String
h i = Just ("10191" ++ show i)

doSomething :: Int -> Maybe (Int, Int, String)
doSomething n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

doSomethingA :: Int -> Maybe (Int, Maybe Int, Maybe (Maybe String))
doSomethingA n = liftA3 (,,) (f n) (g <$> (f n)) ((fmap . fmap) h (g <$> (f n)))

-- Either Monad
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First e)  = First e
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  First e <*> _ = First e
  Second f <*> x = fmap f x

instance Monad (Sum a) where
  Second x >>= f = f x
  First e >>= _ = First e

testListMonad :: IO ()
testListMonad = quickBatch (monad [(1, 2, 3) :: (Int, Int, Int)])

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
     in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

testBadMonad :: IO ()
testBadMonad = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

type No = Nope (String, String, String)

data BahEither b a
  = PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a)  = PLeft (f a)

instance Applicative (BahEither b) where
  pure = PLeft
  PRight b <*> _ = PRight b
  PLeft f <*> x = fmap f x

instance Monad (BahEither b) where
  PRight b >>= _ = PRight b
  PLeft a >>= f = f a

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = frequency [(1, PRight <$> arbitrary), (2, PLeft <$> arbitrary)]

type Bah = BahEither String (String, String, String)

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> fx = fmap f fx

instance Monad Identity where
  Identity a >>= f = f a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type Id = Identity (String, String, String)

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> x = x
  Cons a l <> x = Cons a (l <> x)

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  (Cons f l) <*> fx = fmap f fx <> (l <*> fx)

instance Monad List where
  Nil >>= f = Nil
  Cons x l >>= f = f x <> (l >>= f)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = (flip Cons Nil) <$> arbitrary

type Li = List (String, String, String)

j :: Monad m => m (m a) -> m a
j mmx = mmx >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m_a m_b = do
  bToC <- f <$> m_a
  bToC <$> m_b

a :: Monad m => m a -> m (a -> b) -> m b
a m_a m_f = do
  f <- m_f
  f <$> m_a

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (a:as) f = do
  b <- f a
  bs <- meh as f
  pure (b : bs)

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id

main :: IO ()
main = do
  quickBatch $ functor (undefined :: No)
  quickBatch $ applicative (undefined :: No)
  quickBatch $ monad (undefined :: No)
  quickBatch $ functor (undefined :: Bah)
  quickBatch $ applicative (undefined :: Bah)
  quickBatch $ monad (undefined :: Bah)
  quickBatch $ functor (undefined :: Id)
  quickBatch $ applicative (undefined :: Id)
  quickBatch $ monad (undefined :: Id)
  quickBatch $ functor (undefined :: Li)
  quickBatch $ applicative (undefined :: Li)
  quickBatch $ monad (undefined :: Li)
  print $ j [[1, 2], [], [3]] == [1, 2, 3]
  print $ j (Just (Just 1)) == Just 1
  print $ j (Just Nothing :: Maybe (Maybe Int)) == Nothing
  print $ j (Nothing :: Maybe (Maybe Int)) == Nothing

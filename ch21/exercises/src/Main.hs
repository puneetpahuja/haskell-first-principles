{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Test.QuickCheck          (Arbitrary, Property, Testable,
                                           arbitrary, frequency)
import           Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import           Test.QuickCheck.Classes  (traversable)

type TI = []

newtype I a =
  I a
  deriving (Eq, Ord, Show)

instance Functor I where
  fmap f (I a) = I (f a)

instance Foldable I where
  foldMap f (I a) = f a

instance Traversable I where
  traverse fn (I a) = I <$> fn a

instance Arbitrary a => Arbitrary (I a) where
  arbitrary = I <$> arbitrary

instance Eq a => EqProp (I a) where
  (=-=) = eq

newtype C a b =
  C
    { getC :: a
    }
  deriving (Eq, Ord, Show)

instance Functor (C a) where
  fmap _ (C a) = C a

instance Foldable (C a) where
  foldMap _ (C _) = mempty

instance Traversable (C a) where
  traverse _ (C a) = pure $ C a

instance Arbitrary a => Arbitrary (C a b) where
  arbitrary = C <$> arbitrary

instance Eq a => EqProp (C a b) where
  (=-=) = eq

data O a
  = N
  | Y a
  deriving (Eq, Ord, Show)

instance Functor O where
  fmap f (Y a) = Y (f a)
  fmap _ N     = N

instance Foldable O where
  foldMap f (Y a) = f a
  foldMap _ N     = mempty

instance Traversable O where
  traverse fn (Y a) = Y <$> fn a
  traverse _ N      = pure N

instance Arbitrary a => Arbitrary (O a) where
  arbitrary = frequency [(1, return N), (3, Y <$> arbitrary)]

instance Eq a => EqProp (O a) where
  (=-=) = eq

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap f (Cons a as) = Cons (f a) (fmap f as)
  fmap _ Nil         = Nil

instance Foldable List where
  foldMap f (Cons a as) = f a `mappend` foldMap f as
  foldMap _ Nil         = mempty

instance Traversable List where
  traverse fn (Cons a as) = Cons <$> fn a <*> traverse fn as
  traverse _ Nil          = pure Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [(1, return Nil), (10, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c =
  Three a b c
  deriving (Eq, Show, Ord)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse fn (Three a b c) = Three a b <$> fn c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Pair a b =
  Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse fn (Pair a b) = Pair a <$> fn b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

data Big a b =
  Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b `mappend` f b'

instance Traversable (Big a) where
  traverse fn (Big a b b') = Big a <$> fn b <*> fn b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

data Bigger a b =
  Bigger a b b b
  deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b `mappend` f b' `mappend` f b''

instance Traversable (Bigger a) where
  traverse fn (Bigger a b b' b'') = Bigger a <$> fn b <*> fn b' <*> fn b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

instance Functor Tree where
  fmap _ Empty          = Empty
  fmap f (Leaf a)       = Leaf $ f a
  fmap f (Node lt a rt) = Node (fmap f lt) (f a) (fmap f rt)

instance Foldable Tree where
  foldMap _ Empty          = mempty
  foldMap f (Leaf a)       = f a
  foldMap f (Node lt a rt) = foldMap f lt `mappend` f a `mappend` foldMap f rt

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse fn (Leaf a) = Leaf <$> fn a
  traverse fn (Node lt a rt) =
    Node <$> traverse fn lt <*> fn a <*> traverse fn rt

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (10, return Empty)
      , (100, Leaf <$> arbitrary)
      , (3, Node <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

data S n a =
  S (n a) a
  deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) =>
         EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S n_a a) = S (fmap f n_a) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n_a a) = foldMap f n_a `mappend` f a

instance Traversable n => Traversable (S n) where
  traverse fn (S n_a a) = S <$> traverse fn n_a <*> fn a

main :: IO ()
main = do
  quickBatch (traversable (undefined :: TI (Int, Int, [Int])))
  quickBatch (traversable (undefined :: I (Int, Int, [Int])))
  quickBatch (traversable (undefined :: C Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: O (Int, Int, [Int])))
  quickBatch (traversable (undefined :: List (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Three Int Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Pair Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Big Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Bigger Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Tree (Int, Int, [Int])))
  quickBatch (traversable (undefined :: S [] (Int, Int, [Int])))
  -- print =<< sample' (arbitrary :: Gen (S [] Int))

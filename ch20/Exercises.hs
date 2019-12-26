module Exercises where

import           Data.Bool   (bool)
import           Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\a b -> b || (a == x)) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (\a b -> Just $ maybe a (min a) b) Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\a b -> max (Just a) b) Nothing

null' :: (Foldable t) => t a -> Bool
null' = foldr (const . const $ False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ n -> n + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a `mappend` b) mempty

data Constant a b =
  Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f b (Constant x) = f x b

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f b (Two _ x) = f x b

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f b (Three _ _ x) = f x b

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f b (Three' _ x x') = f x $ f x' b

data Four' a b =
  Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr f b (Four' _ x x' x'') = foldr f b [x, x', x'']

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> bool mempty (pure a) (f a))
-- filterF' ::
--      (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
-- filterF' f as = foldMap pure $ filter f (toList' as)

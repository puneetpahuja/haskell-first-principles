{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Control.Applicative (Applicative (..))

newtype Identity a =
  Identity
    { runIdentity :: a
    }
  deriving (Eq, Show)

newtype Compose f g a =
  Compose
    { getCompose :: f (g a)
    }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose f <*> Compose a = Compose (liftA2 (<*>) f a)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative f' => (a -> f' b) -> Compose f g a -> f' (Compose f g b)
  traverse fn (Compose fga) = Compose <$> traverse (traverse fn) fga

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b =
  Deux a b

instance Bifunctor Deux where
  bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap f g (Deux a c) = Deux (f a) (g c)

data Const a b =
  Const a

instance Bifunctor Const where
  bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
  bimap f _ (Const a) = Const (f a)

data Drei a b c =
  Drei a b c

instance Bifunctor (Drei a') where
  bimap :: (a -> b) -> (c -> d) -> Drei a' a c -> Drei a' b d
  bimap f g (Drei a' a c) = Drei a' (f a) (g c)

data SuperDrei a b c =
  SuperDrei a b

instance Bifunctor (SuperDrei a') where
  bimap :: (a -> b) -> (c -> d) -> SuperDrei a' a c -> SuperDrei a' b d
  bimap f _ (SuperDrei a' a) = SuperDrei a' (f a)

data SemiDrei a b c =
  SemiDrei a

instance Bifunctor (SemiDrei a') where
  bimap :: (a -> b) -> (c -> d) -> SemiDrei a' a c -> SemiDrei a' b d
  bimap _ _ (SemiDrei a') = SemiDrei a'

data Quadriceps a b c d =
  Quadzzz a b c d

instance Bifunctor (Quadriceps a' b') where
  bimap :: (a -> b) -> (c -> d) -> Quadriceps a' b' a c -> Quadriceps a' b' b d
  bimap f g (Quadzzz a' b' a c) = Quadzzz a' b' (f a) (g c)

data Either' a b
  = Left' a
  | Right' b

instance Bifunctor Either' where
  bimap :: (a -> b) -> (c -> d) -> Either' a c -> Either' b d
  bimap f _ (Left' a)  = Left' (f a)
  bimap _ g (Right' c) = Right' (g c)

main :: IO ()
main = do
  putStrLn "hello world"

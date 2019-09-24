module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Zippy a =
  Zippy (ZipList a)
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Zippy a) where
  (Zippy (ZipList xs)) <> (Zippy (ZipList ys)) =
    Zippy . ZipList $ zipWith (<>) xs ys

instance Monoid a => Monoid (Zippy a) where
  mempty = Zippy $ pure mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Zippy a) where
  arbitrary = do
    x <- arbitrary
    return (Zippy . ZipList $ x)

instance Eq a => EqProp (Zippy a) where
  (=-=) = eq

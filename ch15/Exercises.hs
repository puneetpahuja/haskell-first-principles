module Exercises where

import           Data.Monoid

data Booly
  = False'
  | True'

instance Semigroup Booly where
  (<>) True' True' = True'
  (<>) _ _         = False'

instance Monoid Booly where
  mempty = True'

-- Optional Monoid
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only $ x <> y
  (<>) Nada x            = x
  (<>) x Nada            = x

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

testOptional :: IO ()
testOptional = do
  print $ Only (Sum 1) `mappend` Only (Sum 1)
  print $ Only (Product 4) `mappend` Only (Product 2)
  print $ Only (Sum 1) `mappend` Nada
  print $ Only [1] `mappend` Nada
  print $ Nada `mappend` Only (Sum 1)

-- mad lib
type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove of with his "
    , adj
    , " wife."
    ]

-- infix names for function args
asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

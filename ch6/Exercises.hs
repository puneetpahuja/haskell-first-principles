module Exercises where

import Data.List (sort)

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  TisAn x == TisAn y = x == y

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  Two x y == Two x' y' = x == x' && y == y'

data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt x == TisAnInt x' = x == x'
  TisAString x == TisAString x' = x == x'
  _ == _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  Pair x y == Pair x' y' = x == x' && y == y'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple x y == Tuple x' y' = x == x' && y == y'

data Which a
  = ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne x == ThisOne x' = x == x'
  ThatOne x == ThatOne x' = x == x'
  _ == _ = False

data EitherOr a b
  = Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello x == Hello x' = x == x'
  Goodbye x == Goodbye x' = x == x'
  _ == _ = False

data Person =
  Person Bool
  deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood
  = Blah
  | Woot
  deriving (Show, Eq)

settleDown x =
  if x == Woot
    then Blah
    else x

type Subject = String

type Verb = String

type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
  Rocks String
  deriving (Eq, Show, Ord)

data Yeah =
  Yeah Bool
  deriving (Eq, Show, Ord)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show, Ord)

phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

i :: (Num a) => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Int -> Int
freud x = x

myX = 1 :: Int

-- sigmund :: Num a => a -> a
sigmund x = myX

jung :: [Char] -> Char
jung xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n x = f x + fromInteger n

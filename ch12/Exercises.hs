module Exercises where

import           Data.Bool
import           Data.Char

-- Determine the kinds
-- 1. id :: a -> a
-- :k (->) :: * -> * -> *
-- so, kind of a has to be *
-- 2. r :: a -> f a
-- :k (->) :: * -> * -> *
-- so, kind of a and (f a) has to be *
-- kind of a is * and kind of f is (* -> *)
replaceThe :: String -> String
replaceThe sentence = unwords $ map theToA $ words sentence

theToA :: String -> String
theToA "the" = "a"
theToA s     = s

vowels :: String
vowels = "aeiouAEIOU"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fst . go . words
  where
    go = foldr f (0, False)
    f word (ans, nextWordStartWithVowel) =
      if word == "the" && nextWordStartWithVowel
        then (ans + 1, False)
        else (ans, head word `elem` vowels)

countVowels :: String -> Int
countVowels = length . filter isVowel

isVowel :: Char -> Bool
isVowel = flip elem vowels

isConsonant :: Char -> Bool
isConsonant c = isAlpha c && (not . isVowel $ c)

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w = bool Nothing (Just . Word' $ w) (numConsonants >= numVowels)
  where
    numConsonants = length . filter isConsonant $ w
    numVowels = countVowels w

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
integerToNat 0 = Just Zero
integerToNat x = fmap Succ $ integerToNat $ x - 1

-- small library for maybe
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes []           = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs)  = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
  where
    go _ Nothing          = Nothing
    go Nothing _          = Nothing
    go (Just x) (Just xs) = Just (x : xs)

-- small library for either
lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where
    go (Left a) as  = a : as
    go (Right _) as = as

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where
    go (Right b) bs = b : bs
    go (Left _) bs  = bs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where
    go (Left a) (as, bs)  = (a : as, bs)
    go (Right b) (as, bs) = (as, b : bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just . f $ b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)   = f a
either' _ f' (Right b) = f' b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- unfoldr
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Nothing       -> []
    Just (a', b') -> a' : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\y -> Just (y, f y))

-- something other that a list - binary tree
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Nothing            -> Leaf
    Just (a', b', a'') -> Node (unfold f a') b' (unfold f a'')

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (go n) n
  where
    go _ 0 = Nothing
    go t x = Just (x - 1, t - x, x - 1)

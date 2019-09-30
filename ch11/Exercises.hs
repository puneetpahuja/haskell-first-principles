{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Exercises where

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe

data Price =
  Price Integer
  deriving (Eq, Show)

data Size =
  Size Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 333)

urCar = Car Mazda (Price 444)

clownCar = Car Tata (Price 666)

doge = Plane PapuAir (Size 44)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

-- instance TooMany (Int, Int) where
--   tooMany (n1, n2) = tooMany $ n1 + n2
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n1, n2) = tooMany $ n1 + n2

-- Programmers
data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer
    { os :: OperatingSystem
    , lang :: ProgLang
    }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [Programmer {os, lang} | os <- allOperatingSystems, lang <- allLanguages]

-- the quad
-- 1. 4 + 4
-- 2. 4 * 4
-- 3. a -> b => b^a => 4^4 = 256
-- 4. 2 * 2 * 2
-- 5. a -> b -> c = a -> (b -> c) => (c ^ b) ^ a => c ^ (b * a) => 2 ^ (2 * 2) = 16
-- 6. a(2) -> b(4) -> c(4) => c ^ (b * a) = 4 ^ (4 * 2) = 65536
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = mapTree (+ 1) testTree' == mapExpected

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left a right) = [a] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left a right) = postOrder left ++ postOrder right ++ [a]

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left a right) = inOrder left ++ [a] ++ inOrder right

testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreOrder = preOrder testTree == [2, 1, 3]

testInOrder = inOrder testTree == [1, 2, 3]

testPostOrder = postOrder testTree == [1, 3, 2]

testTraversals = [testPreOrder, testInOrder, testPostOrder]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc t = foldr f acc (inOrder t)

-- Chapter exercises
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs'@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf xs' ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = zip wrds (map capitalizeWord wrds)
  where
    wrds = words sentence

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)
  | isSpace x = x : capitalizeWord xs
  | otherwise = toUpper x : xs

tokenize :: Eq a => a -> [a] -> [[a]]
tokenize _ [] = []
tokenize separator xs = token : tokenize separator restXs
  where
    token = takeWhile (/= separator) xs
    restXs = dropWhile (== separator) . dropWhile (/= separator) $ xs

sentences = tokenize '.'

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate "." . map capitalizeWord . sentences

-- Phone exercise
convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

type Digit = Char

type Presses = Int

data DaPhone =
  DaPhone [(Digit, String)]

phone :: DaPhone
phone =
  DaPhone
    [ ('2', "abc")
    , ('3', "def")
    , ('4', "ghi")
    , ('5', "jkl")
    , ('6', "mno")
    , ('7', "pqrs")
    , ('8', "tuv")
    , ('9', "wxyz")
    , ('*', "CAPS")
    , ('0', " ")
    ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) _ = [('$', 0)]
reverseTaps p@(DaPhone ((digit, cs):ds)) c
  | isUpper c = (findCaps p, 1) : reverseTaps p (toLower c)
  | digit == c = [(digit, length cs + 1)]
  | elem c cs = [(digit, (+ 1) . fromJust . elemIndex c $ cs)]
  | otherwise = reverseTaps (DaPhone ds) c

findCaps :: DaPhone -> Digit
findCaps (DaPhone []) = '$'
findCaps (DaPhone ((digit, "CAPS"):_)) = digit
findCaps (DaPhone (_:xs)) = findCaps (DaPhone xs)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\a b -> (snd a) + b) 0

mostPopular :: Eq a => a -> [a] -> a
mostPopular deflt [] = deflt
mostPopular deflt s@(x:xs) =
  bool mostPopularNonX x (count x s > count mostPopularNonX s)
  where
    mostPopularNonX = mostPopular deflt nonXs
    nonXs = filter (/= x) xs
    count y zs = length . filter (== y) $ zs

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular '$'

cost :: DaPhone -> Char -> Presses
cost p = fingerTaps . reverseTaps p

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopular "" . concatMap words

-- Hutton's Razor
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

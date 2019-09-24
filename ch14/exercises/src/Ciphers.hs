module Ciphers where

import Control.Monad (forever)
import Data.Bool (bool)
import Data.Char (chr, isUpper, ord, toLower)
import Debug.Trace (trace)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Test.QuickCheck
  ( Arbitrary
  , Gen
  , arbitrary
  , ioProperty
  , quickCheck
  , verboseCheck
  )

data ShiftDirection
  = L
  | R
  deriving (Eq, Show)

-- Data.Char.isAlpha is quite broad. It accepts unicode chars also which
-- makes the quickCheck properties wrong i.e decode (encode x) /= x
-- Making a restricted version
isAlpha' :: Char -> Bool
isAlpha' x = x `elem` ['a' .. 'z'] || x `elem` ['A' .. 'Z']

shift :: Int -> Char -> Char
shift by c
  | isAlpha' c =
    let pos = ord (toLower c) - ord 'a'
        newPos = (pos + by) `mod` 26
        base = ord . bool 'a' 'A' $ isUpper c
     in chr $ newPos + base
  | otherwise = c

vignere :: String -> String -> String
vignere keyword msg = shiftVignere R (concat . repeat $ keyword) msg

shiftVignere :: ShiftDirection -> String -> String -> String
shiftVignere _ _ [] = []
shiftVignere shiftDir (k:ks) (c:cs)
  | isAlpha' c =
    let shiftSign = bool (-1) 1 (shiftDir == R)
        shiftBy = shiftSign * (ord (toLower k) - ord 'a')
        c' = shift shiftBy c
     in c' : shiftVignere shiftDir ks cs
  | otherwise = c : shiftVignere shiftDir (k : ks) cs

unvignere :: String -> String -> String
unvignere keyword msg = shiftVignere L (concat . repeat $ keyword) msg

caesar :: Int -> String -> String
caesar = map . shift

uncaesar :: Int -> String -> String
uncaesar shiftBy = caesar (-shiftBy)

prop_vignere :: S -> S -> Bool
prop_vignere (S keyword) (S msg) =
  (unvignere keyword . vignere keyword $ msg) == msg
  -- (unvignere keyword . vignere (trace "keyword \n" keyword) $
  --  (trace "msg \n" msg)) ==
  -- msg

newtype S =
  S String
  deriving (Eq, Show)

instance Arbitrary S where
  arbitrary = do
    a <- arbitrary
    return $ S (trace "S " a)

prop_caesar :: Int -> String -> Bool
prop_caesar shiftBy msg = (uncaesar shiftBy . caesar shiftBy $ msg) == msg

main :: IO ()
main
  -- TODO: it gets stuck on some value but I dont know which one. find out a way to know that
  --       and then debug why is it happening like that. Debug the functions.
 = do
  hSetBuffering stdout NoBuffering
  verboseCheck prop_vignere
  quickCheck prop_caesar

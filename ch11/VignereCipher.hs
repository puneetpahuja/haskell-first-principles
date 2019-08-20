module VignereCipher where

import Data.Char
import Data.Bool

data ShiftDirection = L | R deriving (Eq, Show)

shift :: Int -> Char -> Char
shift by c
  | isAlpha c =
    let
      pos = ord (toLower c) - ord 'a'
      newPos = (pos + by) `mod` 26
      base = ord . bool 'a' 'A' $ isUpper c
    in chr $ newPos + base
  | otherwise = c

vignere :: String -> String -> String
vignere keyword msg = shiftVignere R (concat . repeat $ keyword) msg

shiftVignere :: ShiftDirection -> String -> String -> String
shiftVignere _ _ [] = []
shiftVignere shiftDir (k:ks) (c:cs)
  | isAlpha c =
    let
      shiftSign = bool (-1) 1 (shiftDir == R)
      shiftBy = shiftSign * (ord (toLower k) - ord 'a')
      c' = shift shiftBy c
    in c' : shiftVignere shiftDir ks cs
  | otherwise =
    c : shiftVignere shiftDir (k:ks) cs

unvignere :: String -> String -> String
unvignere keyword msg = shiftVignere L (concat . repeat $ keyword) msg

module CaesarCipher where

import Data.Bool
import Data.Char

shift :: Int -> Char -> Char
shift by c
  | isAlpha c =
    let pos = ord (toLower c) - ord 'a'
        newPos = (pos + by) `mod` 26
        base = ord . bool 'a' 'A' $ isUpper c
     in chr $ newPos + base
  | otherwise = c

caesar :: Int -> String -> String
caesar = map . shift

uncaesar :: Int -> String -> String
uncaesar shiftBy = caesar (-shiftBy)

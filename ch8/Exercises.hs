module Exercises where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n >=0 && n <= 9 = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! n
  | otherwise = "error"

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = (digits $ n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

-- exercises.hs
module Exercises where

-- # Scope
-- yes: 1, 4
area d = pi * (r * r)
  where
    r = d / 2

-- # Syntax Errors
x = (++) [1, 2, 3] [4, 5, 6]

y = "<3" ++ " Haskell"

z = concat ["<3", " Haskell"]

printSecond :: IO ()
printSecond = do
  putStrLn greeting

greeting :: String
greeting = "Yarrrrr"

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where
    greeting = "Yarrrrr"

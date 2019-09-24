module Main where

import DogsRule
import Hello
import System.IO

main :: IO ()
main
  -- hSetBuffering stdout NoBuffering
 = do
  putStr "Input name: "
  name <- getLine
  sayHello name
  dogs

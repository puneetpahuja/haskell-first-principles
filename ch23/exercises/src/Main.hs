module Main where

import           System.Random as R

newtype State s a =
  State
    { runState :: s -> (a, s)
    }

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

main :: IO ()
main = do
  putStrLn "hello world"

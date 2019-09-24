module Exercises where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

testBind :: IO ()
testBind = print $ bind (\x -> [x, 1]) [4, 5, 6] == [4, 1, 5, 1, 6, 1]

experiment :: IO ()
experiment = do
  let x = putStrLn <$> getLine
  y <- x
  y

experiment' :: IO ()
experiment' = do
  x <- putStrLn <$> getLine
  x

twiceWhenEven :: [Int] -> [Int]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

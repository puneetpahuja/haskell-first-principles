module Main
  ( main
  ) where

main :: IO ()
main = do
  putStrLn "hello world"

bot :: a
bot = undefined

wc :: a -> b -> a
wc x z =
  let y = bot `seq` 'y'
   in y `seq` x

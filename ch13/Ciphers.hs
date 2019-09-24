module Ciphers where

import Control.Monad (forever)
import Data.Bool (bool)
import Data.Char (chr, isAlpha, isUpper, ord, toLower)
import System.Exit (exitSuccess)

data ShiftDirection
  = L
  | R
  deriving (Eq, Show)

shift :: Int -> Char -> Char
shift by c
  | isAlpha c =
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
  | isAlpha c =
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

ask :: String -> IO String
ask msg = putStrLn msg >> getLine

processInput :: String -> IO ()
processInput "1" = do
  shiftBy <- ask "How much to shift? "
  msg <- ask "Message to encrypt? "
  putStrLn $ "Encrypted message: " ++ caesar (read shiftBy) msg
processInput "2" = do
  keyword <- ask "Keyword? "
  msg <- ask "Message to encrypt? "
  putStrLn $ "Encrypted message: " ++ vignere keyword msg
processInput "3" = exitSuccess
processInput _ = putStrLn "Wrong input. Try Again"

main :: IO ()
main =
  forever $ do
    putStrLn "Select an option:\n1. Caesar Cipher\n2. Vignere Cipher\n3. Exit."
    input <- getLine
    processInput input

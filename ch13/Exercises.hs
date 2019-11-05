module Exercises where

import           Control.Monad (forever)
import           Data.Char     (isAlpha, toLower)
import           System.Exit   (exitSuccess)

palindrome :: IO ()
palindrome =
  forever $ do
    line1 <- getLine
    case isPalindrome line1 of
      True -> putStrLn "It's a palindrome!"
      False -> do
        putStrLn "Nope!"
        exitSuccess

isPalindrome :: String -> Bool
isPalindrome s = s' == reverse s'
  where
    s' = map toLower . filter isAlpha $ s

type Name = String

type Age = Integer

data Person =
  Person Name Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

ask :: String -> IO String
ask msg = putStr msg >> getLine

gimmePerson :: IO ()
gimmePerson = do
  name <- ask "Name of person: "
  age <- ask "Age of person: "
  case mkPerson name (read age) of
    Right person -> putStrLn $ "Yay! Successfully got a person: " ++ show person
    Left err     -> putStrLn $ "Error occured: " ++ show err

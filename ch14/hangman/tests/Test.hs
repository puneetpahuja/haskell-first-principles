module Test where

import Main
import Test.Hspec

filledInSoFar :: [Maybe Char]
filledInSoFar =
  [Nothing, Just 'n', Nothing, Just 'e', Nothing, Just 'e', Nothing, Nothing]

filledS :: [Maybe Char]
filledS =
  [Nothing, Just 'n', Nothing, Just 'e', Nothing, Just 'e', Just 's', Nothing]

filledT :: [Maybe Char]
filledT =
  [Nothing, Just 'n', Just 't', Just 'e', Nothing, Just 'e', Nothing, Just 't']

toGuess :: String
toGuess = "interest"

puzzle :: Puzzle
puzzle = Puzzle toGuess filledInSoFar "hen"

main :: IO ()
main =
  hspec $ do
    describe "fillInCharacter" $ do
      it "correctGuessT" $ do
        fillInCharacter puzzle 't' `shouldBe` Puzzle toGuess filledT "then"
      it "correctGuessS" $ do
        fillInCharacter puzzle 's' `shouldBe` Puzzle toGuess filledS "shen"
      it "wrongGuess" $ do
        fillInCharacter puzzle 'z' `shouldBe`
          Puzzle toGuess filledInSoFar "zhen"
      it "repeatedGuess" $ do
        fillInCharacter puzzle 'e' `shouldBe`
          Puzzle toGuess filledInSoFar "ehen"
    describe "handleGuess" $ do
      it "correctGuessT" $ do
        res <- handleGuess puzzle 't'
        res `shouldBe` Puzzle toGuess filledT "then"
      it "correctGuessS" $ do
        res <- handleGuess puzzle 's'
        res `shouldBe` Puzzle toGuess filledS "shen"
      it "wrongGuess" $ do
        res <- handleGuess puzzle 'z'
        res `shouldBe` Puzzle toGuess filledInSoFar "zhen"
      it "repeatedGuess" $ do
        res <- handleGuess puzzle 'e'
        res `shouldBe` puzzle

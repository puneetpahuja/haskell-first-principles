{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = h
  where
    (xLast, _) = x `divMod` 100
    (_, h) = xLast `divMod` 10

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b =
  case b of
    True -> y
    False -> x

foldBoolGaurd :: a -> a -> Bool -> a
foldBoolGaurd x y b
  | b = y
  | otherwise = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show

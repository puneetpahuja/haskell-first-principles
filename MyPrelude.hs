module MyPrelude where

import Prelude
import Data.Function ((&))

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(f >.> g) x = g (f x)


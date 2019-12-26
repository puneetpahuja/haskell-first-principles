module MyPrelude where

import           Data.Function ((&))
import           Prelude

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) f g x = g (f x)

module Exercises where

import qualified Data.Map as M

keys c =
  M.fromList [(1, 'a'), (2, 'b')] `mappend` keys (const (M.fromList [(1, 'a')]))

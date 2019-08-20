module ImportTopOrLocal where

import TopOrLocal

--topLevelValue :: Integer
--topLevelValue = 6

-- should be 23 if it picks up topLevelValue from TopOrLocal, 24 if it
-- picks up from here
-- Ans: its 23
test :: Integer
test = topLevelFunction 8



-- ch2Exercises.hs

module Ch2Exercises where

-- ### Parentheses and association
-- same - 2
-- different - 1, 3

-- ### Heal the sick
-- 1. Remove space between . and 1 in 3.14
-- let area x = 3.14 * (x * x)

-- 2. variable b not in scope
-- let double x = x * 2

-- 3. fix indentation
x = 7
y = 10
f = x + y

-- ## A head code
-- 1. 5
-- 2. 25
-- 3. 30
-- 4. 6

x1 = x
  where x = 5

xSquare = x * x
  where x = 5

xy = x * y
  where x = 6
        y = 6

-- ch2Exercises.hs

module Ch2Exercises where

-- # Parentheses and association
-- same - 2
-- different - 1, 3

-- # Heal the sick
-- 1. Remove space between . and 1 in 3.14
-- let area x = 3.14 * (x * x)

-- 2. variable b not in scope
-- let double x = x * 2

-- 3. fix indentation
x = 7
y = 10
f = x + y

-- # A head code
-- 1. 5
-- 2. 25
-- 3. 30
-- 4. 6

x1 = x
  where x = 5

xSquare = x * x
  where x = 5

xy = x * y
  where x = 5
        y = 6

xPlus3 = x + 3
  where x = 3
        y = 1000

first = x * 3 + y
  where x = 3
        y = 1000

second = x * 5
  where y = 10
        x = 10 * 5 + y

third = z / x + y
  where x = 7
        y = negate x
        z = y * 10

-- # Parenthesization
eEx = 2 + 2 * 3 - 3             -- 5
eExS = 2 + (2 * 3) - 3          -- 5   -- `e` stands for expression, `S` stands for Solution

e1 = 2 + 2 * 3 - 1              -- 7
e1S = 2 + (2 * 3) - 1           -- 7

e2 = (^) 10 $ 1 + 1             -- 100
e2S = (^) 10 (1 + 1)            -- 100
e2S2 = (^) 10 $ (1 + 1)         -- 100

e3 = 2 ^ 2 * 4 ^ 5 + 1          -- 4097
e3S = ((2 ^ 2) * (4 ^ 5)) + 1   -- 4097

-- # Equivalent expressions
-- Answer - 1, 2
-- 4 - div is integral division

-- # More fun with functions
zz = 7
yy = zz + 8                     -- 15
xx = yy ^ 2                     -- 225
waxOn = xx * 5                  -- 1125

-- 1. 1135, 1135, -1110, 1110

-- 4.
waxOn2 = x * 5
  where z = 7
        x = y ^ 2
        y = z + 8

-- 5.
triple x = x * 3

-- 6.
waxOff x = triple x

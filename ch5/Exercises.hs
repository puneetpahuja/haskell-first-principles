{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises where

-- Type Arguments
{-
1. a
2. d
3. d
4. c
5. a
6. e
7. d
8. a
9. c
-}

-- Chapter Exercises
-- Multiple Choice
{-
1. c
2. a
3. b
4. c
-}

-- Determine the type
example = 1 -- (Num a) => a
a = (* 9) 6 -- (Num a) => a
b = head [(0, "doge"), (1, "kitteh")] -- (Num a) => (a, [Char])
c = head [(0 :: Integer, "doge"), (1, "kitteh")] -- (Integer, [Char])
d = if False then True else False -- Bool
e = length [1, 2, 3, 4, 5] -- Int
f = (length [1, 2, 3, 4]) > (length "TACOCAT") -- Bool

x = 5
y = x + 5
w = y * 10 -- (Num a) => a

z y = y * 10 -- (Num a) => a -> a

f' = 4 / y -- (Fractional a) => a

xx = "Julie"
yy = " <3 "
zz = "Haskell"
f'' = xx ++ yy ++ zz -- [Char]

bignum = (^) 5 $ 10
--wahoo = bignum $ 10   -- error
wahoo = bignum + 10

x' = print
y' = print "woohoo!"
z' = x' "hello world"

aa = (+)
bb = 5
--cc = bb 10 -- error
cc = aa 10 -- error
dd = cc 200

aaa = 12 + bbb
bbb = 10000 * ccc
ccc = 10

-- code.hs

module Code where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3
half x = x / 2
square x = x * x
circleArea r = pi * (square r)

x = 10 * 5 + y
myResult = x * 5
y = 10

foo x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 3
              in print plusTwo

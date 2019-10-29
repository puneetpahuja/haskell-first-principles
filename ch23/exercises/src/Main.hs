{-# LANGUAGE InstanceSigs #-}

module Main where

import           Control.Applicative        (liftA2, liftA3)
import           Control.Monad              (replicateM)
import           Control.Monad.State.Class  (get, put, state)
import           Control.Monad.State.Strict (State, execState)
import           Prelude                    hiding (sum)
import           System.Random              as R

-- newtype State s a =
--   State
--     { runState :: s -> (a, s)
--     }
data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = R.randomR (1, 6) s
      (d2, s2) = R.randomR (1, 6) s1
      (d3, _) = R.randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

-- using do without a monad context (you can skip the last `in`)
test :: Int
test = do
  let x = 1
      y = 2
  x + y

rollDie :: State StdGen Die
rollDie =
  state $ do
    (n, s) <- R.randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (R.randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (count, dies) gen
      | sum >= n = (count, dies)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) ((count + 1), dies ++ [intToDie die]) nextGen

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to = go from to []
  where
    go f t s
      | f <= t = go f (t - 1) $ execState (addResult t) s
      | otherwise = s

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

newtype Moi s a =
  Moi
    { runMoi :: s -> (a, s)
    }

-- TODO: verify functor, applicative, and monad instances (try using checkers)
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi
      (\s ->
         let (a, s') = g s
          in (f a, s'))

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  -- TODO: what to apply first - f or g
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi f <*> Moi g =
    Moi $ \s ->
      let (a2b, s') = f s
          (a, s'') = g s'
       in (a2b a, s'')

instance Monad (Moi s) where
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi f >>= g =
    Moi $ \s ->
      let (a, s') = f s
          Moi h = g a
       in h s'

get' :: Moi s s
get' = Moi (liftA2 (,) id id) -- or ((,) <$> id <*> id) -- or (\s -> (s, s))

put' :: s -> Moi s ()
put' s = Moi $ \_ -> ((), s)

exec' :: Moi s a -> s -> s
exec' (Moi s2as) s = snd . s2as $ s

eval' :: Moi s a -> s -> a
eval' (Moi s2as) s = fst . s2as $ s

modify' :: (s -> s) -> Moi s ()
modify' s2s = Moi $ (\s -> ((), s2s s))

main :: IO ()
main = do
  print $ runMoi get' "curryIs" == ("curryIs", "curryIs")
  print $ runMoi (put' "blah") "woot" == ((), "blah")
  print $ exec' (put' "wilma") "daphne" == "wilma"
  print $ exec' get' "scooby papu" == "scooby papu"
  print $ eval' get' "bunnicula" == "bunnicula"
  print $ eval' get' "stake a bunny" == "stake a bunny"
  print $ eval' (put' "wilma") "daphne" == ()
  print $ runMoi (modify' (+ 1)) 0 == ((), 1 :: Int)
  print $ runMoi (modify' (+ 1) >> modify' (+ 1)) 0 == ((), 2 :: Int)
  -- mapM_ putStrLn $ fizzBuzzFromTo 1 10

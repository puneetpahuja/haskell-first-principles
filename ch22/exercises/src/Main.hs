{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative (liftA2)
import           Control.Monad       (ap)
import           Data.Char           (toUpper)
import           Data.Maybe          (fromMaybe)

boop :: Num a => a -> a
boop = (* 2)

doop :: Num a => a -> a
doop = (+ 10)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Num a => a -> a
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

test :: Integer -> (Integer, Integer, Integer)
test = (,,) <$> boop <*> doop <*> boop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  a <- cap
  b <- rev
  return (a, b)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = cap >>= (\a -> rev >>= (\b -> return (a, b)))

newtype Reader r a =
  Reader
    { runReader :: r -> a
    }

instance Functor (Reader r) where
  fmap f (Reader r_a) = Reader $ f . r_a

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader r_ab <*> Reader r_a = Reader $ ap r_ab r_a

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader r_a >>= a2R_r_b =
    Reader $ \r ->
      let Reader r_b = a2R_r_b $ r_a r
       in r_b r

data Person =
  Person
    { humanName :: HumanName
    , dogName   :: DogName
    , address   :: Address
    }
  deriving (Eq, Show)

data Dog =
  Dog
    { dogsName    :: DogName
    , dogsAddress :: Address
    }
  deriving (Eq, Show)

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy

getDogRM' :: Person -> Dog
getDogRM' = dogName >>= (\name -> address >>= (\addy -> return $ Dog name addy))

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 fn f_a f_b = fn <$> f_a <*> f_b

asks :: (r -> a) -> Reader r a
asks = Reader

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \a b -> f b a

main :: IO ()
main = do
  print $ bip 3 == (26 :: Integer) -- (3 + 10) * 2
  print $ bloop 3 == (26 :: Integer) -- same as above
  print $ bbop 3 == 19 -- (boop 3) + (doop 3) = 6 + 13
  print $ duwop 3 == 19 -- same as above
  print $ boopDoop 3 == 19 -- same as above
  print $ test 3 == (6, 13, 6) -- (boop 3, doop 3, boop 3)
  print $ composed "Julie" == "EILUJ"
  print $ fmapped "Chris" == "SIRHC"
  print $ tupled "Julie" == ("JULIE", "eiluJ")
  print $ tupledM "Julie" == ("JULIE", "eiluJ")
  print $ tupledM' "Julie" == ("JULIE", "eiluJ")
  print $ tupled' "Julie" == ("eiluJ", "JULIE")

-- Chapter Exercises
x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

xs' :: Maybe Integer
xs' = lookup 3 $ zip x y

ys' :: Maybe Integer
ys' = lookup 6 $ zip y z

zs' :: Maybe Integer
zs' = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs' <*> ys'

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys' <*> zs'

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(> 3), (< 8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs' <*> ys')

main' :: IO ()
main' = do
  print $ xs' == Just 6
  print $ ys' == Just 9
  print $ zs' == Nothing
  print $ x1 == Just (6, 9)
  print $ x2 == Nothing
  print $ x3 3 == (Just 9, Just 9)
  print $ fromMaybe 0 xs' == 6
  print $ fromMaybe 0 zs' == 0
  print $ sequenceA [Just (3 :: Integer), Just 2, Just 1] == Just [3, 2, 1]
  print $
    sequenceA [x, y] ==
    [[1, 4], [1, 5], [1, 6], [2, 4], [2, 5], [2, 6], [3, 4], [3, 5], [3, 6]]
  print $ sequenceA [xs', ys'] == Just [6, 9]
  print $ (summed <$> ((,) <$> xs' <*> ys')) == Just 15
  print $ fmap summed ((,) <$> xs' <*> zs') == Nothing
  print $ bolt 7 == True
  print $ fmap bolt z == [True, False, False]
  print $ sequenceA [(> 3), (< 8), even] (7 :: Integer) == [True, True, False]
  print $ (foldr (&&) True $ sequA (6 :: Integer)) == True
  print $ (fromMaybe [] (sequA <$> s')) == [True, False, False]
  print $ (fromMaybe False (bolt <$> ys')) == False

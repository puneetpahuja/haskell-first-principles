{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import           Test.QuickCheck

-- be kind
-- 1. a :: *
-- 2. b :: * -> *, T :: * -> *
-- 3. c :: * -> * -> *
-- Heavy Lifting
a = fmap (+ 1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (* 2) (\x -> x - 2)

d :: Int -> IO String
d = fmap (return . ("1" ++) . show) (\x -> [x,1 .. 3])

e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) ioi
   in fmap (* 3) changed

testHeavyLifting :: IO ()
testHeavyLifting = do
  print $ a == [2]
  print $ b == Just ["Hi,lol", "Hellolol"]
  print $ c 1 == (-2)
  d' <- d 0
  print $ d' == "1[0,1,2,3]"
  e' <- e
  print $ e' == 3693

-- Instances of Func
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == id f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type I = Identity String

data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

type P = Pair String

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type Tw = Two String Int

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type Th = Three Int String Int

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y1 y2) = Three' x (f y1) (f y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

type Th' = Three' String Int

data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z q) = Four x y z (f q)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type Fo = Four String Int String String

data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x1 x2 x3 y) = Four' x1 x2 x3 (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    b3 <- arbitrary
    return $ Four' a b1 b2 b3

type Fo' = Four' Int String

-- can't implement for data Trivial = Trivial because we need kind * -> * for
-- functor but Trivial has kind *
-- Possibly
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

-- TODO: test this with quickCheck
instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

-- Short Exercise (p688)
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

-- TODO: test with quickCheck
instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

-- 2. Because we need f in (instance Functor f) to be of kind * -> *
--    We cant have (Either b) in (Either a b) as * -> *
--    The first type argument i.e. a will be applied first to Either
-- fmap :: (a -> b) -> Wrap f a -> Wrap f b
main :: IO ()
main = do
  quickCheck (functorIdentity :: I -> Bool)
  quickCheck (functorCompose length (* 5) :: I -> Bool)
  quickCheck (functorIdentity :: P -> Bool)
  quickCheck (functorCompose length (+ 7) :: P -> Bool)
  quickCheck (functorIdentity :: Tw -> Bool)
  quickCheck (functorCompose (* 5) (+ 2) :: Tw -> Bool)
  quickCheck (functorIdentity :: Th -> Bool)
  quickCheck (functorCompose (* 5) (+ 2) :: Th -> Bool)
  quickCheck (functorIdentity :: Th' -> Bool)
  quickCheck (functorCompose (* 5) (+ 2) :: Th' -> Bool)
  quickCheck (functorIdentity :: Fo -> Bool)
  quickCheck (functorCompose length (+ 2) :: Fo -> Bool)
  quickCheck (functorIdentity :: Fo' -> Bool)
  quickCheck (functorCompose length (+ 2) :: Fo' -> Bool)

-- Chapter Exercises
-- 1. No
-- 2. Yes
-- 3. Yes
-- 4. Yes
-- 5. No
-- data Sum a b = First b | Second a
-- data Company a b c = DeepBlue a b | Something c
-- data More a b = L b a b | R a b a
data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b =
  IgnoreSomething (f a) (g b)
  deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat                     = NoGoat
  fmap f (OneGoat a)                = OneGoat (f a)
  fmap f (MoreGoats gla gla' gla'') = MoreGoats (fmap f gla) (fmap f gla') (fmap f gla'')

testGoatLord :: IO ()
testGoatLord =
  print $
  fmap (+ 2) (MoreGoats NoGoat (OneGoat 4) (MoreGoats (OneGoat 5) (OneGoat 7) (MoreGoats NoGoat NoGoat (OneGoat 44)))) ==
  (MoreGoats NoGoat (OneGoat 6) (MoreGoats (OneGoat 7) (OneGoat 9) (MoreGoats NoGoat NoGoat (OneGoat 46))))

data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Show a => Show (TalkToMe a) where
  show Halt          = "Halt"
  show (Print msg a) = "Print " ++ msg ++ " " ++ show a
  show (Read f)      = "Read f"

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print x a) = Print x (f a)
  fmap f (Read g)    = Read (f . g)

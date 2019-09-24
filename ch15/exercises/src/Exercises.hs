module Main where

import           Data.Monoid
import           Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary, frequency,
                                  quickCheck, verboseCheck)

data Booly
  = False'
  | True'

instance Semigroup Booly where
  (<>) True' True' = True'
  (<>) _ _         = False'

instance Monoid Booly where
  mempty = True'

-- Optional Monoid
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only $ x <> y
  (<>) Nada x            = x
  (<>) x Nada            = x

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

testOptional :: IO ()
testOptional = do
  print $ Only (Sum 1) `mappend` Only (Sum 1)
  print $ Only (Product 4) `mappend` Only (Product 2)
  print $ Only (Sum 1) `mappend` Nada
  print $ Only [1] `mappend` Nada
  print $ Nada `mappend` Only (Sum 1)

-- mad lib
type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove of with his "
    , adj
    , " wife."
    ]

-- infix names for function args
asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

-- Maybe another monoid
newtype First' a =
  First'
    { getFirst' :: Optional a
    }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) x y =
    case (getFirst' x, getFirst' y) of
      (Nada, p)        -> First' {getFirst' = p}
      (p, Nada)        -> First' {getFirst' = p}
      (Only p, Only q) -> First' {getFirst' = Only p}

instance Monoid (First' a) where
  mempty = First' {getFirst' = Nada}

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency
      [ (1, return $ First' {getFirst' = Nada})
      , (2, return $ First' {getFirst' = Only x})
      ]

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

testFirst' :: IO ()
testFirst' = do
  print $ First' (Only 1) `mappend` First' Nada
  print $ (First' Nada `mappend` First' Nada :: First' Int)
  print $ First' Nada `mappend` First' (Only 2)
  print $ First' (Only 1) `mappend` First' (Only 2)

-- Chapter exercises
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type T = Trivial

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type I = Identity String

data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type Tw = Two String (Sum Int)

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type Th = Three String (Sum Int) (Product Int)

data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four x y z q) <> (Four x' y' z' q') =
    Four (x <> x') (y <> y') (z <> z') (q <> q')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type Fo = Four String (Sum Int) (Product Int) String

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BC = BoolConj

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

type BD = BoolDisj

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst _ <> x = x
  Snd b <> _ = Snd b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Fst a), (1, return $ Snd b)]

type O = Or Int String

newtype Combine a b =
  Combine
    { uncombine :: (a -> b)
    }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\a -> g a <> f a)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\a -> mempty)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

instance Show (Combine a b) where
  show _ = "Combine's show"

combineSemigroupAssoc ::
     (Semigroup b, Eq b)
  => Combine a b
  -> Combine a b
  -> Combine a b
  -> a
  -> Bool
combineSemigroupAssoc f g h a =
  (uncombine ((f <> g) <> h) $ a) == (uncombine (f <> (g <> h)) $ a)

combineMonoidLeftIdentity :: (Monoid b, Eq b) => Combine a b -> a -> Bool
combineMonoidLeftIdentity c@(Combine f) a = (uncombine (c <> mempty)) a == f a

combineMonoidRightIdentity :: (Monoid b, Eq b) => Combine a b -> a -> Bool
combineMonoidRightIdentity c@(Combine f) a = (uncombine (mempty <> c)) a == f a

type C = Combine String String

newtype Comp a =
  Comp
    { uncomp :: (a -> a)
    }

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (\a -> g a <> f a)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp (\a -> mempty)

instance Arbitrary (Comp a) where
  arbitrary = return $ Comp id

instance Show (Comp a) where
  show _ = "Comp's show"

compSemigroupAssoc ::
     (Semigroup a, Eq a) => Comp a -> Comp a -> Comp a -> a -> Bool
compSemigroupAssoc f g h a =
  (uncomp ((f <> g) <> h) $ a) == (uncomp (f <> (g <> h)) $ a)

compMonoidLeftIdentity :: (Monoid a, Eq a) => Comp a -> a -> Bool
compMonoidLeftIdentity c@(Comp f) a = (uncomp (c <> mempty)) a == f a

compMonoidRightIdentity :: (Monoid a, Eq a) => Comp a -> a -> Bool
compMonoidRightIdentity c@(Comp f) a = (uncomp (mempty <> c)) a == f a

type Co = Comp String

data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure x <> Failure y = Failure $ x <> y
  Success x <> _ = Success x
  _ <> Success y = Success y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Failure a), (1, return $ Success b)]

type V = Validation String Int

testValidation :: IO ()
testValidation = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

newtype Mem s a =
  Mem
    { runMem :: s -> (a, s)
    }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem go
        -- go :: s -> (a, s)
    where
      go state =
        let (a', state') = g state
            (a'', state'') = f state'
         in (a' <> a'', state'') -- TODO: or (a'' <> a', state'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem go
      -- go :: s -> (a, s)
    where
      go state = (mempty, state)

-- TODO: Write quickcheck properties for laws
testMem :: IO ()
testMem = do
  let f' = Mem $ \s -> ("hi", s + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

-- TODO: Check these with other people's solutions
main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
  quickCheck (semigroupAssoc :: T -> T -> T -> Bool)
  quickCheck (monoidLeftIdentity :: T -> Bool)
  quickCheck (monoidRightIdentity :: T -> Bool)
  quickCheck (semigroupAssoc :: I -> I -> I -> Bool)
  quickCheck (monoidLeftIdentity :: I -> Bool)
  quickCheck (monoidRightIdentity :: I -> Bool)
  quickCheck (semigroupAssoc :: Tw -> Tw -> Tw -> Bool)
  quickCheck (monoidLeftIdentity :: Tw -> Bool)
  quickCheck (monoidRightIdentity :: Tw -> Bool)
  quickCheck (semigroupAssoc :: Th -> Th -> Th -> Bool)
  quickCheck (semigroupAssoc :: Fo -> Fo -> Fo -> Bool)
  quickCheck (semigroupAssoc :: BC -> BC -> BC -> Bool)
  quickCheck (monoidLeftIdentity :: BC -> Bool)
  quickCheck (monoidRightIdentity :: BC -> Bool)
  quickCheck (semigroupAssoc :: BD -> BD -> BD -> Bool)
  quickCheck (monoidLeftIdentity :: BD -> Bool)
  quickCheck (monoidRightIdentity :: BD -> Bool)
  quickCheck (semigroupAssoc :: O -> O -> O -> Bool)
  quickCheck (combineSemigroupAssoc :: C -> C -> C -> String -> Bool)
  quickCheck (combineMonoidLeftIdentity :: C -> String -> Bool)
  quickCheck (combineMonoidRightIdentity :: C -> String -> Bool)
  quickCheck (compSemigroupAssoc :: Co -> Co -> Co -> String -> Bool)
  quickCheck (compMonoidLeftIdentity :: Co -> String -> Bool)
  quickCheck (compMonoidRightIdentity :: Co -> String -> Bool)
  quickCheck (semigroupAssoc :: V -> V -> V -> Bool)

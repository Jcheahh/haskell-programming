{-# LANGUAGE DerivingVia #-}

module Ch15 where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Booly a
  = False'
  | True'
  deriving (Eq, Show)

instance Semigroup (Booly a) where
  (<>) False' _ = False'
  (<>) _ False' = False'
  (<>) True' True' = True'

instance Monoid (Booly a) where
  mempty = True'

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada b = b
  (<>) a Nada = a
  (<>) (Only a) (Only b) = Only $ a <> b

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' ::
  Exclamation ->
  Adverb ->
  Noun ->
  Adjective ->
  String
madlibbin' e adv noun adj =
  e <> "! he said "
    <> adv
    <> " as he jumped into his car "
    <> noun
    <> " and drove off with his "
    <> adj
    <> " wife."

madlibbinBetter' ::
  Exclamation ->
  Adverb ->
  Noun ->
  Adjective ->
  String
madlibbinBetter' e adv noun adj =
  mconcat
    [ e,
      "! he said ",
      adv,
      " as he jumped into his car ",
      noun,
      " and drove off with his ",
      adj,
      " wife."
    ]

asc ::
  Eq a =>
  (a -> a -> a) ->
  a ->
  a ->
  a ->
  Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

monoidAssoc ::
  (Eq m, Monoid m) =>
  m ->
  m ->
  m ->
  Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency
      [ (1, return Fools),
        (1, return Twoo)
      ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool

newtype First' a = First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) b = b
  (<>) a _ = a

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency
      [ (1, return (First' Nada)),
        (1, return (First' $ Only a))
      ]

firstMappend ::
  First' a ->
  First' a ->
  First' a
firstMappend = mappend

type FirstMappend =
  First' String ->
  First' String ->
  First' String ->
  Bool

type FstId =
  First' String -> Bool

semigroupAssoc ::
  (Eq m, Semigroup m) =>
  m ->
  m ->
  m ->
  Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = mempty

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdenAssoc =
  Identity String -> Identity String -> Identity String -> Bool

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc =
  Two [Int] String -> Two [Int] String -> Two [Int] String -> Bool

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc =
  Three [Int] String [Int] -> Three [Int] String [Int] -> Three [Int] String [Int] -> Bool

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc =
  Four [Int] String [Int] String -> Four [Int] String [Int] String -> Four [Int] String [Int] String -> Bool

-- 6.
newtype BoolConj
  = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj $ x && y

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.
newtype BoolDisj
  = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj $ x || y

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd a <> _ = Snd a
  _ <> b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

type OrAssoc = Or Char Int -> Or Char Int -> Or Char Int -> Bool

-- 9.
newtype Combine a b = Combine {unCombine :: (a -> b)}

-- deriving Semigroup via (a -> b)
instance Show (Combine a b) where
  show _ = "Combine function"

-- instance Eq (Combine a b) where
--   (==) x y = x == y

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ f <> g

--  Combine f <> Combine g = \x -> Combine $ f x <> g x
instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CombineAssoc = Combine String String -> Combine String String -> Combine String String -> Bool

-- 10.
newtype Comp a = Comp {unComp :: (a -> a)}

instance Show (Comp a) where
  show _ = "Comp a"

-- instance Eq (Comp a) where
--   x == y = x == y

instance Semigroup a => Semigroup (Comp a) where
  Comp a <> Comp b = Comp $ a <> b

instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    a <- arbitrary
    return $ Comp a

type CompAssoc = Comp String -> Comp String -> Comp String -> Bool

-- 11.
data Validation a b
  = Failure' a
  | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success' a <> _ = Success' a
  _ <> Success' b = Success' b
  Failure' a <> Failure' b = Failure' $ a <> b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary

    oneof [return $ Failure' a, return $ Success' b]

type ValidationAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

-- 8.
newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g =
    Mem $
      \n ->
        let (x, y) = f n
            (a, s) = g y
         in (x <> a, s)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))

main :: IO ()
main = do
  let ma = monoidAssoc
  -- mli = monoidLeftIdentity
  -- mri = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  -- quickCheck (mli :: Bull -> Bool)
  -- quickCheck (mri :: Bull -> Bool)

  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdenAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)

  -- quickCheck (semigroupAssoc :: CombineAssoc)
  -- quickCheck (semigroupAssoc :: CompAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  let f' = Mem $ \s -> ("hi", s + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
      rmboth = runMem (f' <> f') 0
  print $ rmleft
  print $ rmright
  print $ rmboth
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

-----------------------------------------------------------------
-- Revision
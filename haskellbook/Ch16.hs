{-# LANGUAGE FlexibleInstances #-}

module Ch16 where

import GHC.Arr

{-
   1. *
   2. * -> * and * -> *
   3. * -> * -> *
-}
replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- outermost
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- one more layer in
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- two more layer in
twiceLifted ::
  (Functor f1, Functor f) =>
  f (f1 a) ->
  f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' ::
  [Maybe [Char]] ->
  [Maybe Char]
twiceLifted' = twiceLifted

-- innermost
thriceLifted ::
  (Functor f2, Functor f1, Functor f) =>
  f (f1 (f2 a)) ->
  f (f1 (f2 Char))
thriceLifted =
  (fmap . fmap . fmap) replaceWithP

thriceLifted' ::
  [Maybe [Char]] ->
  [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)
  putStr "liftedReplace lms: "
  print (liftedReplace lms)
  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)
  putStr "twiceLifted lms: "
  print (twiceLifted lms)
  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)
  putStr "thriceLifted lms: "
  print (thriceLifted lms)
  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)

-- 1.
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
-- c :: (Functor f, Num b) => f b -> f b
c = ((* 2) . (\x -> x - 2))

d =
  ((return '1' ++) . show) . (\x -> [x, 1 .. 3])

e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) ioi
   in fmap (* 3) changed

-- 1.
newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 2.
data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

-- 3.
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4.
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- 5.
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

-- 6.
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- 7.
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

-- 8.
data Trivial = Trivial

-- cannot because Trivial dont have * -> * kinds

-- Exercise: Possibly
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap _ LolNope = LolNope

incIfRight ::
  Num a =>
  Either e a ->
  Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight ::
  Show a =>
  Either e a ->
  Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

incEither ::
  Num a =>
  Either e a ->
  Either e a
incEither m = fmap (+ 1) m

showEither ::
  Show a =>
  Either e a ->
  Either e String
showEither s = fmap show s

incEither' ::
  Num a =>
  Either e a ->
  Either e a
incEither' = fmap (+ 1)

showEither' ::
  Show a =>
  Either e a ->
  Either e String
showEither' = fmap show

-- 1.
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

data Wrap f a
  = Wrap (f a)
  deriving (Eq, Show)

instance
  Functor f =>
  Functor (Wrap f)
  where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- 1
data Bool
  = False
  | True

-- invalid datatype for Functor

-- 2.
data BoolAndSomethingElse a
  = False' a
  | True' a

instance Functor BoolAndSomethingElse where
  fmap f (True' a) = True' (f a)
  fmap f (False' a) = False' (f a)

-- 3.
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap f (Truish a) = Truish (f a)
  fmap f Falsish = Falsish

-- 4.
newtype Mu f = InF {outF :: f (Mu f)}

-- Mu has (* -> *) -> * kind

-- 5.
data D
  = D (Array Word Word) Int Int

-- invalid datatype for Functor. The kind is *

-- 1.
data Sum' a b
  = First' a
  | Second' b

instance Functor (Sum' a) where
  fmap f (First' a) = First' a
  fmap f (Second' b) = Second' (f b)

-- 2.
data Company a b c
  = DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

-- 3.
data More a b
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

-- 1.
data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor x) = Bloor (f x)
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance

-- 2.
data K a b
  = K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3.
newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)

newtype K' a b
  = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))

-- 4.
data EvilGoateeConst a b
  = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

-- 5.
data LiftItOut f a
  = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut a) = LiftItOut (fmap f a)

-- 6.
data Parappa f g a
  = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- 7.
data IgnoreOne f g a b
  = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

-- 8.
data Notorious g o a t
  = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

-- 9.
data List a
  = Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)

-- 10.
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats
      (GoatLord a)
      (GoatLord a)
      (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11.
data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g) = Read (f . g)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ch17 where

import Control.Applicative
import Data.List (elemIndex)
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

f x =
  lookup
    x
    [ (3, "hello"),
      (4, "julie"),
      (5, "kbai")
    ]

g y =
  lookup
    y
    [ (7, "sup?"),
      (8, "chris"),
      (9, "aloha")
    ]

h z =
  lookup z [(2, 3), (5, 6), (7, 8)]

m x =
  lookup x [(4, 10), (8, 13), (1, 9001)]

-- 1.
pureTC :: [Integer]
pureTC = pure (* 2) <*> [1 .. 3]

-- 2.
fmapTC :: Maybe Integer
fmapTC = (+) <$> h 5 <*> m 1

-- 3.
apTC :: Maybe [Char]
apTC = (++) <$> f 3 <*> g 7

-- 1.
added :: Maybe Integer
added =
  pure (+ 3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4.
xs = [1, 2, 3]

ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y''

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  _ <*> (Constant a) = Constant a

validateLength ::
  Int ->
  String ->
  Maybe String
validateLength maxLen s =
  if (length s) > maxLen
    then Nothing
    else Just s

newtype Name
  = Name String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s =
  fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a =
  fmap Address $ validateLength 100 a

data Person
  = Person Name Address
  deriving (Eq, Show)

mkPerson ::
  String ->
  String ->
  Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a

-- 1.
first = const <$> Just "Hello" <*> pure "World"

-- 2.
second = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- instance Semigroup a => Semigroup (List a) where
--   Nil <> _ = Nil
--   _ <> Nil = Nil
--   Cons x xs <> Cons y ys = Cons (x <> y) (xs <> ys)

-- instance Monoid a => Monoid (List a) where
--   mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f x) <*> y = (fmap f y) `append` (x <*> y)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap ::
  (a -> List b) ->
  List a ->
  List b
flatMap f = concat' . fmap f

newtype ZipList' a
  = ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take 3000 l
      ys' =
        let (ZipList' l) = ys
         in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' [a]
  ZipList' _ <*> ZipList' [] = ZipList' []
  ZipList' fs <*> ZipList' xs = ZipList' (zipWith ($) fs (cycle xs))

data Errors
  = DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

data Validation err a
  = Failure err
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure err) = Failure err
  fmap f (Success a) = Success (f a)

instance
  Monoid e =>
  Applicative (Validation e)
  where
  pure = Success
  Failure err <*> (Success _) = Failure err
  (Success _) <*> Failure err = Failure err
  Failure err <*> Failure err' = Failure (err <> err')
  Success f <*> Success a = Success (f a)

-- 1. -- Type
-- []
-- Methods
-- pure :: a -> [] a
-- (<*>) :: [] (a -> b) -> [] a -> [] b

-- 2. -- Type
-- IO
-- -- Methods
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. -- Type
-- (,) e
-- -- Methods
-- pure :: a -> (,) e a
-- (<*>) :: (,) e (a -> b) -> (,) e a -> (,) e b

-- 4. -- Type
-- (->) e
-- -- Methods
-- pure :: a -> (->) e a
-- (<*>) :: (->) e (a -> b) -> (->) e a -> (->) e b

-- 1.
data Pair a = Pair a a deriving (Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq (Pair a)) => EqProp (Pair a) where
  (=-=) = eq

-- 2.
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two _ f' <*> Two a a' = Two a (f' a')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq (Two a b)) => EqProp (Two a b) where
  (=-=) = eq

-- 3.
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three f g h <*> Three a b c = Three a b (h c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq (Three a b c)) => EqProp (Three a b c) where
  (=-=) = eq

-- 4.
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  Three' f g g' <*> Three' a b b' = Three' a (g b) (g' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq (Three' a b)) => EqProp (Three' a b) where
  (=-=) = eq

-- 5.
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four f g h j <*> Four a b c d = Four a b c (j d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq (Four a b c d)) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6.
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' f f' f'' g <*> Four' a a' a'' b = Four' a a' a'' (g b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq (Four' a b)) => EqProp (Four' a b) where
  (=-=) = eq

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

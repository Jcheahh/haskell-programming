{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ch18 where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join $ fmap f xs

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' m m' = do
  x <- m
  x' <- m'
  return (x x')

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure b = Second b
  _ <*> (First x) = First x
  (First x) <*> (Second y) = First x
  (Second f) <*> (Second y) = Second (f y)

instance Monad (Sum a) where
  return = pure
  (First x) >>= f = First x
  (Second y) >>= f = f y

data CountMe a
  = CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a =
    CountMe (n + n') (f a)

instance
  Arbitrary a =>
  Arbitrary (CountMe a)
  where
  arbitrary =
    CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
     in CountMe (n + n') b

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you? "

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

-- 1.
data Nope a
  = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq (Nope a) => EqProp (Nope a) where
  (=-=) = eq

-- 2.
data BahEither b a
  = PLeft a
  | PRight b

instance Functor (BahEither a) where
  fmap f (PLeft a) = PLeft (f a)
  fmap f (PRight b) = PRight b

instance Applicative (BahEither a) where
  pure = PLeft
  _ <*> PRight b = PRight b
  PRight b <*> _ = PRight b
  PLeft f <*> PLeft b = PLeft (f b)

instance Monad (BahEither a) where
  return = pure
  PRight b >>= f = PRight b
  PLeft a >>= f = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
  arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

instance Eq (BahEither b a) => EqProp (BahEither b a) where
  (=-=) = eq

-- 3.
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq (Identity a) => EqProp (Identity a) where
  (=-=) = eq

-- 4.
data List a
  = Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> y = (fmap f y) `append` (fs <*> y)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = f x `append` (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

l2 ::
  Monad m =>
  (a -> b -> c) ->
  m a ->
  m b ->
  m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a xs f = f <*> xs

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = do
  xs' <- meh xs f
  f x >>= \x' -> return $ x' : xs'

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id

-----------------------------------------------------------------
-- Revision
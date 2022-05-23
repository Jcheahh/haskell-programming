module Ch20 where

import Data.Monoid

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' ::
  (Foldable t, Eq a) =>
  a ->
  t a ->
  Bool
elem' x = getAny . foldMap (Any . (== x))

minimum' ::
  (Foldable t, Ord a) =>
  t a ->
  Maybe a
minimum' = foldr go Nothing
  where
    go x Nothing = Just x
    go x (Just y) = Just $ min x y

maximum' ::
  (Foldable t, Ord a) =>
  t a ->
  Maybe a
maximum' = foldr go Nothing
  where
    go x Nothing = Just x
    go x (Just y) = Just $ max x y

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ a -> 1 + a) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (<> mempty)

foldMap' ::
  (Foldable t, Monoid m) =>
  (a -> m) ->
  t a ->
  m
foldMap' f = foldr ((<>) . f) mempty

-- 20.6
-- 1.
data Constant a b
  = Constant b

instance Foldable (Constant a) where
  foldr f x (Constant b) = f b x
  foldMap f (Constant b) = f b

-- 2.
data Two a b
  = Two a b

instance Foldable (Two a) where
  foldr f x (Two a b) = f b x
  foldMap f (Two a b) = f b

-- 3.
data Three a b c
  = Three a b c

instance Foldable (Three a b) where
  foldr f x (Three a b c) = f c x
  foldMap f (Three a b c) = f c

-- 4.
data Three' a b
  = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

-- 5.
data Four' a b
  = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b <> f b' <> f b''

filterF ::
  ( Applicative f,
    Foldable t,
    Monoid (f a)
  ) =>
  (a -> Bool) ->
  t a ->
  f a
filterF f = foldMap go
  where
    go x = if f x then pure x else mempty

-----------------------------------------------------------------
-- Revision
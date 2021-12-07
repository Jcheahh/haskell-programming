{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ch21 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Example on when to use Traversable
data Query = Query

data SomeObj = SomeObj

data IoOnlyObj = IoOnlyObj

data Err = Err

-- There's a decoder function that makes
-- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the
-- DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer",
-- that also has IO
makeIoOnlyObj ::
  [SomeObj] ->
  IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

-- after
pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query

  traverse makeIoOnlyObj (traverse decodeFn a)

-- latest
pipelineFn'' ::
  Query ->
  IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' =
  ( ( traverse makeIoOnlyObj
        . traverse decodeFn
    )
      =<<
  )
    . fetchFn

type TI = []

main = do
  let trigger :: TI (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- Traversable instances

-- Identity
newtype Identity' a = Identity' a
  deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap f (Identity' x) = Identity' $ f x

instance Foldable Identity' where
  foldMap f (Identity' x) = f x

instance Traversable Identity' where
  traverse f (Identity' x) = Identity' <$> f x

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = Identity' <$> arbitrary

instance Eq (Identity' a) => EqProp (Identity' a) where
  (=-=) = eq

-- Constant
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq (Constant a b) => EqProp (Constant a b) where
  (=-=) = eq

-- Maybe
data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]

instance Eq (Optional a) => EqProp (Optional a) where
  (=-=) = eq

-- List
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq (List a) => EqProp (List a) where
  (=-=) = eq

-- Three
data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

instance Traversable (Three a b) where
  traverse f (Three a b x) = Three a b <$> f x

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Eq (Three a b c) => EqProp (Three a b c) where
  (=-=) = eq

-- Pair
data Pair a b
  = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a x) = Pair a (f x)

instance Foldable (Pair a) where
  foldMap f (Pair a x) = f x

instance Traversable (Pair a) where
  traverse f (Pair a x) = Pair a <$> f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq (Pair a b) => EqProp (Pair a b) where
  (=-=) = eq

-- Big
-- When you have more than one value of type b, use Monoid and Applicative
-- for the Foldable and Traversable instances, respectively:
data Big a b
  = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a x x') = Big a (f x) (f x')

instance Foldable (Big a) where
  foldMap f (Big a x x') = f x <> f x'

instance Traversable (Big a) where
  traverse f (Big a x x') = Big a <$> f x <*> f x'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance Eq (Big a b) => EqProp (Big a b) where
  (=-=) = eq

-- Bigger
-- Same as for Big:
data Bigger a b
  = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a x x' x'') = Bigger a (f x) (f x') (f x'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a x x' x'') = f x <> f x' <> f x''

instance Traversable (Bigger a) where
  traverse f (Bigger a x x' x'') = Bigger a <$> f x <*> f x' <*> f x''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Eq (Bigger a b) => EqProp (Bigger a b) where
  (=-=) = eq

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S x a) = S (fmap f x) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S x a) = foldMap f x <> f a

instance
  ( Functor n,
    Arbitrary (n a),
    Arbitrary a
  ) =>
  Arbitrary (S n a)
  where
  arbitrary = S <$> arbitrary <*> arbitrary

instance
  ( Applicative n,
    Testable (n Property),
    Eq a,
    Eq (n a),
    EqProp a
  ) =>
  EqProp (S n a)
  where
  (=-=) = eq

instance Traversable n => Traversable (S n) where
  traverse f (S x a) = S <$> traverse f x <*> f a

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node xs a ys) = Node (fmap f xs) (f a) (fmap f ys)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node xs a ys) = foldMap f xs <> f a <> foldMap f ys

  foldr _ z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node xs a ys) = f a $ foldr f (foldr f z xs) ys

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node xs a ys) = Node <$> traverse f xs <*> f a <*> traverse f ys

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [return Empty, Leaf <$> arbitrary, Node <$> arbitrary <*> arbitrary <*> arbitrary]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main' :: IO ()
main' = do
  let iden :: Identity' (Int, Int, [Int])
      iden = undefined

      cons :: Constant (Int, Int, [Int]) (Int, Int, [Int])
      cons = undefined

      opt :: Optional (Int, Int, [Int])
      opt = undefined

      lst :: List (Int, Int, [Int])
      lst = undefined

      three :: Three Int Int (Int, Int, [Int])
      three = undefined

      pair :: Pair Int (Int, Int, [Int])
      pair = undefined

      big :: Big Int (Int, Int, [Int])
      big = undefined

      bigger :: Bigger Int (Int, Int, [Int])
      bigger = undefined

      s :: S [] (Int, Int, [Int])
      s = undefined

      tree :: Tree (Int, Int, [Int])
      tree = undefined

  quickBatch (traversable iden)
  quickBatch (traversable cons)
  quickBatch (traversable opt)
  quickBatch (traversable lst)
  quickBatch (traversable three)
  quickBatch (traversable pair)
  quickBatch (traversable big)
  quickBatch (traversable bigger)
  quickBatch (traversable s)
  quickBatch (traversable tree)
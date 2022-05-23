module Ch12 where

import Data.Maybe

-- 12.5
{-
    1. Given:
        id :: a -> a
        What is the kind of a?
        a :: *
    2. r :: a -> f a
        What are the kinds of a and f?
        a :: *
        f :: * -> *
-}
-- 1.
notThe :: String -> Maybe String
notThe xs = if take 3 xs == "the" then Nothing else Just xs

replaceThe :: String -> String
replaceThe xs = unwords $ fmap (\x -> if notThe x == Nothing then "a" else x) (words xs)

-- 2.
countTheBeforeVowel :: String -> Integer
-- countTheBeforeVowel xs = foldr (\x b -> if notThe x == Nothing then b + 1 else b) 0 (words xs)
countTheBeforeVowel = go 0 . words
  where
    go :: Integer -> [String] -> Integer
    go n [] = n
    go n [a] = n
    go n ("the" : (c : cs) : xs)
      | c `elem` vowels = go (n + 1) xs
    go n (x : xs) = go n xs

-- 3.
countVowels :: String -> Integer
countVowels = foldr (\x b -> if x `elem` vowels then b + 1 else b) 0

newtype Word'
  = Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs = if length filterVowels > length filterNotVowels then Nothing else Just (Word' xs)
  where
    filterVowels = filter (`elem` vowels) xs
    filterNotVowels = filter (`notElem` vowels) xs

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat (-1) = Nothing
integerToNat 0 = Just Zero
integerToNat x = Just (go x)
  where
    go 0 = Zero
    go x = Succ (go (x -1))

-- 1.
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' (Just _) = False

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just x) = f x

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just a) = a

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Nothing : xs) = catMaybes' xs
catMaybes' ((Just x : xs)) = x : catMaybes' xs

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs'@(x : xs) = if isNothing' x then Nothing else sequence xs'

-- 1.
lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' ((Left a) : xs) = a : lefts' xs
lefts' ((Right b) : xs) = lefts' xs

lefts1 :: [Either a b] -> [a]
lefts1 =
  foldr
    ( \x b -> case x of
        Left a -> a : b
        Right _ -> b
    )
    []

-- 2.
rights' :: [Either a b] -> [b]
rights' [] = []
rights' ((Left a) : xs) = rights' xs
rights' ((Right b) : xs) = b : rights' xs

rights1 :: [Either a b] -> [b]
rights1 =
  foldr
    ( \x b -> case x of
        Right a -> a : b
        Left _ -> b
    )
    []

-- 3.
partitionEithers' ::
  [Either a b] ->
  ([a], [b])
partitionEithers' xs = (l, r)
  where
    l = lefts1 xs
    r = rights1 xs

-- 4.
eitherMaybe' ::
  (b -> c) ->
  Either a b ->
  Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ (Left _) = Nothing

-- 5.
either' ::
  (a -> c) ->
  (b -> c) ->
  Either a b ->
  c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

-- 6.
eitherMaybe'' ::
  (b -> c) ->
  Either a b ->
  Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- 2.
myUnfoldr ::
  (b -> Maybe (a, b)) ->
  b ->
  [a]
myUnfoldr f x = case f x of
  Nothing -> []
  Just (a, b) -> a : myUnfoldr f b

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree:
unfold ::
  (a -> Maybe (a, b, a)) ->
  a ->
  BinaryTree b
unfold f x = case f x of
  Nothing -> Leaf
  Just (l, m, r) -> Node (unfold f l) m (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x >= n then Nothing else Just (x + 1, x, x + 1)) 0

-----------------------------------------------------------------
-- Revision
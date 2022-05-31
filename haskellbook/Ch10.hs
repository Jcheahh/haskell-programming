module Ch10 where

import Data.Time
import GHC.Base (liftA3)

{-
foldl (flip (*)) 1 [1..3]

(1 * 1)
(2 * (1 * 1))
(3 * (2 * (1 * 1)))

3. a
4. d
5.
  a) foldr (++) [] ["woot", "WOOT", "woot"]
  b) foldr max [] ["fear is the little death"]
  c) foldr (&&) True [False, True]
  d) foldr (||) False [False, False]
  e) foldr ((++) . show) "" [1..5]
  f) foldl const 'a' [1..5]
  g) foldl const 0 "tacos"
  h) foldr (flip const) 0 "burritos"
  i) foldr (flip const) 'z' [1..5]
-}

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbNumber 25,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

filterDbDate ::
  [DatabaseItem] ->
  [UTCTime]
filterDbDate [] = []
filterDbDate (DbDate x : xs) = x : filterDbDate xs
filterDbDate (_ : xs) = filterDbDate xs

filterDbNumber ::
  [DatabaseItem] ->
  [Integer]
filterDbNumber [] = []
filterDbNumber (x : xs) = case x of
  DbNumber n -> n : filterDbNumber xs
  _ -> filterDbNumber xs

mostRecent ::
  [DatabaseItem] ->
  UTCTime
mostRecent xs = maximum $ filterDbDate xs

sumDb ::
  [DatabaseItem] ->
  Integer
sumDb xs = sum $ filterDbNumber xs

avgDb ::
  [DatabaseItem] ->
  Double
avgDb xs = fromIntegral (sumDb xs) / fromIntegral (length (filterDbNumber xs))

fibs = filter (< 100) $ 1 : scanl (+) 1 fibs

facts :: [Integer]
facts = scanl (*) 1 [1 ..]

factorial x = facts !! x

stops = "pbtdkg"

vowels = "aeiou"

stopVowelStop :: String -> String -> [(Char, Char, Char)]
stopVowelStop xs ys = go xs ys
  where
    vowelStop :: String -> String -> [(Char, Char)]
    vowelStop [] _ = []
    vowelStop (x : xs) ys = fmap (\y -> (x, y)) ys ++ vowelStop xs ys
    vs :: [(Char, Char)]
    vs = vowelStop ys xs
    go :: String -> String -> [(Char, Char, Char)]
    go [] _ = []
    go xs@(x : xs') ys =
      fmap (\(y, z) -> (x, y, z)) vs
        ++ go xs' ys

stopVowelStop' :: [String] -> [String] -> [(String, String, String)]
stopVowelStop' xs ys = liftA3 (,,) xs ys xs

myOr1 :: [Bool] -> Bool
myOr1 = foldr (||) False

myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x : xs) = if x then True else myOr2 xs

myOr3 :: [Bool] -> Bool
myOr3 [] = False
myOr3 (x : xs) = x || myOr2 xs

myOr4 :: [Bool] -> Bool
myOr4 =
  foldr
    ( \a b ->
        if a
          then True
          else b
    )
    False

myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 _ [] = False
myAny1 f (x : xs) = if f x then True else myAny1 f xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 _ [] = False
myAny2 f (x : xs) = f x || myAny2 f xs

myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f xs =
  foldr
    ( \a b ->
        if f a
          then True
          else b
    )
    False
    xs

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 a' = foldr (\a b -> if a == a' then True else b) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a xs = any (== a) xs

myReverse1 :: [a] -> [a]
myReverse1 xs = go xs []
  where
    go [] acc = acc
    go (x : xs) acc = go xs (x : acc)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x : xs) = if f x then x : myFilter f xs else myFilter f xs

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap1 :: (a -> [b]) -> [a] -> [b]
squishMap1 _ [] = []
squishMap1 f (x : xs) = f x ++ squishMap1 f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap1 id

myMaximumBy ::
  (a -> a -> Ordering) ->
  [a] ->
  a
myMaximumBy f [] = error "Empty list"
myMaximumBy f (x : xs) = go f xs x
  where
    go _ [] max = max
    go f (x : xs) max
      | f x max == LT = go f xs max
      | f x max == GT = go f xs x
      | otherwise = go f xs max

myMinimumBy ::
  (a -> a -> Ordering) ->
  [a] ->
  a
myMinimumBy f [] = error "Empty list"
myMinimumBy f (x : xs) = go f xs x
  where
    go _ [] max = max
    go f (x : xs) max
      | f x max == GT = go f xs max
      | f x max == LT = go f xs x
      | otherwise = go f xs max

-----------------------------------------------------------------
-- Revision

-- Exercises: Understanding folds

-- 1. b,c
-- 2. (3 * (2 * (1 * 1)))
-- 3. a
-- 4. d
-- 5.  a) foldr (++) [] ["woot", "WOOT", "woot"]
--     b) foldr max ' ' "fear is the little death"
--     c) foldr (&&) True [False, True]
--     d) foldr (||) False [False, False]
--     e) foldl (flip ((++) . show)) "" [1..5]
--     f) foldr const 0 [1..5]
--     g) foldr const ' ' "tacos"
--     h) foldl (flip const) ' ' "burritos"
--     i) foldl (flip const) 0 [1..5]

-- >>> filterDbDate' theDatabase
-- [1911-05-01 09:28:43 UTC,1921-05-01 09:28:43 UTC]

filterDbDateR ::
  [DatabaseItem] ->
  [UTCTime]
filterDbDateR [] = []
filterDbDateR (x : xs) = case x of
  DbDate ut -> ut : filterDbDateR xs
  _ -> filterDbDateR xs

-- >>> filterDbNumberR theDatabase
-- [9001,25]

filterDbNumberR ::
  [DatabaseItem] ->
  [Integer]
filterDbNumberR [] = []
filterDbNumberR (x : xs) = case x of
  DbNumber int -> int : filterDbNumberR xs
  _ -> filterDbNumberR xs

-- >>> mostRecentR theDatabase
-- 1921-05-01 09:28:43 UTC

mostRecentR ::
  [DatabaseItem] ->
  UTCTime
mostRecentR = maximum . filterDbDate

-- >>> sumDbR theDatabase
-- 9026

sumDbR ::
  [DatabaseItem] ->
  Integer
sumDbR = sum . filterDbNumberR

-- >>> avgDbR theDatabase
-- 4513.0

avgDbR ::
  [DatabaseItem] ->
  Double
avgDbR xs = fromIntegral (sumDbR xs) / fromIntegral leng
  where
    leng = length $ filterDbNumberR xs

-- Scans exercises

fibsR = take 20 $ 1 : scanl (+) 1 fibsR

fibsR' = takeWhile (< 100) $ 1 : scanl (+) 1 fibsR'

fibsN x = fibsR !! x

fac x = take (x + 1) $ scanl (*) 1 [1 ..]

stopVowelStopR :: String -> String -> [(Char, Char, Char)]
stopVowelStopR xs ys = go xs
  where
    vowelStop :: String -> String -> [(Char, Char)]
    vowelStop [] _ = []
    vowelStop (x : xs) ys = fmap (\y -> (x, y)) ys ++ vowelStop xs ys
    vs = vowelStop ys xs
    go :: String -> [(Char, Char, Char)]
    go [] = []
    go (x : xs') = fmap (\(y, z) -> (x, y, z)) vs ++ go xs'

stopVowelStopP :: String -> String -> [(Char, Char, Char)]
stopVowelStopP xs ys = go xs
  where
    vowelStop :: String -> String -> [(Char, Char)]
    vowelStop [] _ = []
    vowelStop (x : xs) ys = fmap (\y -> (x, y)) ys ++ vowelStop xs ys
    vs = vowelStop ys xs
    go :: String -> [(Char, Char, Char)]
    go [] = []
    go (x : xs') = if x == 'p' then fmap (\(y, z) -> (x, y, z)) vs ++ go xs' else go xs'

seekritFunc :: String -> Int
seekritFunc x =
  div
    (sum (map length (words x)))
    (length (words x))

myOrR :: [Bool] -> Bool
myOrR = foldr (||) False

myAnyR :: (a -> Bool) -> [a] -> Bool
myAnyR f = foldr (\x a -> f x || a) False

-- >>> myElemR 1 [2..10]
-- False

myElemR :: Eq a => a -> [a] -> Bool
myElemR x = foldr (\y a -> x == y) False

-- >>> myElemR1 1 [1..10]
-- True

myElemR1 :: Eq a => a -> [a] -> Bool
myElemR1 x = any (x ==)

-- >>> myReverseR "blah"
-- "halb"

myReverseR :: [a] -> [a]
myReverseR = foldl (flip (:)) []

-- >>> myMapR (+1) [1,2,3,4,5]
-- [2,3,4,5,6]

myMapR :: (a -> b) -> [a] -> [b]
myMapR f = foldr (\x a -> f x : a) []

-- >>> myFilterR (> 1) [1,2,3,4,5]
-- [2,3,4,5]

myFilterR :: (a -> Bool) -> [a] -> [a]
myFilterR f = foldr (\x a -> if f x then x : a else a) []

-- >>> squishR [[1],[2],[3],[4],[5]]
-- [1,2,3,4,5]

squishR :: [[a]] -> [a]
squishR = foldr (++) []

-- >>> squishMapR (\x -> [1, x, 3]) [2]
-- [1,2,3]

squishMapR :: (a -> [b]) -> [a] -> [b]
squishMapR f = squishR . myMapR f

squishAgainR :: [[a]] -> [a]
squishAgainR = squishMapR id

-- >>> myMaximumBy (\_ _ -> GT) [1..10]
-- 10

myMaximumByR ::
  Num a =>
  (a -> a -> Ordering) ->
  [a] ->
  a
myMaximumByR f = foldr (\x a -> if f x a == GT then x else a) 0

-- >>> myMinimumBy compare [1..10]
-- 1
myMinimumByR ::
  Num a =>
  (a -> a -> Ordering) ->
  [a] ->
  a
myMinimumByR f = foldr (\x a -> if f x a == GT then a else x) 0

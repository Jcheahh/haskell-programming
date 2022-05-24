module Ch8 where

import Data.Char
import Data.List (intercalate, intersperse)

{-
  1. d
  2. b
  3. a, b, c, d
  4. b
-}

{-
  go 15 2 0

  go (15 - 2) 2 (0 + 1)

  go 13 2 1
  go (13 - 2) 2 (1 + 1)

  go 11 2 2
  go (11 - 2) 2 (2 + 1)

  go 9 2 3
  go (9 - 2) 2 (3 + 1)

  go 7 2 4
  go (7 - 2) 2 (4 + 1)

  go 5 2 5
  go (5 - 2) 2 (5 + 1)

  go 3 2 6
  go (3 - 2) 2 (6 + 1)

  go 1 2 7

  | 1 < 2 = (7, 1)
  (7, 1)
-}

{-

  1. "woops mrow woohoo!"
  2. "1 mrow haha"
  3. "woops mrow 2 mrow haha"
  4. "woops mrow blue mrow haha"
  5. "pink mrow haha green mrow woops mrow blue"
  6. "are mrow Pugs mrow awesome"

-}

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x =
  fibonacci (x - 1) + fibonacci (x - 2)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = error "cant found"

digits :: Int -> [Int]
digits n = map digitToInt (filter isDigit (intersperse ' ' (show n)))

digits' :: Int -> [Int]
digits' n = go n []
  where
    go n acc
      | n == 0 = acc
      | otherwise =
        go (n `div` 10) $ (n `mod` 10) : acc

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" (map digitToWord (digits n))

-----------------------------------------------------------------
-- Revision

-- Intermission: Exercise
-- (+1) (+1) (+1) (+1) (+1) $ 5 = 10

-- 8.6

-- 1.
-- C,D

-- 2.
-- B,C,D

-- 3.
-- A,B,C,D

-- 4.
-- B

cattyConny' :: String -> String -> String
cattyConny' x y = x ++ " mrow " ++ y

flippy' :: String -> String -> String
flippy' = flip cattyConny'

appedCatty' :: String -> String
appedCatty' = cattyConny' "woops"

frappe' :: String -> String
frappe' = flippy' "haha"

-- 1.
-- "woops mrow woohoo"

-- 2.
-- "1 mrow haha"

-- 3.
-- "2 mrow woohoo mrow haha"

-- 4.
-- "woops mrow blue mrow haha"

-- 5.
-- "pink mrow haha mrow green mrow woops mrow blue"

-- 6.
-- "are mrow Pugs mrow awesome"

-- Recursion

-- 1.
-- (7,1)

-- 2.
-- (7,1)

-- 3.
-- (7,1)

-- >>> addToN 7
-- 28

-- 2.
addToN :: (Eq a, Num a) => a -> a
addToN 0 = 0
addToN n = n + (addToN (n -1))

-- 3.
multTWo :: (Integral a) => a -> a -> a
multTWo x y = go y 0
  where
    go y acc = if y > 0 then go (y -1) (x + acc) else acc

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise =
        go (n - d) d (count + 1)

dividedBy' :: Integral a => a -> a -> (a, a)
dividedBy' = divMod

mc91 :: Int -> Int
mc91 x
  | x > 100 = x - 10
  | otherwise = 91

digitToWord' :: Int -> String
digitToWord' n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = error "cant found"

d1 :: Int -> [Int]
d1 n = go n []
  where
    go x acc = if x == 0 then acc else go (x `div` 10) (x `mod` 10 : acc)

d1' :: Int -> [Int]
d1' n = map digitToInt $ filter isDigit (intersperse ' ' (show n))

-- >>> wordNumber' 12345
-- "one-two-three-four-five"

wordNumber' :: Int -> String
wordNumber' n = intercalate "-" $ map digitToWord' $ d1 n

wordNumber2 :: Int -> String
wordNumber2 n = concat $ intersperse "-" $ map digitToWord' $ d1 n

module Ch9 where

import Data.Bool (bool)
import Data.Char (isSpace, isUpper, toUpper)

enumFromTo' :: (Ord a, Enum a) => a -> a -> [a]
enumFromTo' x y = go x y []
  where
    go x y acc
      | x > y = reverse acc
      | x == y = reverse (x : acc)
      | otherwise = go (succ x) y (x : acc)

eftBool :: Bool -> Bool -> [Bool]
eftBool = enumFromTo'

eftOrd ::
  Ordering ->
  Ordering ->
  [Ordering]
eftOrd = enumFromTo'

eftInt :: Int -> Int -> [Int]
eftInt = enumFromTo'

eftChar :: Char -> Char -> [Char]
eftChar = enumFromTo'

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences =
  firstSen ++ secondSen
    ++ thirdSen
    ++ fourthSen

-- This is the result that putStrLn sentences should print:
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this:
myLines :: String -> [String]
myLines = breakWith '\n'

myWords :: String -> [String]
myWords = breakWith ' '

myFilter :: String -> [String]
myFilter xs = filter (\x -> x `notElem` ["the", "a", "an"]) (myWords xs)

breakWith a y = go y []
  where
    go y acc =
      case y of
        "" -> reverse acc
        ys@(y : ys') ->
          if y == a
            then go ys' acc
            else
              go
                (dropWhile (/= a) ys)
                (takeWhile (/= a) ys : acc)

-- We want myLines sentences to equal:
shouldEqual :: [[Char]]
shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

-- The main function here is a small test to ensure you’ve written
-- your function correctly:
main :: IO ()
main = do
  print $
    "Are they equal? "
      ++ show
        ( myLines sentences
            == shouldEqual
        )
  print $
    "Are they equal? "
      ++ show
        ( myLines' sentences
            == shouldEqual
        )

mine = [(x ^ 2, y ^ 3) | x <- [1 .. 5], x < 50, y <- [1 .. 5], y < 50]

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem b (x : xs) = if b == x then True else myElem b xs

myReverse :: [a] -> [a]
myReverse xs = go xs []
  where
    go [] acc = acc
    go (x : xs) acc = go xs (x : acc)

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

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

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' ::
  (a -> b -> c) ->
  [a] ->
  [b] ->
  [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-----------------------------------------------------------------
-- Revision

eFT :: (Enum a, Ord a) => a -> a -> [a]
eFT x y = reverse $ go x y []
  where
    go n m acc
      | n < m = go (succ n) m (n : acc)
      | n > m = acc
      | otherwise = m : acc

eftBool' :: Bool -> Bool -> [Bool]
eftBool' = eFT

eftOrd' ::
  Ordering ->
  Ordering ->
  [Ordering]
eftOrd' = eFT

eftInt' :: Int -> Int -> [Int]
eftInt' = eFT

eftChar' :: Char -> Char -> [Char]
eftChar' = eFT

myWords' :: String -> [String]
myWords' = checkEqual ' '

myLines' :: String -> [String]
myLines' = checkEqual '\n'

checkEqual :: (Eq a) => a -> [a] -> [[a]]
checkEqual target xs = go xs []
  where
    go [] acc = reverse acc
    go xs acc =
      if null (dropWhile (/= target) xs)
        then go (dropWhile (/= target) xs) (takeWhile (/= target) xs : acc)
        else go (tail (dropWhile (/= target) xs)) (takeWhile (/= target) xs : acc)

mySqrCube = length [(x ^ 2, y ^ 3) | x <- [1 .. 5], x < 50, y <- [1 .. 5], y < 50]

-- Exercise: Bottom madness

-- 1. ⊥
-- 2. [1]
-- 3. ⊥
-- 4. 3
-- 5. ⊥
-- 6. [2]
-- 7. ⊥
-- 8. [1]
-- 9. [1,3]
-- 10. ⊥

-- 1. 1
-- 2. 2
-- 3. 1
-- 4. 2
-- 5. 1
-- 6. 3
-- 7. 2

-- Exercise: More bottoms

-- 1. ⊥
-- 2. [1]
-- 3. ⊥
-- 4. A function that takes a list and check is it "aeiou"
isItMystery :: [Char] -> [Bool]
isItMystery = map (\x -> elem x "aeiou")

-- 5. (a) [1,4,9,16,25,36,49,64,81,100]
--    (b) [1,10,20]
--    (c) [15,15,15]
-- 6.
foldBool :: [Char] -> [Bool]
foldBool xs = map (bool False True) $ isItMystery xs

x = [x | x <- [1 .. 30], (x `rem` 3) == 0]

-- Exercises: Filtering
-- 1. [x | x <- [1 .. 30], (x `rem` 3) == 0]
-- 2. length [x | x <- [1 .. 30], (x `rem` 3) == 0]
-- 3.

isTAA :: [Char] -> Bool
isTAA "the" = False
isTAA "a" = False
isTAA "an" = False
isTAA _ = True

myFilter' :: String -> [String]
myFilter' xs = filter isTAA $ words xs

zipy :: [a] -> [b] -> [(a, b)]
zipy [] _ = []
zipy _ [] = []
zipy (x : xs) (y : ys) = (x, y) : zipy xs ys

zipyWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipyWith _ [] _ = []
zipyWith _ _ [] = []
zipyWith f (x : xs) (y : ys) = f x y : zipyWith f xs ys

toHello :: String -> String
toHello = filter isUpper

capFirst :: String -> String
capFirst [] = ""
capFirst xs = toUpper (head xs) : tail xs

capAll :: String -> String
capAll [] = []
capAll (x : xs) = toUpper x : capAll xs

capHead :: String -> Char
capHead [] = ' '
capHead xs = toUpper (head xs)

myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x : xs) = x && myAnd' xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x : xs) = x || myOr' xs

-- >>>  myAny' odd [1, 3, 5]
-- True

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' f (x : xs) = f x || myAny' f xs

-- >>> myElem' 1 [1..10]
-- True

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' t (x : xs) = t == x || myElem' t xs

-- >>> myReverse' "blah"
-- "halb"

myReverse' :: [a] -> [a]
myReverse' xs = go xs []
  where
    go [] acc = acc
    go (y : ys) acc = go ys (y : acc)

squish' :: [[a]] -> [a]
squish' = foldr (++) []

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f xs = squish' $ fmap f xs

squishAgain' :: [[a]] -> [a]
squishAgain' = squishMap' id

-- >>> myMaximumBy' compare  [1, 53, 9001, 10]
-- 9001

myMaximumBy' ::
  (Ord a, Num a) =>
  (a -> a -> Ordering) ->
  [a] ->
  a
myMaximumBy' f xs = go xs 0
  where
    go [] big = big
    go (y : ys) big = if f big y == GT then go ys big else go ys y

myMinimumBy' ::
  (Ord a, Num a) =>
  (a -> a -> Ordering) ->
  [a] ->
  a
myMinimumBy' f xs = go xs 0
  where
    go [] small = small
    go (y : ys) small = if f small y == LT then go ys small else go ys y

myMaximum' :: (Ord a, Num a) => [a] -> a
myMaximum' = myMaximumBy' compare

myMinimum' :: (Ord a, Num a) => [a] -> a
myMinimum' = myMinimumBy' compare

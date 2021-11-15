mTh x y z = x * y * z

{-

  1. a, b, c, d
  2. a, d
  3. addOneIfOdd n = case odd n of
      True -> f n
      False -> n
      where
        f = \x -> x + 1

     addFive = \x -> \y -> (if x > y then y else x) + 5

     mflip f x y = f y x

-}

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where
    f = \x -> x + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

{-
  1.  (a) k :: (a, b) -> a
      (b) yes
      (c) k3
-}

-- 2.
f2 ::
  (a, b, c) ->
  (d, e, f) ->
  ((a, d), (c, f))
f2 (a, b, c) (d, e, f) = ((a, d), (c, f))

-- 1. The following should return x when x is greater than y:
functionC x y =
  case x > y of
    True -> x
    False -> y

-- 2. The following will add 2 to even numbers and otherwise simply
-- return the input value:
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

-- The next exercise doesn’t have all the cases covered. See if you
-- can fix it.
-- 3. The following compares a value, x, to 0 and returns an indicator
-- for whether x is a positive number or negative number. What if
-- x is 0? You may need to play with the compare function a bit to
-- find what to do:
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = flip dodgy 2

avgGrade ::
  (Fractional a, Ord a) =>
  a ->
  Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where
    y = x / 100

-- 3. B
-- 4. pal takes a list and return boolean
-- 5. pal :: [a] -> Bool
-- 6. C
-- 7. Num
-- 8. number :: Num a => a -> a

{-
  1. b, d
  2. b, d
  3. a, d
  4. b
  5. a, d
-}

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

-- tensDigit x = b
--   where
--     (a, b) = x `divMod` 10

hunsD x = d2
  where
    d = x `div` 100
    d2 = d `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y bool = case (x, y, bool) of
  (x, _, False) -> x
  (_, y, True) -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print (roundTrip 4 :: Integer)
  print (id 4)
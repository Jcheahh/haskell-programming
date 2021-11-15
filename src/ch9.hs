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
main =
  print $
    "Are they equal? "
      ++ show
        ( myLines sentences
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

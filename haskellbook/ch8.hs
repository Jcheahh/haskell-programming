import Data.Char
import Data.List (intersperse)

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
  | n == 9 = "night"
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

module Ch4 where

data Mood = Blah | Woot deriving (Show)

{-
    1. Mood
    2. Blah | Woot
    3. changeMood :: Mood -> Mood
    4. changeMood Blah = Woot
       changeMood Woot = Blah
-}

-- 5.
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs = abs

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = (,) ((,) (snd x) (snd y)) ((,) (fst x) (fst y))

{-
  1. c
  2. a
  3. b
  4. c
-}
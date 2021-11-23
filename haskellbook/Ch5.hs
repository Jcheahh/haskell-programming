module Ch5 where

{-
    1. a
    2. d
    3. d
    4. c
    5. a
    6. e
    7. d
    8. a
    9. c
-}

{-
  1. string -> string
  2. Fractional b => a -> b
  3. Int -> String
  4. Int -> Bool
  5. a -> Bool
-}

{-
  1.
    a. 54 Num a => a
    b. (0,"doge") (Num a) => (a, String)
    c. (0 :: Integer ,"doge") (Integer, String)
    d. False
    e. 5
    f. Int -> Int -> Bool
  2. w :: Num a => a
  3. z :: Num a => a -> a
  4. f :: Fractional a => a
  5. f :: String
-}

{-
  1. bigNum x = (^) 5 $ x
     wahoo = bigNum $ 10
  2. Correct
  3.  a = (+)
      b = 5
      c = a 10
      d = c 200
  4. a = 12 + b
     b = 10000 * c
     c = 1
-}

{-

  1. 1 -> 0 -> 2 -> 2
  2. 0 -> 3 -> 3
  3. 0 -> 1 -> 3
  4. 0 -> 0 -> 3

-}

{-

  1.  functionH :: [a] -> a
      functionH (x:_) = x

  2.  functionC :: a -> a -> Bool
      functionC x y =
      if (x > y) then True else False

  3.  functionS :: (a, b) -> b
      functionS (x, y) = y

-}

{-
  1.  i :: a -> a
      i x = x

  2.  c :: a -> b -> a
      c x _ = x

  3.  c'' :: b -> a -> b
      c'' x _ = x

  4.  c' :: a -> b -> b
      c' _ y = y

  5.  r :: [a] -> [a]
      r xs = xs

  6.  co :: (b -> c) -> (a -> b) -> a -> c
      co f1 f2 x = f1 . f2 $ x

  7.  a :: (a -> c) -> a -> a
      a _ x = x

  8.  a' :: (a -> b) -> a -> b
      a' f1 x = f1 $ x
-}

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if x < y then fstString x else sndString y
  where
    x = "Singin"
    y = "Somewhere"

main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate 2 -1)
  print ((+) 0 blah)
  where
    blah = negate 1

-- 1.
f' :: Int -> String
f' = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f'

-- 2.
data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- 3.
data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f1 f2 x = a
  where
    f1' = f1 x
    (a, b) = f2 f1'
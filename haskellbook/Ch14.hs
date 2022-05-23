{-# LANGUAGE ScopedTypeVariables #-}

module Ch14 where

import Ch11 (capitalizeWord)
import Ch8
  ( digitToWord,
    digits,
    wordNumber,
  )
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Trivial
  = Trivial
  deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen =
  return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

main :: IO ()
main = do
  sample trivialGen

data Identity a
  = Identity a
  deriving (Eq, Show)

identityGen ::
  Arbitrary a =>
  Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance
  Arbitrary a =>
  Arbitrary (Identity a)
  where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

identityGenString :: Gen (Identity String)
identityGenString = identityGen

data Pair a b
  = Pair a b
  deriving (Eq, Show)

pairGen ::
  ( Arbitrary a,
    Arbitrary b
  ) =>
  Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance
  ( Arbitrary a,
    Arbitrary b
  ) =>
  Arbitrary (Pair a b)
  where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

sumGenEqual ::
  ( Arbitrary a,
    Arbitrary b
  ) =>
  Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof
    [ return $ First a,
      return $ Second b
    ]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls ::
  ( Arbitrary a,
    Arbitrary b
  ) =>
  Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency
    [ (10, return $ First a),
      (1, return $ Second b)
    ]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

main1 :: IO ()
main1 = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $
      do
        wordNumber 100
        `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001
        `shouldBe` "nine-zero-zero-one"
  describe "halfIdentify" $ do
    it
      "x is always\
      \ equal to x"
      $ do
        property $ \x -> halfIdentity x `shouldBe` (x :: Double)

-- 1.
half x = x / 2

halfIdentity x = (* 2) $ half x

prop_halfIndentity :: Double -> Bool
prop_halfIndentity x = halfIdentity x == x

runQc :: IO ()
runQc = quickCheck prop_halfIndentity

-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

runLO :: IO ()
runLO = do
  quickCheck (\(xs :: [Int]) -> listOrdered $ sort xs)
  quickCheck (\(xs :: [String]) -> listOrdered $ sort xs)
  quickCheck (\(xs :: String) -> listOrdered $ sort xs)

-- 3.
plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

-- 4.
multiplyAssociative :: Int -> Int -> Int -> Bool
multiplyAssociative x y z =
  x * (y * z) == (x * y) * z

multiplyCommutative :: Int -> Int -> Bool
multiplyCommutative x y =
  x * y == y * x

-- 5.
quotRem' :: Int -> Int -> Bool
quotRem' x y =
  y == 0 || (quot x y) * y + (rem x y) == x

divMod' :: Int -> Int -> Bool
divMod' x y =
  y == 0 || (div x y) * y + (mod x y) == x

-- 6.
-- NOT ASSOCIATIVE!!!
-- NOT COMMUTATIVE!!!
-- powerAssociative :: Int -> Int -> Int -> Bool
-- powerAssociative x y z =
--   x ^ (y ^ z) == (x ^ y) ^ z

-- powerCommutative :: Int -> Int -> Bool
-- powerCommutative x y =
--   x ^ y == y ^ x

-- 7.
reverseList :: (Eq a) => [a] -> Bool
reverseList xs = (reverse . reverse) xs == id xs

-- 8.
prop_dollerSign :: Fun Int String -> Int -> Bool
prop_dollerSign (Fn f) a = (f $ a) == f a

prop_dotSign :: Fun String Int -> Fun Int String -> Int -> Bool
prop_dotSign (Fn f) (Fn g) a = f (g a) == (f . g) a

-- 9.
prop_foldrCons :: (Eq a) => [a] -> Bool
prop_foldrCons xs = foldr (:) [] xs == xs ++ []

-- Not equal
prop_foldrConcat :: [String] -> Bool
prop_foldrConcat xs = foldr (++) [] xs == concat xs

-- 10.
-- Not equal
prop_takeLength :: Int -> [a] -> Bool
prop_takeLength n xs = length (take n xs) == n

prop_readShow :: (Show a, Read a, Eq a) => a -> Bool
prop_readShow x = read (show x) == x

square :: Floating a => a -> a
square x = x * x

squareIdentity :: (Floating a, Eq a) => a -> Bool
squareIdentity x = (square . sqrt) x == x

twice f = f . f

fourTimes = twice . twice

prop_idempotenceCapitalize :: String -> Bool
prop_idempotenceCapitalize x =
  ( capitalizeWord x
      == twice capitalizeWord x
  )
    && ( capitalizeWord x
           == fourTimes capitalizeWord x
       )

prop_idempotenceSort :: [Int] -> Bool
prop_idempotenceSort x =
  ( sort x
      == twice sort x
  )
    && ( sort x
           == fourTimes sort x
       )

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFool' :: Gen Fool
genFool' =
  frequency
    [ (1, return Frue),
      (2, return Fulse)
    ]

runAC :: IO ()
runAC = do
  quickCheck plusAssociative
  quickCheck plusCommutative
  quickCheck multiplyAssociative
  quickCheck multiplyCommutative
  quickCheck quotRem'
  quickCheck divMod'
  -- quickCheck powerAssociative
  -- quickCheck powerCommutative
  quickCheck (reverseList :: [Int] -> Bool)
  quickCheck prop_dollerSign
  quickCheck prop_dotSign
  quickCheck (prop_foldrCons :: [Int] -> Bool)
  quickCheck (prop_readShow :: Int -> Bool)
  -- quickCheck (squareIdentity :: Double -> Bool) -- doesn't hold negative value
  quickCheck prop_idempotenceCapitalize
  quickCheck prop_idempotenceSort

-----------------------------------------------------------------
-- Revision
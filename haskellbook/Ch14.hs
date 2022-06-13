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

data IdentityR a
  = IdentityR a
  deriving (Eq, Show)

identityGenR ::
  Arbitrary a =>
  Gen (IdentityR a)
identityGenR = do
  a <- arbitrary
  return (IdentityR a)

instance Arbitrary a => Arbitrary (IdentityR a) where
  arbitrary = identityGenR

identityGenIntR :: Gen (IdentityR Int)
identityGenIntR = arbitrary

data PairR a b
  = PairR a b
  deriving (Eq, Show)

pairGenR ::
  ( Arbitrary a,
    Arbitrary b
  ) =>
  Gen (PairR a b)
pairGenR = do
  a <- arbitrary
  b <- arbitrary
  return (PairR a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PairR a b) where
  arbitrary = pairGenR

pairGenIntStringR :: Gen (PairR Int String)
pairGenIntStringR = arbitrary

data SumR a b
  = FirstR a
  | SecondR b
  deriving (Eq, Show)

sumGenEqualR :: (Arbitrary a, Arbitrary b) => Gen (SumR a b)
sumGenEqualR = do
  a <- arbitrary
  b <- arbitrary
  oneof
    [ return $ FirstR a,
      return $ SecondR b
    ]

sumGenCharIntR :: Gen (SumR Char Int)
sumGenCharIntR = sumGenEqualR

sumGenFirstPlsR ::
  ( Arbitrary a,
    Arbitrary b
  ) =>
  Gen (SumR a b)
sumGenFirstPlsR = do
  a <- arbitrary
  b <- arbitrary
  frequency
    [ (10, return $ FirstR a),
      (1, return $ SecondR b)
    ]

sumGenCharIntFirstR :: Gen (SumR Char Int)
sumGenCharIntFirstR = sumGenFirstPlsR

mainR :: IO ()
mainR = hspec $ do
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
  describe "halfIdentity" $ do
    it "return x for halfIdentity x" $
      do
        property (\(x :: Double) -> x `shouldBe` halfIdentity x)
  describe "listOrdered" $ do
    it "for any list you apply (sort) to, this property should hold" $
      do
        property (\(x :: [Int]) -> listOrdered $ sort x)

plusAssociativeR :: Int -> Int -> Int -> Bool
plusAssociativeR x y z =
  x + (y + z) == (x + y) + z

plusCommutativeR :: Int -> Int -> Bool
plusCommutativeR x y =
  x + y == y + x

-- 4.
multiplyAssociativeR :: Int -> Int -> Int -> Bool
multiplyAssociativeR x y z =
  x * (y * z) == (x * y) * z

multiplyCommutativeR :: Int -> Int -> Bool
multiplyCommutativeR x y =
  x * y == y * x

-- 5.
quotRemR :: Int -> Int -> Bool
quotRemR x y =
  y == 0 || (quot x y) * y + (rem x y) == x

divModR :: Int -> Int -> Bool
divModR x y =
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
prop_reverseR :: (Eq a) => [a] -> Bool
prop_reverseR xs = (reverse . reverse) xs == id xs

-- 8.
prop_dollerSignR :: Fun Int String -> Int -> Bool
prop_dollerSignR (Fn f) a = (f $ a) == f a

prop_dotSignR :: Fun String Int -> Fun Int String -> Int -> Bool
prop_dotSignR (Fn f) (Fn g) a = f (g a) == (f . g) a

-- 9.
prop_foldRConsR :: (Eq a) => [a] -> Bool
prop_foldRConsR xs = foldr (:) [] xs == xs ++ []

prop_foldRAppendR :: (Eq a) => [[a]] -> Bool
prop_foldRAppendR xs = foldr (++) [] xs == concat xs

-- 10.
-- No. Because the xs possibly to be a empty list

-- 11.
prop_readShowR :: (Read a, Show a, Eq a) => a -> Bool
prop_readShowR x = read (show x) == x

squareR x = x * x

squareIdentityR = squareR . sqrt

-- Failure
-- Doesn't hold negative value

prop_idempotenceCapitalizeR :: String -> Bool
prop_idempotenceCapitalizeR x =
  ( capitalizeWord x
      == twice capitalizeWord x
  )
    && ( capitalizeWord x
           == fourTimes capitalizeWord x
       )

prop_idempotenceSortR :: Ord a => [a] -> Bool
prop_idempotenceSortR x =
  ( sort x
      == twice sort x
  )
    && ( sort x
           == fourTimes sort x
       )

runQcR :: IO ()
runQcR = do
  quickCheck plusAssociativeR
  quickCheck plusCommutativeR
  quickCheck multiplyAssociativeR
  quickCheck multiplyCommutativeR
  quickCheck quotRemR
  quickCheck divModR
  quickCheck (prop_reverseR :: [Int] -> Bool)
  quickCheck prop_dollerSignR
  quickCheck prop_dotSignR
  quickCheck (prop_foldRConsR :: [Int] -> Bool)
  quickCheck (prop_foldRAppendR :: [[Int]] -> Bool)
  quickCheck (prop_readShowR :: Int -> Bool)
  quickCheck prop_idempotenceCapitalizeR
  quickCheck (prop_idempotenceSortR :: [Int] -> Bool)

data FoolR
  = FulseR
  | FrueR
  deriving (Eq, Show)

genFoolR :: Gen FoolR
genFoolR = oneof [return FulseR, return FrueR]

genFoolR' :: Gen FoolR
genFoolR' = elements [FulseR, FrueR]

genFoolR1 :: Gen FoolR
genFoolR1 = frequency [(2, return FulseR), (1, return FrueR)]

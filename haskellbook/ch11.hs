{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

capitalFirst :: String -> String
capitalFirst [] = []
capitalFirst (x : xs) = toUpper x : xs

{-
  1. 8
  2. 16
  3. 256
  4. 8
  5. 16
  6. 65536
-}

data Price
  = Price Integer
  deriving (Eq, Show)

data Size
  = Size Integer
  deriving (Eq, Show)

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars [] = []
areCars (x : xs) = isCar x : areCars xs

areCars1 :: [Vehicle] -> [Bool]
areCars1 = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x
getManu _ = error "cannot find manufacturer"

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (x, y) = tooMany x || length y == 42

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y == 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany $ x + y

type Gardener = String

data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem,
    lang :: ProgLang
  }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux,
    OpenBSDPlusNevermindJustBSDStill,
    Mac,
    Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = go allOperatingSystems allLanguages
  where
    go [] _ = []
    go (x : xs) ys = map (\y -> Programmer {os = x, lang = y}) ys ++ go xs ys

data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

convert :: Quantum -> Bool
convert Yes = True
convert No = True
convert Both = True

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = False

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = False
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes = False
convert3 No = True
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = False

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree ::
  (a -> b) ->
  BinaryTree a ->
  BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node
    (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected =
  Node
    (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup OK!"
    else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree ::
  (a -> b -> b) ->
  b ->
  BinaryTree a ->
  b
foldTree _ x Leaf = x
foldTree f x (Node left a right) = foldTree f (f a (foldTree f x left)) right

isSubseqOf ::
  (Eq a) =>
  [a] ->
  [a] ->
  Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x : xs') (y : ys) = if x == y then isSubseqOf xs' ys else isSubseqOf xs ys

capitalizeWords ::
  String ->
  [(String, String)]
capitalizeWords xs = zip bs as
  where
    bs = words xs
    as = map capitalFirst bs

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph xs = unwords $ map capitalizeWord (words xs)

-- -----------------------------------------

-- | 1      | 2 ABC | 3 DEF  |
-- -----------------------------------------
-- | 4 GHI  | 5 JKL | 6 MNO  |
-- -----------------------------------------
-- | 7 PQRS | 8 TUV | 9 WXYZ |
-- -----------------------------------------
-- | * ^    | 0 + _ | # .,   |
-- -----------------------------------------

-- | 2 -> 'A'
-- | 22 -> 'B'
-- | 222 -> 'C'
-- | 2222 -> '2'
-- | 22222 -> 'A
data DaPhone = DaPhone [(Digit, String)]

allPossiple :: DaPhone
allPossiple =
  DaPhone
    [ ('1', " "),
      ('2', "abc"),
      ('3', "def"),
      ('4', "ghi"),
      ('5', "jkl"),
      ('6', "mno"),
      ('7', "pqrs"),
      ('8', "tuv"),
      ('9', "wxyz"),
      ('*', "*^"),
      ('0', " +_"),
      ('#', "#.,")
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

type Digit = Char

type Presses = Int

reverseTaps ::
  DaPhone ->
  Char ->
  [(Digit, Presses)]
reverseTaps (DaPhone xs) b =
  if isLower b
    then go xs b
    else upperWord : go xs (toLower b)
  where
    upperWord = ('*', 1)
    go [] b = error "error"
    go ((first, second) : xs) b =
      if elem b second
        then [(first, 1 + fromMaybe (error "xxx") (elemIndex b second))]
        else go xs b

cellPhonesDead ::
  DaPhone ->
  String ->
  [(Digit, Presses)]
-- cellPhonesDead _ [] = []
-- cellPhonesDead xs (y : ys) = reverseTaps xs y ++ cellPhonesDead xs ys
cellPhonesDead d = concatMap $ reverseTaps d

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps ((first, second) : xs) = second + fingerTaps xs

mostPopular :: (Ord a) => [a] -> a
mostPopular xs = go xs Map.empty
  where
    go :: (Ord a) => [a] -> Map.Map a Integer -> a
    go [] map =
      fst $
        Map.foldrWithKey
          ( \k v (k', v') ->
              if v > v'
                then (k, v)
                else (k', v')
          )
          (error ":(", 0)
          map
    go (x : xs) map =
      go xs $ Map.insertWith (+) x 1 map

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular

coolestLtr :: [String] -> Char
coolestLtr xs = mostPopularLetter (concat xs)

coolestWord :: [String] -> String
coolestWord = mostPopular

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add a b) = (+) (eval a) (eval b)

-- Expected output:
-- Prelude> printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
-- Prelude> a1 = Add (Lit 9001) (Lit 1)
-- Prelude> a2 = Add a1 (Lit 20001)
-- Prelude> a3 = Add (Lit 1) a2
-- Prelude> printExpr a3
-- "1 + 9001 + 1 + 20001"

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b

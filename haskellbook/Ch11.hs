{-# LANGUAGE FlexibleInstances #-}

module Ch11 where

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

-----------------------------------------------------------------
-- Revision

-- Exercises: Dog types

-- 1. type constructor
-- 2. * -> *
-- 3. *
-- 4. Doggies Int
-- 5. Doggies Integer
-- 6. Doggies String
-- 7. type constructor
-- 8. a -> DogueDeBordeaux a
-- 9. DogueDeBordeaux String

data PriceR
  = PriceR Integer
  deriving (Eq, Show)

data SizeR
  = Small
  | Medium
  | Big
  deriving (Eq, Show)

data ManufacturerR
  = MiniR
  | MazdaR
  | TataR
  deriving (Eq, Show)

data AirlineR
  = PapuAirR
  | CatapultsR'UsR
  | TakeYourChancesUnitedR
  deriving (Eq, Show)

data VehicleR
  = CarR ManufacturerR PriceR
  | PlaneR AirlineR SizeR
  deriving (Eq, Show)

myCarR = CarR MiniR (PriceR 14000)

urCarR = CarR MazdaR (PriceR 20000)

clownCarR = CarR TataR (PriceR 7000)

dogeR = PlaneR PapuAirR

isCarR :: VehicleR -> Bool
isCarR (CarR _ _) = True
isCarR _ = False

isPlaneR :: VehicleR -> Bool
isPlaneR (PlaneR _ _) = True
isPlaneR _ = False

areCarRs :: [VehicleR] -> [Bool]
areCarRs = map isCarR

getManuR :: VehicleR -> ManufacturerR
getManuR (CarR manu _) = manu
getManuR _ = error "No manu"

-- Exercises: Cardinality

-- 1. 1
-- 2. 3
-- 3. 65536
-- 4. infinity cardinality for Integer
-- 5. 2 ^ 8

-- Exercises: For example

-- 1. Example
-- 2. instance Show Example
-- 3. Int -> Example

class TooManyR a where
  tooManyR :: a -> Bool

instance TooManyR Int where
  tooManyR n = n > 42

instance TooManyR (Int, String) where
  tooManyR (n, m) = tooManyR n

instance TooManyR (Int, Int) where
  tooManyR (n, m) = tooManyR (n + m)

instance (Num a, TooManyR a) => TooManyR (a, a) where
  tooManyR (n, m) = tooManyR (n + m)

-- Exercises: Pity the Bool

-- 1. 4
-- 2. 258

type GardenerR = String

data GardenR
  = GardeniaR GardenerR
  | DaisyR GardenerR
  | RoseR GardenerR
  | LilacR GardenerR
  deriving (Show)

data OperatingSystemR
  = GnuPlusLinuxR
  | OpenBSDPlusNevermindJustBSDStillR
  | MacR
  | WindowsR
  deriving (Eq, Show)

data ProgLangR
  = HaskellR
  | AgdaR
  | IdrisR
  | PureScriptR
  deriving (Eq, Show)

data ProgrammerR = ProgrammerR
  { osR :: OperatingSystemR,
    langR :: ProgLangR
  }
  deriving (Eq, Show)

allOperatingSystemsR :: [OperatingSystemR]
allOperatingSystemsR =
  [ GnuPlusLinuxR,
    OpenBSDPlusNevermindJustBSDStillR,
    MacR,
    WindowsR
  ]

allLanguagesR :: [ProgLangR]
allLanguagesR =
  [HaskellR, AgdaR, IdrisR, PureScriptR]

allProgrammersR :: [ProgrammerR]
allProgrammersR = go allOperatingSystemsR allLanguagesR
  where
    go [] ys = []
    go (x : xs) ys = fmap (\l -> ProgrammerR {osR = x, langR = l}) ys ++ go xs ys

convert1' :: Quantum -> Bool
convert1' Yes = True
convert1' No = True
convert1' Both = True

convert2' :: Quantum -> Bool
convert2' Yes = True
convert2' No = True
convert2' Both = False

convert3' :: Quantum -> Bool
convert3' Yes = True
convert3' No = False
convert3' Both = True

convert4' :: Quantum -> Bool
convert4' Yes = False
convert4' No = True
convert4' Both = True

convert5' :: Quantum -> Bool
convert5' Yes = False
convert5' No = False
convert5' Both = False

convert6' :: Quantum -> Bool
convert6' Yes = False
convert6' No = False
convert6' Both = True

convert7' :: Quantum -> Bool
convert7' Yes = False
convert7' No = True
convert7' Both = False

convert8' :: Quantum -> Bool
convert8' Yes = True
convert8' No = False
convert8' Both = False

-- Exercises: The Quad

-- 1. (4 + 4) 8
-- 2. (4 * 4) 16
-- 3. (4 ^ 4) 256
-- 4. ((2 * 2) * 2) 8
-- 5. ((2 ^ 2) ^ 2) 16
-- 6. ((2 ^ 4) ^ 4) 65536

mapTreeR ::
  (a -> b) ->
  BinaryTree a ->
  BinaryTree b
mapTreeR _ Leaf = Leaf
mapTreeR f (Node left a right) =
  Node (mapTreeR f left) (f a) (mapTreeR f right)

testTreeR :: BinaryTree Integer
testTreeR =
  Node
    (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpectedR =
  Node
    (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

mapOkayR =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup OK!"
    else error "test failed!"

preorderR :: BinaryTree a -> [a]
preorderR Leaf = []
preorderR (Node left x right) = [x] ++ preorderR left ++ preorderR right

inorderR :: BinaryTree a -> [a]
inorderR Leaf = []
inorderR (Node left x right) = inorderR left ++ [x] ++ inorderR right

postorderR :: BinaryTree a -> [a]
postorderR Leaf = []
postorderR (Node left x right) = postorderR left ++ postorderR right ++ [x]

testTreeR' :: BinaryTree Integer
testTreeR' =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorderR :: IO ()
testPreorderR =
  if preorderR testTreeR' == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorderR :: IO ()
testInorderR =
  if inorderR testTreeR' == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorderR :: IO ()
testPostorderR =
  if postorderR testTreeR' == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"

mainR :: IO ()
mainR = do
  testPreorderR
  testInorderR
  testPostorderR

-- 11.18

-- 1. A
-- 2. C
-- 3. B
-- 4. C

isSubseqOfR ::
  (Eq a) =>
  [a] ->
  [a] ->
  Bool
isSubseqOfR [] _ = True
isSubseqOfR _ [] = False
isSubseqOfR xs'@(x : xs) ys'@(y : ys)
  | x == y = isSubseqOfR xs ys
  | otherwise = isSubseqOfR xs' ys

capitalizeWordsR ::
  String ->
  [(String, String)]
capitalizeWordsR xs = map (\x -> (x, capitalizeWordR x)) (words xs)

capitalizeWordR :: String -> String
capitalizeWordR [] = []
capitalizeWordR (x : xs) = toUpper x : xs

isLast :: String -> Bool
isLast xs = last xs == '.'

capitalizeParagraphR :: String -> String
capitalizeParagraphR xs = go (words xs) [] []
  where
    go [] _ acc = unwords acc
    go (x : xs) reco acc
      | isLast x && null reco = go xs [] (acc ++ [capitalizeWordR x])
      | isLast x && not (null reco) = go xs [] (acc ++ [capitalizeWordR (head reco)] ++ tail reco ++ [x])
      | null reco = go xs (reco ++ [capitalizeWordR x]) acc
      | otherwise = go xs (reco ++ [x]) acc

data DaPhoneR = DaPhoneR [(Digit, String)]

allPossipleR :: DaPhoneR
allPossipleR =
  DaPhoneR
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

convoR :: [String]
convoR =
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

type DigitR = Char

type PressesR = Int

reverseTapsR ::
  DaPhoneR ->
  Char ->
  [(Digit, Presses)]
reverseTapsR (DaPhoneR xs) x =
  if isLower x
    then go xs x
    else upperX : go xs (toLower x)
  where
    upperX = ('*', 1)
    go [] _ = error "Not found"
    go ((key, value) : kvs) target =
      if target `elem` value
        then [(key, 1 + fromMaybe (error "Not found") (elemIndex target value))]
        else go kvs target

cellPhonesDeadR ::
  DaPhoneR ->
  String ->
  [(Digit, Presses)]
cellPhonesDeadR dp = concatMap $ reverseTapsR dp

fingerTapsR :: [(Digit, Presses)] -> Presses
fingerTapsR [] = 0
fingerTapsR ((key, value) : kvs) = value + fingerTapsR kvs

mostPopularR :: Ord a => [a] -> a
mostPopularR xs = go xs Map.empty
  where
    go [] map =
      fst $
        Map.foldrWithKey
          (\k v (k', v') -> if v > v' then (k, v) else (k', v'))
          (error "", 0)
          map
    go (x : xs) map = go xs $ Map.insertWith (+) x 1 map

mostPopularLetterR :: String -> Char
mostPopularLetterR = mostPopularR

coolestLtrR :: [String] -> Char
coolestLtrR = mostPopularLetterR . concat

coolestWordR :: [String] -> String
coolestWordR = mostPopularR

data ExprR
  = LitR Integer
  | AddR ExprR ExprR

evalR :: ExprR -> Integer
evalR (LitR int) = int
evalR (AddR exp exp1) = evalR exp + evalR exp1

printExprR :: ExprR -> String
printExprR (LitR int) = show int
printExprR (AddR exp exp1) = printExprR exp ++ " + " ++ printExprR exp1

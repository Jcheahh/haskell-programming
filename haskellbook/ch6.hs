import Data.List

f1' :: RealFrac a => a
f1' = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- 1. It’s not a typo, we’re just being cute with the name:
data TisAnInteger
  = TisAn Integer

instance Eq TisAnInteger where
  TisAn a == TisAn a' = a == a'

-- 2.
data TwoIntegers
  = Two Integer Integer

instance Eq TwoIntegers where
  Two a b == Two a' b' = a == a' && b == b'

-- 3.
data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt a == TisAnInt a' = a == a'
  TisAString x == TisAString x' = x == x'
  TisAnInt _ == TisAString _ = False
  TisAString _ == TisAnInt _ = False

-- 4.
data Pair a
  = Pair a a

instance Eq a => Eq (Pair a) where
  Pair a b == Pair a' b' = a == a' && b == b'

-- 5.
data Tuple a b
  = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'

-- 6.
data Which a
  = ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne a == ThisOne a' = a == a'
  ThatOne x == ThatOne x' = x == x'
  ThisOne _ == ThatOne _ = False
  ThatOne _ == ThisOne _ = False

-- 7.
data EitherOr a b
  = Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello a == Hello a' = a == a'
  Goodbye a == Goodbye a' = a == a'
  Hello _ == Goodbye _ = False
  Goodbye _ == Hello _ = False

{-
  1. a, b, c
  2. a, b
  3. a
  4. c
  5. a
-}

-- 1. cannot type check because no Show instance

data Person = Person Bool deriving (Show)

-- instance Show Person where
--   show _ = "Haha"

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. cannot type check because no Eq instance

data Mood'
  = Blah'
  | Woot'
  deriving (Show, Eq)

settleDown x =
  if x == Woot'
    then Blah'
    else x

{-
  3. (a) Blah' | Woot'
     (b) cant work because different type
     (c) return false because Blah' < Woot'
-}

-- 4. Does, s1 become function takes one argument
type Subject = String

type Verb = String

type Object = String

data Sentence
  = Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

data Rocks
  = Rocks String
  deriving (Eq, Show)

data Yeah
  = Yeah Bool
  deriving (Eq, Show)

data Papu
  = Papu Rocks Yeah
  deriving (Eq, Show)

-- 1. Cannot
-- phew = Papu "chases" True
-- 2. Can
truth =
  Papu
    (Rocks "chomskydoz")
    (Yeah True)

-- 3. Can
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. Cannot
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

{- 1. NO
   2. NO
   3. YES
   4. YES
   5. YES
   6. YES
   7. NO
   8. NO
   9. YES
   10. YES
   11. NO
-}

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith ::
  Num b =>
  (a -> b) ->
  Integer ->
  a ->
  b
arith f num x = f x + fromInteger num
{-# LANGUAGE InstanceSigs #-}

module Ch22 where

import Control.Applicative
import Data.Char (toUpper)
import Data.Maybe

boop = (* 2)

doop = (+ 10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = do
  c <- cap
  r <- rev
  return (c, r)

tupled' :: [Char] -> ([Char], [Char])
tupled' =
  rev >>= \r ->
    cap >>= \c ->
      return (r, c)

newtype HumanName
  = HumanName String
  deriving (Eq, Show)

newtype DogName
  = DogName String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }
  deriving (Eq, Show)

pers :: Person
pers =
  Person
    (HumanName "Big Bird")
    (DogName "Barkley")
    (Address "Sesame Street")

chris :: Person
chris =
  Person
    (HumanName "Chris Allen")
    (DogName "Papu")
    (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

(<$->>) ::
  (a -> b) ->
  (r -> a) ->
  (r -> b)
(<$->>) = (<$>)

(<*->>) ::
  (r -> a -> b) ->
  (r -> a) ->
  (r -> b)
(<*->>) = (<*>)

getDogR' :: Person -> Dog
getDogR' =
  Dog <$->> dogName <*->> address

getDogR'' :: Person -> Dog
getDogR'' =
  liftA2 Dog dogName address

myLiftA2 ::
  Applicative f =>
  (a -> b -> c) ->
  f a ->
  f b ->
  f c
myLiftA2 f x y = f <$> x <*> y

newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) ::
    Reader r (a -> b) ->
    Reader r a ->
    Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure

  (>>=) ::
    Reader r a ->
    (a -> Reader r b) ->
    Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

f :: (r -> a -> b) -> (r -> a) -> r -> b
f rab ra r = rab r (ra r)

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- asks dogName
  addy <- asks address
  return $ Dog name addy

getDogRM'' :: Reader Person Dog
getDogRM'' = Reader getDogRM

x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 ::
  Integer ->
  (Maybe Integer, Maybe Integer)
x3 = do
  m <- z'
  return (m, m)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (> 3) (< 8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(> 3), (< 8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ foldr (&&) True (sequA 6)
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(> 3), (< 8), even] 7

-----------------------------------------------------------------
-- Revision
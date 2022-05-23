{-# LANGUAGE InstanceSigs #-}

module Ch23 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import qualified Data.DList as DL
import System.Random

-- without state
data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x ->
      error $
        "intToDie got non 1-6 integer: "
          ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

-- with state
rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' ::
  State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
         in go
              (sum + die)
              (count + 1)
              nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
         in go
              (sum + die)
              (count + 1)
              nextGen

rollsCountLogged ::
  Int ->
  StdGen ->
  (Int, [Die])
rollsCountLogged n g = go 0 0 g []
  where
    go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
    go sum count gen accum
      | sum >= n = (count, accum)
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
         in go
              (sum + die)
              (count + 1)
              nextGen
              (intToDie die : accum)

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let (n, s) = g s
     in (f n, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) ::
    Moi s (a -> b) ->
    Moi s a ->
    Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
      let (a, s1) = g s
          (fs, s2) = f s1
       in -- (b, s2) = f (s) a
          (fs a, s2)

instance Monad (Moi s) where
  return = pure
  (>>=) ::
    Moi s a ->
    (a -> Moi s b) ->
    Moi s b
  (Moi f) >>= g =
    Moi $ \s ->
      let (n, s) = f s
       in runMoi (g n) s

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList xs = execState (mapM_ addResult xs) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzList' :: [Integer] -> DL.DList String
fizzbuzzList' xs =
  execState (mapM_ addResult' xs) DL.empty

addResult' ::
  Integer ->
  State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

fizzbuzzFromTo ::
  Integer ->
  Integer ->
  [String]
fizzbuzzFromTo from to
  | from < to = fizzBuzz to : fizzbuzzFromTo from (to - 1)
  | from == to = [fizzBuzz from]
  | otherwise = error "error"

main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzList' [1 .. 100]

get' :: Moi s s
get' = Moi $ \s -> (s, s)

put' :: s -> Moi s ()
put' s = Moi $ const ((), s)

exec' :: Moi s a -> s -> s
exec' (Moi sa) s =
  snd $ sa s

eval' :: Moi s a -> s -> a
eval' (Moi sa) = fst . sa

modify'' :: (s -> s) -> Moi s ()
modify'' = \f -> Moi $ \x -> ((), f x)

-----------------------------------------------------------------
-- Revision
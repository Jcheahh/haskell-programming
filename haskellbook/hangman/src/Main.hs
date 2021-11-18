module Main where

import Control.Monad
import Data.Char (isAlpha, toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
  )
import System.Random (randomRIO)

main :: IO ()
main = do
  putStrLn "hello world"

maybeAll ::
  (a -> Bool) ->
  Maybe a ->
  Bool
maybeAll = all

eitherAll ::
  (a -> Bool) ->
  Either b a ->
  Bool
eitherAll = all

newtype WordList
  = WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength
            && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle
  = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    ( intersperse ' ' $
        fmap renderPuzzleChar discovered
    )
      ++ " Guessed so far: "
      ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (map (const Nothing) xs) []

-- J a c k
-- _ _ _ _

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle xs bs ds) b = case xs of
  [] -> False
  c : cs -> if c == b then True else charInWord (Puzzle cs bs ds) b

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle xs bs ds) b = case ds of
  [] -> False
  c : cs -> if c == b then True else alreadyGuessed (Puzzle xs bs cs) b

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just x) = x
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter
  ( Puzzle
      word
      filledInSoFar
      s
    )
  c =
    Puzzle word newFilledInSoFar (c : s)
    where
      zipper guessed wordChar guessChar =
        if wordChar == guessed
          then Just wordChar
          else guessChar
      newFilledInSoFar =
        zipWith
          (zipper c)
          word
          filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case ( charInWord puzzle guess,
         alreadyGuessed puzzle guess
       ) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
        \ character, pick \
        \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the\
        \ word, filling in the word\
        \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in\
        \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 20
    then do
      putStrLn "You lose!"
      putStrLn $
        "The word was: "
          ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ ->
      putStrLn
        "Your guess must\
        \ be a single character"

main1 :: IO ()
main1 = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1 == reverse (isPalindrome line1)) of
    True -> putStrLn "It's a palindrome!"
    False ->
      do
        putStrLn
          "Nope!"
        exitSuccess
  where
    isPalindrome xs = filter isAlpha (map toLower xs)

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson ::
  Name ->
  Age ->
  Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $
      PersonInvalidUnknown $
        "Name was: " ++ show name
          ++ " Age was: "
          ++ show age

gimmePerson :: IO ()
gimmePerson = forever $ do
  putStrLn
    "Insert your name: "
  name <- getLine
  putStrLn "Insert your age: "
  age <- getLine
  case (mkPerson name (read age)) of
    Left pi -> print pi
    Right per -> do
      putStr "Yay! Successfully got a person: "
      print per

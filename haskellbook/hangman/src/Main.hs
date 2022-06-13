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
import Test.Hspec

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
  dict <- readFile "haskellbook/hangman/data/dict.txt"
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

------------------------------------------------------------------
-- Revision

data PuzzleR
  = PuzzleR String [Maybe Char] [Char]
  deriving (Eq)

instance Show PuzzleR where
  show (PuzzleR _ discovered guessed) =
    intersperse
      ' '
      ( fmap renderPuzzleCharR discovered
      )
      ++ " Guessed so far: "
      ++ guessed

freshPuzzleR :: String -> PuzzleR
freshPuzzleR xs = PuzzleR xs (fmap (const Nothing) xs) []

charInWordR :: PuzzleR -> Char -> Bool
charInWordR (PuzzleR xs _ _) x = x `elem` xs

alreadyGuessedR :: PuzzleR -> Char -> Bool
alreadyGuessedR (PuzzleR _ _ zs) x = x `elem` zs

renderPuzzleCharR :: Maybe Char -> Char
renderPuzzleCharR Nothing = '_'
renderPuzzleCharR (Just x) = x

fillInCharacterR :: PuzzleR -> Char -> PuzzleR
fillInCharacterR
  ( PuzzleR
      word
      filledInSoFar
      s
    )
  c =
    PuzzleR word newFilledInSoFar (c : s)
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

handleGuessR :: PuzzleR -> Char -> IO PuzzleR
handleGuessR puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWordR puzzle guess, alreadyGuessedR puzzle guess) of
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
      return (fillInCharacterR puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in\
        \ the word, try again."
      return (fillInCharacterR puzzle guess)

gameOverR :: PuzzleR -> IO ()
gameOverR (PuzzleR wordToGuess _ guessed) =
  if (length guessed) > 20
    then do
      putStrLn "You lose!"
      putStrLn $
        "The word was: "
          ++ wordToGuess
      exitSuccess
    else return ()

gameWinR :: PuzzleR -> IO ()
gameWinR (PuzzleR wordToGuess fillInSoFar _) =
  if all isJust fillInSoFar
    then do
      putStrLn "You Win!"
      putStrLn $
        "The word is: "
          ++ wordToGuess
      exitSuccess
    else return ()

runGameR :: PuzzleR -> IO ()
runGameR puzzle = forever $ do
  gameOverR puzzle
  gameWinR puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuessR puzzle c >>= runGameR
    _ ->
      putStrLn
        "Your guess must\
        \ be a single character"

mainR :: IO ()
mainR = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzleR (fmap toLower word)
  runGameR puzzle

isChar :: Char -> Bool
isChar x = x `elem` ['a' .. 'z']

palindromeR :: IO ()
palindromeR = forever $ do
  line1 <- getLine
  let filterL = filter isChar $ fmap toLower line1
  case (filterL == reverse filterL) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

puzzle1 :: PuzzleR
puzzle1 = PuzzleR "hello" [Nothing, Just 'e', Nothing, Nothing, Nothing] ['a', 'e', 's']

puzzle2 :: PuzzleR
puzzle2 = PuzzleR "hello" [Just 'h', Just 'e', Nothing, Nothing, Nothing] ['h', 'a', 'e', 's']

runHangman :: IO ()
runHangman = hspec $ do
  describe "fillInCharacter" $ do
    it "fillInCharacterR puzzle1 'h' `shouldBe` puzzle2" $ do
      fillInCharacterR puzzle1 'h' `shouldBe` puzzle2
  describe "handleGuess" $ do
    it "handleGuessR puzzle1 'e' shouldBe` puzzle1" $ do
      pz <- handleGuessR puzzle1 'e'
      pz `shouldBe` puzzle1
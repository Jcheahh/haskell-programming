module Ch13 where

import Control.Monad

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

-----------------------------------------------------------------
-- Revision
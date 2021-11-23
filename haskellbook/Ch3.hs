module Ch3 where

main :: IO ()
main = putStrLn "hello world!"

main1 :: IO ()
main1 = do
  putStrLn "Count to four for me:"
  putStr "one, two"
  putStr ", three, and"
  putStrLn " four!"

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main2 :: IO ()
main2 = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where
    secondGreeting =
      concat [hello, " ", world]

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs x = drop 9 x ++ drop 5 (take 9 x) ++ take 5 x

main3 :: IO ()
main3 = print $ rvrs "Curry is awesome"
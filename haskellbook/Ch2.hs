module Ch2 where

triple :: Integer -> Integer
triple x = x * 3

times3pointonefour :: Float -> Float
times3pointonefour x = x * pi

thirdLetter :: String -> Char
thirdLetter x = x !! 2

z = 7

y = z + 8

x = y ^ 2

waxOn = x1 * 5
  where
    x1 = x

waxOff = triple
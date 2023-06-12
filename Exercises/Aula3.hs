module Aula3 where

-- Ex 22)
-- sum [x*x | x <- take 100 [1..]]


-- Ex 23)
--a)
aprox :: Int -> Double
aprox n = 4*(sum [ f x | x <- [0..n] ])
          where f x = ((-1)^x) / fromIntegral (2*x+1)

--b)
aprox2 :: Int -> Double
aprox2 n = sqrt (12*sum ([f x | x <- take n [0..]]))
           where f y = ((-1)^y) / fromIntegral ((y+1)^2)

-- Coisa extra aula (filtrar numeros maiores que um valor dado)

filtra :: Int -> [Int] -> [Int]
filtra h hs = [ x | x <- hs, x > h]


-- Ex 24)
divprop :: Int -> [Int]
divprop x = [k | k <- take (x-1) [1..], x `mod` k == 0]


-- Ex 25)
perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..n], x == sum (divprop x)]


-- Ex 26)
primo :: Int -> Bool
primo n = divprop n == [1]


-- Ex 27)
binom n k = a `div` b
  where a = product [k+1..n]
        b = product [1..n-k]

pascal :: Int -> [[Int]]
pascal n = [[binom x k | k <- [0..x]] | x <- [0..n]]


-- Ex 28)
dotprod:: [Float] -> [Float] -> Float
dotprod xs ys = sum [a*b | (a,b) <- zip xs ys]


-- Ex 29)
pitagoricos :: Int -> [(Int,Int,Int)]
pitagoricos n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]


-- Ex 30)
forte :: String -> Bool
forte s = length s >= 8 && and [or [x k | k <- s] | x <- [isLower, isUpper, isNumber]]
          where isLower z = z >= 'a' && z <= 'z'
                isUpper z = z >= 'A' && z <= 'Z'
                isNumber z = z >= '0' && z <= '9'
















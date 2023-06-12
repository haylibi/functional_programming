module Suplementares where
import Data.List

-- Ex 104
-- a)
mindiv :: Int -> Int
mindiv n = mindivAux n 2

mindivAux n k | n<2 = n 
              | k*k > n = n
              | n `mod` k == 0 = k
			  | otherwise = mindivAux n (k+1)

-- b)
prime n = n > 1 && mindiv n == n

-- Ex 105
cifrar :: Int -> String -> String
cifrar n [] = []
cifrar n (x:xs) = (f nn ([x..'Z'] ++ ['A'..x])):(cifrar nn xs)
                       where nn = n `mod` 26
                             f a (y:ys) | a == 0 = y 
                                        | y > 'Z' || y < 'A' = y
                                        | otherwise = f (a-1) ys


-- Ex 106
magico :: [[Int]] -> Bool
magico x = magicoAux x && magicoAux (transpose x) && diagMag x && diagMag (transpose x)

diagMag (x:xs) = (sum [k !! a | (k,a) <- zip (x:xs) [0..]] == sum x) && 
                 (sum [k !! (tmp - a) | (k,a) <- zip (x:xs) [1..]] == sum x) where tmp = (length x)

magicoAux [] = True
magicoAux (x:xs) = and [sum k == sum x | k <- xs]


-- Ex 107
--oitoRainhas :: [[Int]]
oitoRainhas = filter pass (permutations [0..7])

pass [] = True
pass (x:xs) = (and [k /= x + b && k /= x - b | (k,b) <- zip xs [1..]]) && (pass xs)

permutations [] = [[]]
permutations (x:xs) = [y | k <- permutations xs, y <- interleave xs]
                     where interleave [] = [[x]]
                           interleave (y:ys) = (x:y:ys):map (y:) (interleave ys)

-- Ex 108

decompor n [] | n == 0 = [[]]
              | otherwise = []
decompor n (x:xs) = [take k (cycle [x]) ++ l | k <- [0.. n `div` x], l <- decompor (n-x*k) xs]


-- Ex 109
type Grid = [[Int]]

-- a)
check :: Grid -> Bool
check xs = checkLines xs && checkSquares xs && checkCols xs
checkLines xs = and [k1 /= k2 | x <- xs, (k1,a) <- zip x [1..], (k2,b) <- zip x [1..], a /= b]
checkCols xs = checkLines (transpose xs)
checkSquares xs = and [checkSq a b xs | a <- [0..2], b <- [0..2]]
checkSq :: Int -> Int -> Grid -> Bool
checkSq a b xs = and [((xs!!(3*a+k1))!!(3*b+k2)) /=  ((xs!!(3*a+k3))!!(3*b+k4)) | k1 <- [0..2], k2 <- [0..2], k3 <- [0..2], k4 <- [0..2], (k1,k2) /= (k3,k4)]

sudok :: [[Int]]
sudok = [[1,2,3,4,5,6,7,8,9],
         [4,5,6,7,8,9,1,2,3],
         [7,8,9,1,2,3,4,5,6],
         [2,3,4,5,6,7,8,9,1],
         [5,6,7,8,9,1,2,3,4],
         [8,9,1,2,3,4,5,6,7],
         [3,4,5,6,7,8,9,1,2],
         [6,7,8,9,1,2,3,4,5],
         [9,1,2,3,4,5,6,7,8]]



-- Ex 110
palavras :: String -> [String]
palavras [] = []
palavras xs = palavrasAux [] "" xs
palavrasAux l s [] = [l++s]
palavrasAux l s (x:xs) | x == ' ' = if s /= "" then ([l++s] ++ palavrasAux l "" xs) else palavrasAux l "" xs
                       | otherwise = palavrasAux l (s++[x]) xs

despalavras :: [String] -> String
despalavras [x] = x
despalavras (x:xs) = x ++ " " ++ despalavras xs

-- Ex 111
calcPi1 :: Int -> Double
calcPi1 n = sum (take n [(fromInteger a) / (fromInteger b) | (a,b) <- zip (cycle [4,-4]) (filter odd [1..])])

calcPi2 :: Int -> Double
calcPi2 n = 3 + sum (take n [(fromInteger a) / (fromInteger (b*(b+1)*(b+2))) | (a,b) <- zip (cycle [4,-4]) [2,4..]])

-- Ex 112

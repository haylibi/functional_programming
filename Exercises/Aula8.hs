module Aula8 where

-- Ex 57)
factorial = [product (take k [1..]) | k<-[0..]]

-- Com zip
--faczip = (1:[x*y | (x,y) <- zip faczip [1..]])

-- agora com map :: (a -> b) -> [a] -> [b]
--fac' = 1 : map (\x->product [1..x]) [0..]

-- agora com zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--fac'' = 1 : zipWith ???? ???? ????

-- agora com foldr / listas compreensão -- 'ineficiente'

--fac''' = 1 : [ foldr ???? ???? [1..n] | n <- [1..]]


fibonacci = [1,1] ++ [x+y | (x,y) <- zip fibonacci (drop 1 fibonacci)]

-- Ex 58)
--potencias 2
merge (x:xs) (y:ys) |x>y = y:(merge (x:xs) ys)
                    |x==y = x:(merge xs ys)
                    |otherwise = x:(merge xs (y:ys))

hamming = 1: merge p2 (merge p3 p5)
           where p2 = map (2*) hamming
                 p3 = map (3*) hamming 
                 p5 = map (5*) hamming

-- Ex 59)
somas xs = [sum (take x xs) | x <- [0..]]

-- Ex 60) a)
pascal = [[binom n k |k <- [0..n]] | n <- [0..]]
         where binom n k = (product [(k+1)..n]) `div` (product [1..(n-k)])

-- b)
binom2 _ 0 = 1
binom2 n k = if n == k then 1 else binom2 (n-1) (k-1) + binom2 n (k+1)

pascal2 = [[binom2 n k |k <- [0..n]] | n <- [0..]]
         where binom2 n k = (product [(k+1)..n]) `div` (product [1..(n-k)])

-- Ex 61)   
shift (x:xs) = xs++[x]
rotate xs = take (length xs) (iterate shift xs)

-- Ex 62)
-- a)
strings = [k:[] | k<-['a'..'z']] ++ [a:b | b<-strings, a <- ['a'..'z']]
-- b)
stringsN n = [a | a<-takeWhile f strings, length a == n]
              where f a = (length a) <= n


-- Ex 63)
myscanl f i l = i:[f x y | (x,y) <- zip (myscanl f i l) l]






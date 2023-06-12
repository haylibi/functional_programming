module Aula2 where

-- Ex14
--a) [Char]
--b) (Char,Char,Char)
--c) [(Bool,Char)]
--d) ([Bool],[Char])
--e) [[a]->[a]]
--f) [Bool -> Bool]


-- Ex15	
--1) f:: Num a => (a, a) -> Int
--2) g :: Num a => a -> b;  f :: b -> Int
--3) g :: a; f :: Num a => g -> (a -> Int) ----> os parenteses sao irrelevantes
--4) g :: a; f :: Num a => g -> [a] -> Int
--5) f :: Num a => (a,a) -> [Int -> Int]

lista_funcs :: (Int, Int) -> [Int -> Int]
lista_funcs (x,y)
	|x <= y = ((+) x): lista_funcs(x+1, y)
	|otherwise = []


-- Ex 16
-- g :: t
-- f :: Num a => t -> [a -> [Int]]
-- (f g) :: Num a => [a -> [Int]]
-- head (f g) :: Num a => a -> [Int]


-- Ex 17
segundo :: [a] -> a
segundo xs = head (tail xs)

trocar :: (a,b) -> (b,a)
trocar (x,y) = (y,x)

par :: a -> b -> (a,b)
par x y = (x,y)

dobro :: Num a => a -> a
dobro x = 2*x

metade :: Fractional a => a -> a
metade x = x/2

minuscula :: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'

intervalo :: Ord a => a -> a -> a -> Bool
intervalo x a b = x <= a && x <= b

palindromo :: Eq a => [a] -> Bool
palindromo xs = reverse xs == xs

twice :: (a -> a) -> a -> a 
twice f x = f (f x)


-- Ex 18
--inc :: Int -> Int
--inc :: Num a => a -> a
--inc x = x+1

--dobro :: Int -> Int
--dobro :: Num a => a -> a
--dobro x = x + x

--quadrado :: Int -> Int
--quadrado :: Num a => a -> a
--quadrado x = x*x


--media :: Float -> Float -> Float
--media :: Fractional a => a -> a -> Float
--media x y = (x+y)/2

--triangulo :: Int -> Int -> Int -> Bool
--triangulo :: (Ord a, Num a) => a -> a -> a -> Bool
--triangulo a b c = a<b+c && b<a+c && c<a+b


--Ex 19
--f1 :: Int -> (Int -> Int) -> Int
f1 a c = a + c (a)

--f2 :: Char -> Bool -> Bool
f2 a b = b && a <= 'z' && a >= 'a'

--f3 :: (Char -> Char -> Int) -> Char -> Int
f3 x w = x 'a' w + x w 'b' + 1

--f4 :: Eq a => a -> [a] -> Bool
f4 x y = head y == x && True

--f5 :: Eq a => a -> [a] -> [a]
f5 x y |x == head y =  y
       |otherwise = tail y

--f6 Ord a => a -> a -> a
f6 x y |x<y = y
       |x>=y = x

--Ex 20
-- So no caso (2,[3]) e (2,[]), o tipo de resultado seria true ou false

--Ex 21
-- So no caso (2,[3]), pois [] nao pode ter nenhum elemento, logo nao podemos levar [a] -> a da lista vazia, resultados seriam do tipo a








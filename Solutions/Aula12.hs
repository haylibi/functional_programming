module Aula12 where

-- Ex 89)
data Nat = Zero | Succ Nat

(+.) :: Nat -> Nat -> Nat
(+.) Zero y = y
(+.) (Succ x) y = Succ ((+.) x y)


-- Ex 101)

data Arv a = Vazia | No a (Arv a) (Arv a)
             deriving Show
-- a)
soma :: Num a => Arv a -> a
soma Vazia = 0
soma (No a esq dir) = a + (soma esq) + (soma dir)
-- b)
valorArv :: Num a => Arv a -> a
valorArv Vazia = 0
valorArv (No a _ _) = a
-- c)
somasTree :: Num a => Arv a -> Arv a
somasTree Vazia = Vazia
somasTree (No a esq dir) = No (a + (valorArv a1) + (valorArv a2)) (a1) (a2)
                               where a1 = somasTree esq
                                     a2 = somasTree dir
									 


-- Ex 102)
simetria Vazia = Vazia
simetria (No a esq dir) = No a (simetria dir) (simetria esq)


-- Ex 103)
foldtree :: (a -> b -> b -> b) -> b -> Arv a -> b
foldtree f k Vazia = k
foldtree f k (No a esq dir) = f a (foldtree f k esq) (foldtree f k dir)


t :: Arv Int
t = No 5 (No 6 Vazia (No 12 Vazia Vazia)) (No 1 Vazia Vazia)
soma3 x y z = x+y+z
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f x [] = x
-- foldr f k x:xs = f x (foldr f k xs)


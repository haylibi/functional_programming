module Aula4 where

-- Ex 31)

powerset :: Num a => a -> a
powerset 0 = 1
powerset n = 2 * powerset (n-1) 


-- Ex 32)
--a)
and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) = x && (and2 xs)

--b)
or2 :: [Bool] -> Bool
or2 [] = False
or2 (x:xs) |x = True
           |otherwise = or2 xs

--c)
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ (concat2 xs)

--d)
replicate2 :: Int -> a -> [a]
replicate2 1 x = [x]
replicate2 n x = (x: replicate2 (n-1) x)

--e)
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

--f)
elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 a (x:xs) |a == x = True
               |otherwise = elem2 a xs


-- Ex 33)
concat3 :: [[a]] -> [a]
concat3 x = [a | i <- take (length x) [0..], a <- x !! i]

replicate3 :: Int -> a -> [a]
replicate3 n x = [x | i <- [1..n]]

(!!!!) :: [a] -> Int -> a
(!!!!) x n = head [k | (k,a) <- zip x [0..], a == n]

-- Ex 34)
-- a)
--leastSquare n = (head [x | x <- [1..n], x*x >= n]) 
leastSquare 1 = 1
leastSquare n = if w*w == (n-1) then (w+1) else w
                where w = leastSquare (n-1)

-- Do prof
leastSquarenAux :: Int -> Int -> Int
leastSquarenAux n k | k*k >= n = k
                   | otherwise = leastSquarenAux n (k+1)

leastSquaren :: Int -> Int
leastSquaren n = leastSquarenAux n 0

-- b)
isqrtn n = head [x-1 | x <- [1..], x*x>n]


-- Ex 35)
--a)
factorial 0 = 1
factorial n = n * (factorial (n-1))

--b)
rangeProduct a b |a==b = b
                 |otherwise = a*(rangeProduct (a+1) b)

--c)
factorial2 n = rangeProduct 1 n


-- Ex 36)
--mdc :: Integral a => a -> a -> a
mdc a 0 = a
mdc a b = mdc b (a `mod` b)


-- Ex 37)
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) |[k | k <- xs, x == k] == [] = [x] ++ nub(xs)
           |otherwise = nub(xs)

-- Ex 38)
--intersperse :: a -> [a] -> [a]
interperse _ [] = []
interperse _ (a:[]) = [a]
interperse ch (x:xs) = (x:ch:(interperse ch xs))


-- Ex 39)
maxFun :: (Int -> Int) -> Int -> Int
maxFun f 1 = max (f 1) (f 0)
maxFun f n = max (f n) (maxFun f (n-1))
 

-- Ex 40)
--anyZero :: (Int -> Int) -> Int -> Bool
anyZero f 0 = (f 0) == 0
anyZero f n = (f n) == 0 || anyZero f (n-1)


-- Ex 41)
--sumFun :: (Int -> Int) -> Int -> Int
sumFun f 0 = f 0
sumFun f n = (f n) + (sumFun f (n-1))


-- Ex 42)
--a)
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) |a < x = (a:x:xs) 
                |otherwise = (x: (insert a xs))

--b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)


-- Ex 43)
--a)
minim :: Ord a => [a] -> a
minim [x] = x
minim (x:xs) = min x (minim xs)
                 where min a b = if a<b then a else b

minimu :: Ord a => [a] -> a
-- completar a definição
minimu (x:[]) = x
minimu (x:xs) |x<=w = x
               |otherwise = w
            where w = minimu xs

--b)
del :: Eq a => a -> [a] -> [a] 
del _ [] = []
del a (x:xs) |a == x = xs
             |otherwise = x: (del a xs)

--c)
ssort :: (Eq a, Ord a) => [a] -> [a]
ssort [] = []
ssort l = [x] ++ ssort (del x l)
          where x = minim l


-- Ex 44)
--a)
merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) = (a: merge l1 l2)
                     where a = if x<y then x else y
                           l1 = if a==x then xs else x:xs
                           l2 = if l1 == (x:xs) then ys else y:ys
--b)
metades :: [a] -> ([a],[a])
metades xs = (l1, l2) 
              where k = length xs
                    l1 = [x | (x,i) <- zip xs [1..k],  i <= k-i]
                    l2 = [x | (x,i) <- zip xs [1..k],  i> k-i]
msort [] = []
msort (x:[]) = (x:[])
msort xs = merge (msort (fst l)) (msort (snd l))
           where l = metades xs


-- Ex 45)
bits 1 = [[True], [False]]
bits n = [x++y | x <- bits (n-1), y <- bits 1]



-- Ex 46)
--permutations (x:[]) = [(x:[])]
--permutations xs = [[y] ++ xxs | y <- xs, xxs <- permutations (del y xs)] 

permutations []     = [[]]
permutations (x:xs) = [y | p <- permutations xs, y <- interleave p]
  where
    interleave []     = [[x]]
    interleave (y:ys) = (x:y:ys) : map (y:) (interleave ys)





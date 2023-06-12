module Aula6e7 where

-- Ex 47)
--[f x | x <- xs, p x]

--ex47 :: (a -> b) -> (a ->  Bool) -> [a] -> [b]
ex47 f p xs = map f (filter p xs)

-- Ex 48)
-- a)
(+++) :: [a] -> [a] -> [a]
(+++) a b = foldr f b a
            where f x y = (x:y)
			

-- b)
concat1 :: [[a]] -> [a]
concat1 xs = foldr (+++) [] xs

-- c)
reverse xs = foldr f [] xs
             where f a b = b ++ [a]
				   
--reverse xs = foldr (\x a -> a++x) [] [[k] | k <- xs]

reverse2 xs = foldl f [] [[k] | k <- xs]
             where f a b = b ++ a

-- d)
elem x xs = any ((==) x) xs



-- Ex 49)
dec2int xs = foldl f 0 xs
           where f a b = 10*a + b



-- Ex 50)
zipWith1 _ [] _ = []
zipWith1 _ _ [] = []
zipWith1 f (x:xs) (y:ys) = [f x y] ++ zipWith1 f xs ys



-- Ex 51)
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) |a < x = (a:x:xs) 
                |otherwise = (x: (insert a xs))

--isort :: Ord a => [a] -> [a]
--insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) |a < x = (a:x:xs) 
                |otherwise = (x: (insert a xs))

isort xs = foldr (insert) [] xs

-- Ex 52)
-- a)
--shift _ [] = []
--shift _ [] = []
--shift 0 xs = xs 
--shift n (x:xs) = shift (n-1) (xs ++ [x])

shift [] = []
shift (x:xs) = (xs ++ [x])
--b)
rotate xs = foldr f [xs] [1..((length xs) - 1)]
            where f _ b = b ++ [shift (b !! ((length b)-1))]

--rotate xs = [shift n xs | n <- [0..((length xs)-1)]]


-- Ex 53)
--a)
maximum1 xs = foldl1 f xs		--funciona com foldr1 e foldl1
              where f a b = if a>b then a else b
minimum1 xs = foldl1 f xs		--funciona com foldr1 e foldl1
              where f a b = if a<b then a else b	

--b)
foldl2 f xs = foldl f (head xs) (tail xs)
foldr2 f xs = foldr f (last xs) (init xs)

-- Ex 54)
--a)
--succ i = i+1
--pred i = i-1

add i 0 = i
add i j = succ (add i (pred j))

mult i 1 = i
mult i j = add i (mult i (pred j))

exp1 i 0 = 1
exp1 i j = mult i (exp1 i (pred j))

--b)
foldi :: (a -> a) -> a -> Integer -> a

foldi f q 0 = q
foldi f q i = f (foldi f q (pred i))

add1 i j = foldi succ i j

mult1 i j = foldi (add i) i (pred j)

exp i j = foldi (mult i) i (pred j)

--c)
fact i = snd (foldi (\(a,b) -> ((succ a),a*b)) (1,1) i)
fib x = fst (foldi (\(a,b) -> (b,a+b)) (0,1) x)


-- Ex 55)
mdc a b = fst (until p (\(a,b) -> (b, a `mod` b)) (a,b))
          where p (a,b) = b==0
		  
		  
-- Ex 56)
--scan _ _ [] = []
--scan f z (x:xs) = (f z x: scan f (f z x) xs)
scan f z xs = scan2 f z xs 0
scan2 f z xs (n) | n == length xs = [foldr f z xs]
				 | otherwise = (foldr f z (take n xs):scan2 f z xs (n+1))
				 
-- Listas em compreensao
--scan f z xs = [foldr f z k | k<-x]
--			  where x = [take z xs | z<-[0..(length xs)]]


module Aula11 where
import Stack

-- Ex 78
-- a)
data Shape = Circle Float | Rectangle Float Float 
             deriving Show
			 
-- b)

perimetro :: Shape -> Float
perimetro (Circle k) = 3.141592654*2*k
perimetro (Rectangle a b) = 2*a + 2*b 

-- Ex 79

-- 1)
data Point a = P a a 
             deriving (Eq, Show)

data Rect a = RP (Point a) (Point a)
           deriving (Eq, Show)


areaRect :: Num a => Rect a -> a
areaRect (RP (P a b) (P c d)) = (d-b)*(c-a)

intersect :: (Eq a) => Rect a -> Rect a -> Bool
intersect (RP p1 p2) (RP p3 p4) = p1 == p3 || p1 == p4 || p2 == p3 || p2 == p4

-- Ex 80
parent p = parentAux p (empty)

parentAux :: String -> Stack Char -> Bool
parentAux "" x = isEmpty(x)
parentAux (x:xs) auxStack | x == '(' = parentAux xs (push ')' auxStack)
                          | x == '{' = parentAux xs (push '}' auxStack) 
                          | x == '[' = parentAux xs (push ']' auxStack) 
						  | isEmpty(auxStack) = False
                          | otherwise = if x == (top auxStack) then (parentAux xs (pop auxStack)) else False

-- Ex 81

-- a)
calc :: Stack Float -> String -> Stack Float
calc stack s | s == "+" = push (b + a) nstk
             | s == "-" = push (b - a) nstk
             | s == "*" = push (b * a) nstk
             | s == "/" = push (b / a) nstk
             | otherwise = push (read s) stack
     where a = (top stack)
           b = (top (pop stack))
           nstk = pop (pop stack)

-- b)
calcular :: String -> Float
calcular s = calcularAux (words s) (empty)
calcularAux [] stack = top stack
calcularAux (s:ss) stack =  calcularAux ss (calc stack s)

calcular2 :: String -> Float
calcular2 s = top $ foldl calc empty (words s)


-- Ex 82

-- 1)
type G = ([V],E)
type V = Int
type E = [(V,V)]

-- 2)

-- Funcoes auxiliares
vertices :: G -> [V]
vertices = fst

arcos :: G -> [(V,V)]
arcos = snd

adjacentes :: G -> V -> [V]
adjacentes (ve, ar) v = [snd k | k <- ar, fst k == v]

caminhos :: G -> V -> V -> Int
caminhos g o d | o == d = 1
               | saidas == [] = 0
               | otherwise = sum [caminhos g o' d | o' <- saidas]
                   where saidas = adjacentes g o

grafo82 :: G
grafo82 = ([1,2,3,4,5,6],[(1,2),(1,4),(2,3),(2,5),(2,6),(3,6),(4,5),(6,5)])

grafo1 :: G
grafo1 = ([1,2,3],[(1,2),(2,3),(3,1)])


-- Ex 83
{-
para s = []
    top (push x []) = top (x:[]) = x
supondo que e verdade para s, seja xs algum valor
    top (push x xs:s) = top (x:(xs:s)) = x

isEmpty empty = isEmpty [] = True

isEmpty (push x s) = isEmpty (x:s) = False (x is always some value)
-}

-- Ex 84
-- Data.Map
{- ocorrences :: String -> IO()
ocorrences text = ocorrencesAux text (Data.Map.fromList [])

ocorrencesAux :: String -> Map Char Int -> IO()
ocorrencesAux [] p = [putStrLn((fst x):(show (snd x)):[]) | x <- Data.Map.toList p]
ocorrencesAux (x:xs) p | Data.Map.lookup x p == Nothing = ocorrencesAux xs (Data.Map.insert x 1 p)
                       | otherwise = let Just a = Data.Map.lookup x p in ocorrencesAux xs (Data.Map.insert x a) -}


-- Ex 84 -> num ficheiro a parte

-- Ex 85 (nao percebi 1 crl)


-- Ex 86

data Set a = Empty | No (Set a) a (Set a)
               deriving (Show, Eq)

vazio = Empty

inserir :: Ord a => a -> Set a -> Set a
inserir a Empty = No Empty a Empty
inserir a (No left x right) | a == x = No left x right
                          | a < x = No (inserir a left) x right
                          | otherwise = No left x $inserir a right

member :: Ord a => a -> Set a -> Bool
member a Empty = False
member a (No left x right) | a == x = True
                           | otherwise = (member a left) || (member a right)

-- Ex 87

{- union :: Ord a => Set a -> Set a -> Set a
union Empty k = k
union (No left val right) k = union left $union right (inserir val k)

intersect :: Ord a => Set a -> Set a -> Set a
intersect Empty _ = Empty
intersect (No l1 v1 r1) (No l2 v2 r2) | v1 == v2 = No (intersect l1 l2) v1 (intersect r1 r2)
                                      | v1 < v2 =  -}



-- Ex 88
data Map k a = No k a (Map k a) (Map k a) | Vazia 
     deriving (Eq, Show)

--
-- completar as duas definições seguintes

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a Vazia = No k a Vazia Vazia
insert k a (No f s left right) | k == f = No k a left right
                               | k < f = No f s (insert k a left) right
                               | otherwise = No f s left (insert k a right)

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k Vazia = Nothing
lookup k (No f s left right) | f == k = Just s
                             | k < f = lookup k left
                             | otherwise = lookup k right

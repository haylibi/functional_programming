module Aula10 where
{-
-- type
type Ponto = (Int,Int)
somarPontos :: Ponto -> Ponto -> Ponto
somarPontos (x1,y1) (x2,y2) = (x1+x2, y1+y2)
-- data
data Arv a = Vazia
           | No a (Arv a) (Arv a) deriving (Show, Eq)
arv1 :: Arv Int
arv1 = Vazia
arv2 :: Arv Int
arv2 = No 1 Vazia Vazia
-- > arv1 == arv2
-- False
-- > arv1 == Vazia
-- True
-}
-- aula teórica 10

import Data.List
---------------------------------
pad n s = (take (n - length s) $ repeat ' ') ++ s
nivelS :: (Show a) => Int -> Arv a -> [String]
nivelS _ Vazia = [""]
nivelS 0 (No x _ _) = [show x]
nivelS n (No _ l r) = (nivelS (n-1) l) ++ (nivelS (n-1) r)
arv2lists :: Arv Int -> IO ()
arv2lists a = sequence_ [putStrLn x | x <- xs]
  where l   = length $ show $ mais_dir a
        str = map (\x -> map (pad l) (nivelS x a)) [0..(altura a)-1]
        f   = ((+l).(*2))
        spc = reverse $ take (altura a) $ zip (iterate f 0) (iterate f l)
        sps = zip str spc
        xs  = map (\(l,(f,s)) -> 
                    (replicate f ' ') ++ 
                    concat (intersperse (replicate s ' ') l)) sps
----------------------------------

data Arv a = Vazia
           | No a (Arv a) (Arv a)
           deriving (Show,Eq)
listar :: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir

ordenada :: Ord a => Arv a -> Bool
ordenada arv = crescente (listar arv)
  where -- verificar se uma lista  ́e crescente
    crescente xs = and (zipWith (<=) xs (tail xs))
	
pertence :: Ord a => a -> Arv a -> Bool
pertence x Vazia = False
pertence x (No y esq dir)| x==y  = True           -- encontrou
                         | x<y   = pertence x esq -- procura à esquerda
                         | x>y   = pertence x dir -- procura à direita
						 
inserir :: Ord a => a -> Arv a -> Arv a
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir) | x==y = No y esq dir -- já ocorre
                         | x<y  = No y (inserir x esq) dir -- insere à esquerda
                         | x>y  = No y esq (inserir x dir )-- insere à direita
						 
-- inserir multiplos valores
-- foldr inserir Vazia [3,1,2]
-- No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia))
inserirLista :: Ord a => [a] -> Arv a -> Arv a
inserirLista xs a = foldr inserir a xs

-- de notar que pode construir árvores desequilibradas
-- pre-condição: a lista deve estar por ordem crescente
construir :: [a] -> Arv a
construir [] = Vazia
construir xs = No x (construir xs') (construir xs'')
  where n = (length xs) `div` 2  -- ponto médio
        xs' = take n xs -- valores à esquerda
        x:xs'' = drop n xs -- valores central e à direita
		
mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq
--intuição: mais_esq do lado direito: é o menor valor que é maior que x

remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia
remover x (No y Vazia dir) | x==y = dir
remover x (No y esq Vazia) | x==y = esq
remover x (No y esq dir)
        | x<y = No y (remover x esq) dir
        | x>y = No y esq (remover x dir)
        | x==y = let z = mais_esq dir
                 in No z esq (remover z dir)
				 
-- árvores equilibradas -- se em cada nó a altura das sub-árvores
-- difere no máximo em 1
altura :: Arv a -> Int
altura Vazia = 0
altura (No _ l r) = 1 + max (altura l) (altura r)

equilibrada :: Arv a -> Bool
equilibrada Vazia = True
equilibrada (No _ esq dir) = abs (altura esq - altura dir) <= 1 &&
                             equilibrada esq &&
                             equilibrada dir
-- aula teórica 11
desvio :: Arv a -> Int
desvio Vazia = 0
desvio (No _ esq dir) = altura esq - altura dir

pesquisaAVL :: Ord a => a -> Arv a -> Bool
pesquisaAVL x Vazia = False
pesquisaAVL x (No y esq dir)
  | x==y = True
  | x<y  = pesquisaAVL x esq
  | x>y  = pesquisaAVL x dir
  
rodar_dir :: Arv a -> Arv a
rodar_dir (No x (No y t1 t2) t3) = No y t1 (No x t2 t3)
rodar_dir t = t 

rodar_esq :: Arv a -> Arv a
rodar_esq (No x t1 (No y t2 t3)) = No y (No x t1 t2) t3
rodar_esq t = t

--listar t = listar(rodar_dir t)
--listar t = listar(rodar_esq t)
corrige_dir :: Arv a -> Arv a
corrige_dir (No x t1 t2)
  | desvio t1 == -1 = rodar_dir (No x (rodar_esq t1) t2)
  | otherwise = rodar_dir (No x t1 t2)
corrige_dir t = t

corrige_esq :: Arv a -> Arv a
corrige_esq (No x t1 t2)
  | desvio t2 == 1 = rodar_esq (No x t1 (rodar_dir t2))
  | otherwise = rodar_esq (No x t1 t2)
corrige_esq t = t

re_equilibrar :: Arv a -> Arv a
re_equilibrar t | d== 2 = corrige_dir t
                | d== -2 = corrige_esq t
                | otherwise = t
                where d = desvio t
				
inserirAVL :: Ord a => a -> Arv a -> Arv a
inserirAVL x Vazia = No x Vazia Vazia
inserirAVL x (No y esq dir)
         | x==y = No y esq dir
         | x<y  = re_equilibrar (No y (inserirAVL x esq) dir)
         | x>y  = re_equilibrar (No y esq (inserirAVL x dir))
		 
-- aula prática 10
-- Ex 70
sumArv Vazia = 0
sumArv (No x esq dir) = x + (sumArv esq) + (sumArv dir)

-- Ex 71

listarDecr Vazia = []
listarDecr (No a esq dir) = (listarDecr dir) ++ [a] ++ (listarDecr esq)

-- Ex 72
nivel _ Vazia = []
nivel 0 (No x _ _) = [x]
nivel n (No x esq dir) = (nivel (n-1) esq) ++ (nivel (n-1) dir)


-- Ex 73
{-
nivel 3 (construir [1..10])
nivel 3 (construir [1..100])
nivel 3 (construir [1..1000])

nivel 3 (foldr inserir Vazia [1..10])
nivel 3 (foldr inserir Vazia [1..100])
nivel 3 (foldr inserir Vazia [1..1000])

nivel 3 (foldr inserirAVL Vazia [1..10])
nivel 3 (foldr inserirAVL Vazia [1..100])
nivel 3 (foldr inserirAVL Vazia [1..1000])
-}

-- Ex 74
mapArv _ Vazia = Vazia
mapArv f (No x esq dir) = No (f x) (mapArv f esq) (mapArv f dir)

-- Ex 75
foldrArv :: (a -> b -> b) -> b -> Arv a -> b --Nao deveria ser este o tipo, deveria ser (a -> b -> b -> b)
foldrArv f b (No a Vazia Vazia) = f a b
foldrArv f b (No a esq Vazia) = (foldrArv f (f a b) esq)
foldrArv f b (No a Vazia dir) = f a (foldrArv f b dir)
foldrArv f b (No a esq dir) = foldrArv f (foldrArv f b (No a Vazia dir)) esq

-- (do prof)
-- esta um pouco diferente da minha, ate nos tipos muda (nao percebi a pergunta direito)
foldArv :: (a -> b -> b -> b) -> b -> Arv a -> b
foldArv f z Vazia = z
foldArv f z (No x e d) = f x (foldArv f z e) (foldArv f z d)

-- Ex 76
-- a)
mais_dir (No a _ Vazia) = a
mais_dir (No _ _ dir) = mais_dir dir

-- b)
remover2 x Vazia = Vazia
remover2 x (No y Vazia dir) | x==y = dir
remover2 x (No y esq Vazia) | x==y = esq
remover2 x (No y esq dir)
        | x<y = No y (remover2 x esq) dir
        | x>y = No y esq (remover2 x dir)
        | x==y = let z = mais_dir esq
                 in No z (remover2 z esq) dir
				 
-- Ex 77
removerAVL x arv = re_equilibrar (remover2 x arv)
		   


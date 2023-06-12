module Ex88 where

data Map k a = No k a (Map k a) (Map k a) | Vazia 
     deriving (Eq, Show)

--
-- completar as duas definições seguintes

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a Vazia = No k a Vazia Vazia
insert k a (No f s left right) | k == f = No k a left right
                               | k < f = No f s (insert k a left) right
                               | otherwise = No f s left (insert k a right)

lookupp :: Ord k => k -> Map k a -> Maybe a
lookupp k Vazia = Nothing
lookupp k (No f s left right) | f == k = Just s
                              | k < f = lookupp k left
                              | otherwise = lookupp k right
							  
t :: Map Int Int
t = No 2 0 (No (-2) 0 Vazia Vazia) Vazia
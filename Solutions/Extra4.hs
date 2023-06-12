type G = ([V], [E])
type V = Int
type E = (Int,Int)
transitiva :: G -> Bool
transitiva (v,e) = and [isIn (fst k,snd a) e | k <- e, a <- find (snd k) e]

find :: Int -> [E] -> [E]
find a e = [k | k <- e , a == fst k] 

isIn :: E -> [E] -> Bool
isIn a e = or [a == k | k <- e] 
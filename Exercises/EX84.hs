module Ex84 where
import Data.Map
import Data.Char


ocorrences :: String -> IO()
ocorrences text = ocorrencesAux text (Data.Map.fromList [])

ocorrencesAux :: String -> Map Char Int -> IO()
ocorrencesAux [] p = [putStrLn((fst x):(show (snd x)):[]) | x <- Data.Map.toList p]
ocorrencesAux (x:xs) p | Data.Map.lookup xt p == Nothing = ocorrencesAux xs (Data.Map.insert xt 1 p)
                       | otherwise = let Just a = Data.Map.lookup xt p in ocorrencesAux xs (Data.Map.insert xt a)
                             where xt = Data.Char.toLower x
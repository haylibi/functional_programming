-- lista infinita de todos os números primos
primos :: [Integer]
primos = crivo [2..]
    where
     crivo (p:xs) = p : filter (\x -> x`mod`p /= 0) (crivo xs)
  
-- 3) listar *todos* os pares de primos gémeos
gemeos :: [(Integer,Integer)]
gemeos = filter p (zip primos (tail primos))
              where p a = (fromIntegral (fst a)) == (fromIntegral (snd a))-2
-- lista infinita de todos os números primos
primos :: [Integer]
primos = crivo [2..]
    where
     crivo (p:xs) = p : filter (\x -> x`mod`p /= 0) (crivo xs)

-- contar primos entre a e b
listarPrimos :: Integer -> Integer -> [Integer]
listarPrimos a b = filter p (take (fromIntegral b) primos)
                  where p x = if x>=a && x<=b then True else False
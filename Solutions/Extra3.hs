-- lista infinita de todos os números primos
primos :: [Integer]
primos = crivo [2..]
    where
     crivo (p:xs) = p : filter (\x -> x`mod`p /= 0) (crivo xs)
  
-- encontrar uma "testemunha" da conjetura de Goldbach
goldbach :: Integer -> (Integer,Integer)
goldbach n = head [(a, b) | a <- (takeWhile f primos), b <- (takeWhile f primos), a+b==n]
                     where f x = x <= n
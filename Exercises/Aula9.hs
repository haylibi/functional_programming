module Aula9 where

-- TESTES INICIAIS
str2Int :: String -> Int
str2Int s = read s 

ioSoma1 :: IO Int
ioSoma1 = do s <- getLine
             let i = str2Int s
             let ip1 = i + 1
             return ip1
           
pioSoma1 :: IO ()
pioSoma1 = do i <- ioSoma1
              putStrLn ("resultado: " ++ (show i))

-- FIM TESTES

-- Ex 64
elefantesList = [putStrLn ("Se " ++ (show i) ++ " incomodam muita gente,\n" ++ (show (i+1)) ++ " incomodam muito mais!") | i <- [2..]]

elefantes :: Int -> IO ()
elefantes n = sequence_ (take (n-2) elefantesList)

-- Ex 65
-- Ficheiro a parte
-- Ex 66
-- Ficheiro a parte
-- Ex 67
isLowChar c = or [c == k | k<-['a'..'z']]
isHighChar c = or [c == k | k <- ['A'..'Z']]

rotChar c n |isLowChar c = head (drop n (dropWhile (/= c) (take 52 (cycle ['a'..'z']))))
            |isHighChar c = head (drop n (dropWhile (/= c) (take 52 (cycle ['A'..'Z']))))
            |otherwise = c

cifra n s = [rotChar k n | k <- s]
-- Ficheiro a parte (o resto)

-- Ex 68
filtrar _ [] [] = []
filtrar x (s:xs) (aux:auxs)| x == s = (s: (filtrar x xs auxs))
                           | otherwise = (aux: (filtrar x xs auxs))
jogo n s aux = do putStrLn aux >> putStr "? "
                  c <- getChar
                  if (filtrar c s aux) == s then return n
			      else if (filtrar c s aux) == aux then 
                     do putStr "\nNao ocorre\n"; jogo (n+1) s aux
			           else 
                     do putChar '\n'; (jogo (n+1) s (filtrar c s aux))
adivinha :: String -> IO ()
adivinha s = do n <- jogo 1 s aux
                putChar '\n' >> putStr ("Adivinhou em " ++ (show n) ++ " tentativas")
				where aux = (take (length s) (cycle ['-']))

-- Ex 69

display [] _ = return ()
display (state:states) n = do putStrLn ((show n) ++ ": " ++  displayAux state)
                              display states (n+1)

displayAux s = take s (cycle ['*'])


jogoNim :: [Int] -> IO ()
jogoNim state = do display state 1
                   putStr "Player 1: "
                   p1 <- getLine
                   if ((update state (read p1)) == [0,0,0,0,0]) then 
				        do putStr "Winner: Player 1"; return ()
                   else 
                        do display (update state (read p1)) 1
                           putStr "Player 2: "
                           p2 <- getLine
                           if ((update (update state (read p1)) (read p2)) == [0,0,0,0,0]) then 
						       do putStr "Winner: Player 2"; return ()
                           else 
						       jogoNim (update (update state (read p1)) (read p2))


update :: [Int] -> Int -> [Int]
update s p = [f (a,b) | (a,b) <- zip s [1..]]
             where f (a,b) | b == p && a>0 = a - 1
			               | otherwise = a

nim = jogoNim [5,4,3,2,1]
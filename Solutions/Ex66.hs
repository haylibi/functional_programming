module Ex66 where
putStrLns [] = return ()
putStrLns (x:xs) = do putStr x
                      putStrLns xs

putStrLnsReverse [] = return ()
putStrLnsReverse (x:xs) = do putStr (reverse x) >> putStr "\n"
                             putStrLnsReverse xs

main :: IO ()
main = do inp <- getContents     --Nao esta bem certo, mas nao sei que funcao usar para obter os conteudos do input (com linhas)
          let rl = lines inp
          putStrLnsReverse rl
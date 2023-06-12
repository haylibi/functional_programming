module RPN where
import Stack

calc :: Stack Float -> String -> Stack Float
calc stack s | s == "+" = push (a + b) newStack
             | s == "-" = push (a - b) newStack
             | s == "*" = push (a * b) newStack
             | s == "/" = push (a / b) newStack
			 | otherwise = push (read s) stack
       where a = top stack
             b = top (pop stack)
             newStack = pop (pop stack)



calcular :: String -> Float
calcular s = top (foldl calc empty (words s))

main :: IO()
main = do inp <- getContents     --Nao esta bem certo, mas nao sei que funcao usar para obter os conteudos do input (com linhas)
          let rl = lines inp
          put rl

put [] = return ()
put (x:xs) = do putStr (show (calcular x)) >> putStr "\n"
                put xs

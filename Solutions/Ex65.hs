module Ex65 where

wc = do inp <- getLine
        let lista = words inp
        putStrLn ((show (length (lines inp))) ++ "    " ++ (show (length lista)) ++ "    " ++ (show (length inp)))
		
main :: IO()
main = wc
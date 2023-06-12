module Ex67 where 


isLowChar c = or [c == k | k<-['a'..'z']]
isHighChar c = or [c == k | k <- ['A'..'Z']]

rotChar c n |isLowChar c = head (drop n (dropWhile (/= c) (take 52 (cycle ['a'..'z']))))
            |isHighChar c = head (drop n (dropWhile (/= c) (take 52 (cycle ['A'..'Z']))))
            |otherwise = c

cifra n s = [rotChar k n | k <- s]


main :: IO ()
main = do s <- getLine
          putStrLn (cifra 13 s)
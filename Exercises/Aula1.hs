--dir: C:\Users\Duarte Pinto\Documents\MEGAsync\3 ano\Semestre 2\Prog Funcional\Exercicios

-- Ex1
inc x = x+1
quadrado x = x*x
dobro x = x+x
media x y = (x+y)/2

--inc (quadrado 5)
--inc (5 * 5)
--inc (25)
--25 + 1
--26

--quadrado (inc 5)
--quadrado (5+1)
--quadrado (6)
--6 * 6
--36

-- media (dobro 3) (inc 5)
-- media (3+3) (5+1)
-- media 6 6
-- (6+6)/2
-- 6


-- Ex2
triangulo a b c = a<b+c && 
                  b<a+c && 
                  c<a+b

triangulo2 a b c
	|a>=b+c = False
	|b>=a+c = False
	|c>=a+b = False
	|otherwise = True

-- Ex3
heron a b c = sqrt(s*(s-a)*(s-b)*(s-c))
  where {s = (a+b+c)/2}

-- Ex4
metades l = ( pp , sp ) 
  where pp = take x l
        sp = drop x l
        x = (length l) `div` 2


-- Ex5
-- a)
last1 xs = head (reverse xs)
last2 xs = xs !! (length xs - 1)
last3 xs = (reverse xs) !! 0
-- b)
init1 l = reverse (tail (reverse l))
init2 l = reverse (drop 1 (reverse l))

-- Ex6
--a)
binom n k = a `div` b 
  where a = product [1..n] 
        b = product ([1..k]++[1..n-k])

--b)
binom2 n k = a `div` b
  where a = product [k+1..n]
        b = product [1..n-k]

-- Ex7
--a)
max3 a b c |a >= max b c = a
           |b >= max a c = b
           |c >= max a b = c
min3 a b c |a <= min b c = a
           |b <= min a c = b
           |c <= min a b = c 

--b)
max_3 a b c = max a (max b c)
min_3 a b c = min a (min b c)

-- Ex8
--a)
maxOccurs a b = (x,y)
  where x = max a b
        y = (max a b) `div` (min a b)
--b)
orderTriple (a,b,c) = (x,y,z)
  where x = min_3 a b c
        y = (a+b+c)-x-z
        z = max_3 a b c

-- Ex9
classifica n |n <= 9 = "reprovado"
             |n > 9 && n <= 12 = "suficiente"
             |n > 12 && n <= 15 = "bom"
             |n > 15 && n <= 18 = "muito bom"
             |n > 18 && n <= 20 = "muito bom com distincao"
             |otherwise = "impossivel"

-- Ex10
xor True _ = True 
xor _ True = True
xor _ _ = False

-- Ex11
safetail1 [] = []
safetail1 l = tail l

safetail2 l = if l == [] then [] else tail l 

safetail3 l |l == [] = []
            |otherwise = tail l

-- Ex12
--a)
curtal xs = (length xs) <= 2

--b)
curta [] = True
curta [_] = True
curta [_,_] = True
curta _ = False

-- Ex13
textual 0 = "zero"
textual 1 = "um"
textual 2 = "dois"
textual 3 = "tres"
textual 4 = "quatro"
textual 5 = "cinco"
textual 6 = "seis"
textual 7 = "sete"
textual 8 = "oito"
textual 9 = "nove"
textual 10 = "dez"
textual 11 = "onze"
textual 12 = "doze"
textual 13 = "treze"
textual 14 = "quatorze"
textual 15 = "quinze"
textual 16 = "dezasseis"
textual 17 = "dezassete"
textual 18 = "dezoito"
textual 19 = "dezanove"
textual 20 = "vinte"
textual 30 = "trinta"
textual 40 = "quarenta"
textual 50 = "cinquenta"
textual 60 = "sessenta"
textual 70 = "setenta"
textual 80 = "oitenta"
textual 90 = "noventa"
textual 100 = "cem"
textual 200 = "duzentos"
textual 300 = "trezentos"
textual 400 = "quatrocentos"
textual 500 = "quinhentos"
textual 600 = "seiscentos"
textual 700 = "setecentos"
textual 800 = "oitocentos"
textual 900 = "novecentos"
textual 1000 = "mil"
textual 1000000 = "um milhao"
textual n |n<100 = textual ((n `div` 10)*10) ++ " e " ++ textual (n `mod` 10)
          |n<200 = "cento e " ++ textual (n `mod` 100)
          |n<1000 = textual ((n `div` 100)*100) ++ " e " ++ textual (n `mod` 100)
          |n<=1100 = "mil e " ++ textual (n `mod` 1000)
          |n<2000 = "mil " ++ textual(n `mod` 1000)
          |n<1000000 = textual (n `div` 1000) ++ " mil " ++ textual(n `mod` 1000)
          |n<=1001000 = textual (n `div` 10^6) ++ " milhao e " ++ textual(n `mod` 10^6)
          |n<2000000 = textual (n `div` 10^6) ++ " milhao " ++ textual(n `mod` 10^6)
          |otherwise = textual (n `div` 10^6) ++ " milhoes " ++ textual(n `mod` 10^6)


lastd xs = drop ((length xs) - 1) xs
lasth xs = head (reverse xs)
lastex xs = xs !! ((length xs) - 1)

initd xs = reverse (drop 1 (reverse (xs)))

factores n = [x | x <- [1..n], n `mod` x == 0]

esDivisor n x = n `mod` x == 0 -- Se usa la definicion de la funcion entre `` para usar los parametros infijos
divisores n = [x | x <- [1..n], esDivisor n x]

perfect n = [x | x <- [1..n], sum (init (factores x)) == x]

-- Clase del 4/9
-- Las listas [1,2,3,4,5,9] haskell las interpreta como 1:(2:(3:(4:(5:(9[])))))
-- Y se puede discriminar los elementos mandados como parametros

inicio :: [a] -> a
inicio (x:_) = x

finali (x:xs) = xs

sumarPrimeros :: [Int] -> Int
sumarPrimeros (a:b:_) = a+b

fAND :: Bool -> Bool -> Bool
True `fAND` True = True
_ `fAND` _ = False

fOR :: Bool -> Bool -> Bool
False `fOR` False = False
_ `fOR` _ = True

-- En el paradigma funcional tambien existe el if como expresion condicional

abso :: Int -> Int
abso n = if n >=0 then n else -n

-- Siempre deben contener un else

-- Otra forma es usando guardas

absoluto :: Int -> Int
absoluto n | n >=0 = n | otherwise = -n
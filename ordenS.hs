--Funciones currificadas
--Una funcion devuelve una funcion que recive multiples parametros
--Una funcion puede recibir una funcion como parametro

suma :: Int -> (Int -> Int)
suma x y = x + y

twice :: (a->a) -> a -> a
twice f x = f (f x)

--suma 5 4 = (suma 5) 4 = 9
--Se pueden guardar funciones como parcialmente aplicadas
--let sumaCinco = suma 5
--sumaCinco 4 = 9

--Funciones lambda
-- (\x -> x+x) 2

--map es un ejemplo de funcion de orden superior

mapComp f xs = [f x | x<-xs]

mapRec _ [] = []
mapRec f (x:xs) = f x : map f xs

--filter

--filter :: (a -> Bool) -> [a] -> [a]
--devuelve los elementos de una lista que devuelven verdadero en la primera funcion

--foldr
-- sum xs = foldr (+) 0 xd     ===     sum = foldr (+) 0

--Tipos de datos definidos por el usuario

data NumerosSimples = Uno | Dos | Muchos deriving (Show,Eq)

--Uso en funciones

convertir 1 = Uno
convertir 2 = Dos
convertir _ = Muchos

--Entrada Salida

hm :: IO ()
hm = do putStrLn "Hola Mundo"

hola :: IO ()
hola = do
    putStrLn "Nombre?"
    nombre <- getLine
    putStrLn ("Hola " ++ nombre ++ " !")

strlen :: IO()
strlen = do
    putStr "Ingrese palabra: "
    pal <- getLine
    putStr "La palabra tiene "
    putStr (show (length pal))
    putStrLn " caracteres"

prompt x = do
    putStrLn x
    number <- getLine
    return number

sucesor = do
    number <- prompt "Ingrese un numero"
    print $ succ (read number :: Int)

sumar = do
    number <- prompt "Ingrese un numero"
    let x = read number
    number <- prompt "Ingrese un numero"
    let y = read number
    putStrLn(show(x+y))

import System.Random (randomRIO)

juego tar x = do
    n <- prompt "Ingrese numero: "
    let guess = read n
    


launcher = do
    n <- randomRIO (1::Int,100)
    let tar = read n



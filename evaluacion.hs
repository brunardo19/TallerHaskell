--Funcion generica de ingreso de datos
prompt x = do
    putStrLn x
    getLine


-- Actividad 1
-- 1. Desarrollar las funciones que permitan:
-- a. Ingresar del nombre, edad y altura de diferentes personas
-- b. Armar una lista de tuplas del tipo [(String,Int,Float)], con la información ingresada.
-- c. Imprimir un detalle de los datos ingresados con el siguiente formato:
--      Nombre: xxxx Edad: 99 Altura: 99.99
--      Nombre: xxxx Edad: 99 Altura: 99.99
--      Nombre: xxxx Edad: 99 Altura: 99.99
-- d. Calcular e imprimir, a continuación del detalle, el promedio de edad y de altura.

mainPersona :: IO ()
mainPersona = do
    list <- ingreso []
    putStrLn ("Personas ingresadas") 
    mostrar list
    promedio list 0 0 0

-- a/b

ingreso :: [(String,Int,Float)] -> IO [(String,Int,Float)]
ingreso xs = do
    nombre <- prompt "Ingrese el nombre: "
    a <- prompt "Ingrese la edad: "
    let edad = read a :: Int
    a <- prompt "Ingrese la altura: "
    let altura = read a :: Float


    a <- prompt "Agregar una persona? (1/0): "
    let op = read a :: Int
    case op of
        1 -> ingreso (xs ++ [(nombre,edad,altura)])
        0 -> return (xs ++ [(nombre,edad,altura)])

-- c

mostrar :: [(String, Int, Float)] -> IO ()
mostrar [] = putStrLn "--------------------Fin-----------------------"
mostrar xs = do
    mostrarPersona (head xs)
    mostrar (tail xs)

mostrarPersona :: (String, Int, Float) -> IO ()
mostrarPersona (nombre, edad, altura) = do
    putStrLn $ "Nombre: " ++ show nombre ++ "           " ++  "Edad: " ++ show edad ++ "           " ++ "Altura: " ++ show altura

-- d

promedio :: [(String, Int, Float)] -> Int -> Float -> Int -> IO ()
promedio [] ed al len = do
    let a =  (fromIntegral ed :: Float) / (fromIntegral len :: Float)
    let b =  al / fromIntegral len
    putStrLn $ ("Promedio de edad : " ++ show a ++ " - Promedio de altura: " ++ show b)

promedio ((nombre, edad, altura):xs) ed al len= do
    promedio xs (ed + edad) (al + altura) (len + 1)


--Actividad 2
-- Desarrollar una función aDigitos :: Integer -> [Integer], que convierta un número en una lista de dígitos.
-- Por ejemplo: aDigitos 2345 = [2,3,4,5]. La función desarrollada debe cumplir con las siguientes propiedades:

aDigitos :: Integer -> [Integer]
aDigitos 0 = []
aDigitos n = (aDigitos (div n 10)) ++ [mod n 10]

--Actividad 3
-- Desarrolle una función sumaMult :: [Integer] -> [Integer] tome dos listas de la misma longitud y sume la
-- multiplicación de sus elementos en igual posición. Es decir, sumaMult [1,2,3,4] [3,4,5,6] = 50

sumaMult :: [Integer] -> [Integer] -> Integer
sumaMult [] [] = 0
sumaMult xa xb = (head xa) * (head xb) + (sumaMult (tail xa) (tail xb))

-- Actividad 4
-- Desarrolle una función agregarDigito:: Int -> [Integer] -> [Integer] que agregue un número a una lista de la
-- siguiente forma: que tome un valor entero x, lo divida por 11. Del resultado se tome la parte entera (z) y se
-- aplique la siguiente fórmula x - (z*11). El resultado es el dígito que se debe agregar a la lista dada. Por
-- ejemplo: agregarDigito 50 [1,2,3,4] = [1,2,3,4,6]

agregarDigito:: Integer -> [Integer] -> [Integer]

agregarDigito x xs = xs ++ [x-((div x 11)*11)]

--Actividad 5
-- Desarrolle una función ListaDigitos :: [Integer] -> [Integer], que tome una listas de enteros y devuelva una
-- lista de los dígitos de cada entero. Por ejemplo: ListaDigitos [1,12,34,5] = [1,1,2,3,4,5]

listaDigitos :: [Integer] -> [Integer]
listaDigitos [] = []
listaDigitos xs = aDigitos (head xs) ++ listaDigitos(tail xs)

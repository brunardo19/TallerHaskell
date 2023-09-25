--Funcion generica de ingreso de datos
prompt x = do
    putStrLn x
    getLine

mainPersona :: IO ()
mainPersona = do
    list <- ingreso []
    putStrLn ("Personas ingresadas") 
    mostrar list
    promedio list 0 0 (length list)

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

mostrar :: [(String, Int, Float)] -> IO ()
mostrar [] = putStrLn "--------------------Fin-----------------------"
mostrar xs = do
    mostrarPersona (head xs)
    mostrar (tail xs)

mostrarPersona :: (String, Int, Float) -> IO ()
mostrarPersona (nombre, edad, altura) = do
    putStrLn $ "Nombre: " ++ show nombre ++ "           " ++  "Edad: " ++ show edad ++ "           " ++ "Altura: " ++ show altura

promedio :: [(String, Int, Float)] -> Int -> Float -> Int -> IO ()
promedio [] ed al len = do
    
    let a = ed/len
    let b = al/ fromIntegral len
    putStrLn $ ("Promedio de edad : " ++ show (ed/len) ++ "Promedio de altura: " ++ show b)
promedio ((nombre, edad, altura):xs) ed al len= do
    let ed = ed + edad
    let al = al + altura
    promedio xs ed al len




aDigitos :: Integer -> [Integer]
aDigitos 0 = []
aDigitos n = (aDigitos (div n 10)) ++ [mod n 10]

sumaMult :: [Integer] -> [Integer] -> Integer
sumaMult [] [] = 0
sumaMult xa xb = (head xa) * (head xb) + (sumaMult (tail xa) (tail xb))

agregarDigito:: Integer -> [Integer] -> [Integer]

agregarDigito x xs = xs ++ [x-((div x 11)*11)]

listaDigitos :: [Integer] -> [Integer]
listaDigitos [] = []
listaDigitos xs = aDigitos (head xs) ++ listaDigitos(tail xs)

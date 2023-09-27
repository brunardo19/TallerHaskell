--Funcion generica de ingreso de datos
prompt x = do
    putStrLn x
    getLine

-- Actividad 1
-- Desarrollar las funciones que permitan ingresar e imprimir la información de un Ticket de
-- Compra. Para esto debe permitir:
--      a. el ingreso del código, descripción, cantidad y precio unitario de diferentes productos
--      b. armar una lista de tuplas del tipo [(Int,String,Int,Float)], con la información ingresada.
--      c. Imprimir el detalle del Ticket con el siguiente formato:
--          Cod: 999 Descripción: xxxx Cantidad: 99 Precio Unitario: 99.99
--          Cod: 999 Descripción: xxxx Cantidad: 99 Precio Unitario: 99.99
--          Cod: 999 Descripción: xxxx Cantidad: 99 Precio Unitario: 99.99
--      d. Calcular e imprimir, a continuación del detalle del Ticket, el costo total de la compra
--      (cantidad * precio unitario). 

mainTicket :: IO ()
mainTicket = do
    list <- ingreso []
    putStrLn ("Productos ingresados")
    mostrar list
    total list 0

-- a/b

ingreso :: [(Int,String,Int,Float)] -> IO [(Int,String,Int,Float)]
ingreso xs = do
    a <- prompt "Ingrese codigo del producto: "
    let cod = read a :: Int
    desc <- prompt "Ingrese descripcion: "
    a <- prompt "Ingrese cantidad: "
    let cant = read a :: Int
    a <- prompt "Ingrese precio por unidad: "
    let precio = read a :: Float


    a <- prompt "Agregar otro producto? (1/0): "
    let op = read a :: Int
    case op of
        1 -> ingreso (xs ++ [(cod,desc,cant,precio)])
        0 -> return (xs ++ [(cod,desc,cant,precio)])

-- c

mostrar :: [(Int, String, Int, Float)] -> IO ()
mostrar [] = putStrLn "--------------------Fin-----------------------"
mostrar xs = do
    mostrarTicket (head xs)
    mostrar (tail xs)

mostrarTicket :: (Int, String, Int, Float) -> IO ()
mostrarTicket (cod,desc,cant,precio) = do
    putStrLn $ "Cod: " ++ show cod ++ "           "
     ++  "Descripcion: " ++ show desc ++ "           "
     ++  "Cantidad: " ++ show cant ++ "           "
     ++ "Precio Unitario: " ++ show precio

-- d

total :: [(Int, String, Int, Float)] -> Float -> IO ()
total [] t = do
    putStrLn $ ("Costo Total: " ++ show t)

total ((cod,desc,cant,precio):xs) t= do
    total xs (t + (precio * (fromIntegral cant :: Float)))


-- Actividad 2
-- Desarrollar una función esValido :: Integer -> Bool, que permita verificar si un CUIL dado es válido
-- o no. El proceso para validar un CUIL es el siguiente:
-- a. El CUIL tiene el formato XY - DNI - Z, donde XY determina si es femenino (27), masculino
-- (20) o empresa (30) y el Z constituye en dígito verificador.
-- b. Para generar el dígito verificador, se debe multiplicar los 10 primero dígitos de la
-- siguiente manera X * 5, Y * 4, 1 * 3, 2 * 2, 3 * 7, 4 * 6, 5 * 5, 6 * 4, 7 * 3, 8 * 2.
-- c. Se suman dichos resultados. El resultado obtenido se divide por 11. De esa división se
-- obtiene un Resto que determina Z
-- i. Si el resto es 0 entonces Z=0

-- Funcion Auxiliar aDigitos
aDigitos :: Integer -> [Integer]
aDigitos 0 = []
aDigitos n = (aDigitos (div n 10)) ++ [mod n 10]

esValido :: Integer -> Bool
esValido x = length (aDigitos x) == 11 && vDigito (aDigitos x) (mod (vMulti (aDigitos x) [5,4,3,2,7,6,5,4,3,2] 0) 11) (vTipo (aDigitos x))

vTipo :: [Integer] -> String
vTipo (2:7:xs) = "f"
vTipo (2:0:xs) = "m"
vTipo (3:0:xs) = "e"
vTipo (2:3:xs) = "x"
vTipo xs = "n"

vMulti :: [Integer] -> [Integer] -> Integer -> Integer
vMulti _ [] x = x
vMulti (a:as) (b:bs) x = vMulti as bs (x+(a*b))

vDigito :: [Integer] -> Integer -> String -> Bool
vDigito xs r t =
    case t of
    "n" -> False
    _ -> case r of
        0 -> last xs == 0
        1 -> t == "x" && (last xs == 9 || last xs == 4)
        _ -> last xs == 11 - r
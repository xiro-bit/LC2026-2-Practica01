module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float| --representa cada uno un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show, Eq)

--Funcion que calcula el area de las figuras

-- constante pi: definimos la constante pi para usarla en el caso del circulo
numeropi :: Float
numeropi = 3.1416


area :: Shape -> Float
area (Square x) = x ** 2
area (Rectangle x y) = x * y
area (Circle x) = (x**2) * numeropi
area (Triangle x) = sqrt(s * (s-x)* (s-x) * (s-x))
    where 
        s= semiperimetro (Triangle x)
area (Trapeze x y z) = ((x + y)/2) * z


-- Obtener el semiperimetro para poder sacar el área del triangulo
semiperimetro :: Shape -> Float
semiperimetro (Triangle x) = perimeter (Triangle x) /2


--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Square x) = x * 4
perimeter (Rectangle x y) = (x + y) * 2 
perimeter (Circle x) = (2 * x) * numeropi
perimeter (Triangle x) = x * 3
perimeter (Trapeze x y z) = x + y + 2* sqrt(z**2 +((x-y)/2)**2)




--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float,Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x,y) (x1,y1) = sqrt((x1-x)**2 + (y1-y)**2)

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (x,y)= sqrt(x**2 + y **2)
    

--Ejercicio 3
data Haskellium = Haskellium {name :: String,
                             lastName1 :: String,
                             lastName2 :: String,
                             houseShape :: Shape,
                             location :: Point} 
                             deriving (Show, Eq)

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
--se asume que padre 1 y padre2 viven en la misma casa asi que la forma y la locacion es la misma
son :: Haskellium -> Haskellium -> String -> Haskellium
son padre1 padre2 nombre = Haskellium {name = nombre, lastName1 = lastName1 padre1, lastName2 = lastName1 padre2, houseShape = houseShape padre1, location= location padre2}

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost h = area shape + perimeter shape * 2.5
    where shape = houseShape h
        

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork hask 
    |distancia < 300 = distancia / 30  -- se ira en bici
    |distancia >= 300  = distancia/ 70   -- se ira en moto
    where distancia = from0 (location hask)

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo palabra = lista == palabraEnReversa lista
    where lista = palabraenCaracter palabra


--funcion auxiliar que devuelve una lista de caracteres una palabra dada
palabraenCaracter :: String -> [[Char]]
palabraenCaracter word = [[c]| c <- word]

--funcion auxiliar que devuelve la lista de caracteres al revés
palabraEnReversa ::  [[Char]] -> [[Char]]
palabraEnReversa []=[]
palabraEnReversa (x:xs) = palabraEnReversa xs ++ [x]



--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr funcion acumulador []     = acumulador
myFoldr funcion acumulador (x:xs) = funcion x (myFoldr funcion acumulador xs)

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [x:ys| ys <-conjuntoPotencia xs] ++ conjuntoPotencia xs 

--ARBOLES

--Implementacion

data OneTwoTree a = Void| Node a (OneTwoTree a)| Branch a (OneTwoTree a) (OneTwoTree a)


--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Void = 0
suma (Node x arbol) = x + suma arbol
suma (Branch x arbol1 arbol2) = x + suma arbol1 + suma arbol2
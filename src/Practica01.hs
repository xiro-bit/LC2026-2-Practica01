module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float| --representa cada uno un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show)

--Funcion que calcula el area de las figuras

-- constante pi: definimos la constante pi para usarla en el caso del circuloare
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
perimeter (Trapeze x y z) = ((x + y)/2) * z




--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float,Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x,y) (x1,y1) = sqrt((x1-x)**2 + (y1-y)**2)

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (x,y)= sqrt(x**2 + y **2)
    

--Ejercicio 3
data Haskellium = Undefined

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son = undefined

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost = undefined

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork = undefined

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo = undefined

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr = undefined

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = undefined

--ARBOLES

--Implementacion

data OneTwoTree a = Undefinedd

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma = undefined
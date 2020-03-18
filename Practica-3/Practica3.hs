-- Espinal Cruces Martin Felipe

module Practica3 where 

 import Binario
 import Practica2

--Ejercicio 2.1
 binarios :: [Int] -> [Binario]
--solo hay que usar la funcion con el map a la lista que el usuario de
 binarios (x) = map (natToBin) (x)

--Ejercicio 2.2
 pares :: [Binario] -> [Binario]
  --solo hay que leer el ultimo bit que seria el mas chico que representa un uno y asi saber si es par o no
 pares a = filter esPar a
-- pares a = filter (\x ->) a
--intente hacerlo con una lamda pero no supe como hacerlo sin el patern matching 
 esPar :: Binario -> Bool
 esPar BaseUno = False
 esPar (Uno(x)) = False
 esPar (Cero(x)) = True

--Ejercicio 2.3
 tooLong :: [String] -> [String]
 --Tu código va aquí
 --Investigue que esta es la forma de poner la condición de x's que cumplen tal cosa
 tooLong xs = filter (\x -> length x <= 7) xs


--Ejercicio 2.4
 sFibonacci :: Int -> [Int]
 --solo hace falta hacer un map en la funcion fib sobre la lista n
 sFibonacci n = map (fibonacci) [0..n] 

--Ejercicio 2.5
--basta con filtrar los elementos diferentes al elmento parámetro
 quitaElemento :: (Eq a) => [a] -> a -> [a]
 --Tu código va aquí
 quitaElemento xs x = filter (/=x) xs

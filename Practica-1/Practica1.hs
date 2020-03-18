--ESTRUCTURAS DISCRETAS 2019-1
--Fernandez Aguilar Alex Gerardo
--Práctica 1

--Ejercicio 1.1
areaCirculo :: Float -> Float
areaCirculo a = pi*a

--Ejercicio 1.2
distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (a,b) (c,d)= sqrt(((a-c)^2)+((b-d)^2))

--Ejercicio 1.3
imp :: Bool -> Bool -> Bool
imp x y = not x || y

--Ejercicio 1.4:
xor :: Bool -> Bool -> Bool
xor x y = (x||y)&&(not(x&&y))

--Ejercicio 1.5
mes :: Int -> String 
mes 1 = "Enero" 
mes 2 = "Febrero"
mes 3 = "Marzo"
mes 4 = "Abril"
mes 5 = "Mayo"
mes 6 = "Junio"
mes 7 = "Julio"
mes 8 = "Agosto"
mes 9 = "Septiembre"
mes 10 = "Octubre"
mes 11 = "Noviembre"
mes 12 = "Diciembre"


--Ejercicio 1.6
calculadora :: String -> (Int ,Int) -> Int
calculadora "first" (a,b) = a
calculadora "last" (a,b) = b
calculadora "sum" (a,b) = a+b
calculadora "rest" (a,b) = a-b
calculadora "mul" (a,b) = a*b
calculadora "div" (a,b)= div a b
calculadora "pow" (a,b)= a^b

--Ejercicio 1.7
loki :: Int -> Bool -> String
loki a False = if ((a>15)&&(a<25)) then "Sale a jugar" else "No sale a jugar"
loki a True = if ((a>20)&&(a<30)) then "Sale a jugar" else "No sale a jugar"

--Ejercicio 1.8
monos :: Bool -> Bool -> String
monos x y = if (x == y) then "Hay Problemas" else "No hay Problemas"

-- Funcion Suma
suma :: Int -> Int -> Int 
suma 0 n = n
suma n m = suma (pred n) (succ m)

--Ejercicio 1.9
multiplica :: Int -> Int -> Int
multiplica 1 b = b
multiplica a b = suma b (multiplica (pred a) b)  

--Ejercicio 1.10
potencia :: Int -> Int -> Int
potencia a 0 = 1
potencia a b = multiplica a (potencia a (pred b)) 

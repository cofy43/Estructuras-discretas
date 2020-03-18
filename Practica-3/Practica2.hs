--Espinal Cruces Martin Felipe

module Practica2 where

--DEFINICIÓN DE LISTAS

--1.1: Naturales.
nat = [x|x<-[1,2..]]
--1.2: Multiplos de diez.
multiplosDiez = [x*10|x<-nat]
--1.3: potencias de 2.
potenciasDos = [x**2|x<-nat]

--1.5: años desde el año de tu nacimiento.
anosVividos = [x|x<-[1998,1999..2018]]


--DEFINICIÓN DE FUNCIONES

--Ejercicio 2.1
fibonacci :: Int -> Int
fibonacci 0 = 0 
fibonacci 1 = 1 
fibonacci 2 = 1
fibonacci n = fibonacci (pred n) + fibonacci (n-2) 

--Ejercicio 2.2
elemento :: (Eq a) => [a] -> a -> Bool
elemento [] _ = False
elemento [x] a =if(x==a) then True else False
elemento (x:xs) a =if(x==a) then True else elemento xs a

--Ejercicio 2.3
sumaLista ::(Num a) => [a] -> a 
sumaLista [] = 0
sumaLista [a] = a
sumaLista (x:xs)= x + sumaLista xs 

--Ejercicio 2.4
meses :: [Int] -> [String]
meses [] = error "No ingresaste ningun mes"
meses [a] = [mes a] 
meses (x:xs) = (mes x):(meses xs)
--ejercicio de la practica anterior
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


--Ejercicio 2.5
divisoresPropios :: Int -> [Int]
divisoresPropios 0 = error "el cero no es divisible entre ningun numero"
divisoresPropios 1 = [1]
divisoresPropios n = 1:divisores n (pred n)
--funcion auxiliar
--lo que hace la funcion es ir calculando los divisores del numero empezando del mas grande al mas chico,
--pero para imprimirlos en el orden desado solo tuve que divir el divisor mayor entre la cifra y asi obtener el divisor menor,
--puesto que si un numero es divisible entre x la divison del numero entre x tambien es un factor propio
divisores :: Int -> Int -> [Int]
divisores n 1 = []
divisores n m = if((mod n m)==0)then (div n m):(divisores n (pred m))else divisores n (pred m)


--Ejercicio 2.6
esPerfecto :: Int -> Bool
esPerfecto  a = if((sumaLista(divisoresPropios a)) == a)then True else False

--Ejercicio 2.7
--al ya tener una funcion que suma los elementos de la lista de sus div propios solo queda comprarlos entre si para saber que uno es amigo del otro y el otro es amigo de uno
sonAmigos :: Int -> Int -> Bool
sonAmigos a b =if(((sumaLista(divisoresPropios a)) == b)&&((sumaLista(divisoresPropios b)) == a)) then True else False


--Ejercicio 2.8
supersuma :: Int -> Int
supersuma a =if(((div a 10)<10)&&((mod a 10)==0))then a else (mod a 10)+ supersuma(div (a -(mod a 10)) 10 )

--Ejercicio 2.9
japones :: Int -> String
japones a = if(a<10)then nombresJ a else (nombresJ (div(a-(mod a 10))10))++(" ju ")++(nombresJ (mod a 10))
--funcion auxiliar con los nombres en japones
nombresJ :: Int -> String
nombresJ 0 = "rei"
nombresJ 1 = "ichi"
nombresJ 2 = "ni"
nombresJ 3 = "san"
nombresJ 4 = "yon"
nombresJ 5 = "go"
nombresJ 6 = "roku"
nombresJ 7 = "nana"
nombresJ 8 = "haci"
nombresJ 9 = "kyu"


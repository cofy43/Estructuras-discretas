-- Alex Gerardo Fernandez Aguilar 314338097
-- Luis Erick Montes Garcia 419004547

 --Ejercicio 1.1
 data Binario = BaseUno | Uno Binario | Cero Binario deriving (Eq)

 instance Show Binario where
  show BaseUno = "1"
  show (Cero b) = show b ++ "0"
  show (Uno b) = show b ++ "1"

--Ejercicio 1.2
 natToBin :: Int -> Binario
 natToBin 1 = BaseUno
 natToBin a 
  | (mod a 2) == 0 = (Cero(natToBin(div a 2)))
  | (mod a 2) == 1 = (Uno(natToBin(div a 2)))

--Ejercicio 1.3
 binToNat :: Binario -> Int
 binToNat BaseUno = 1
 binToNat (Uno(x)) = 1+2*(binToNat(x))
 binToNat (Cero(x)) = 2*(binToNat(x))

--Ejercicio 1.4
 sucesor :: Binario -> Binario
 --Iniciamos reemplazando los 1's por 0´s, si nos encontramos
 --con un 0 lo reemplazamos y copiamos lo siguiente tal cual
 --si no caemos en caso BaseUno que quiere decir aumentar un uno y los demás 0
 sucesor BaseUno = Cero BaseUno
 sucesor (Uno b) = Cero (sucesor b)
 sucesor (Cero b) = Uno b


--Ejercicio 1.5
 bitsEncendidos :: Binario -> Int
  --Si es de la forma Uno B o BaseUno sumamos 1 a nuestro int
 bitsEncendidos BaseUno = 1
 bitsEncendidos (Uno(x)) = 1+(bitsEncendidos(x))
 bitsEncendidos (Cero(x)) = 0*(bitsEncendidos(x))
module Util
    where

import Data.Char

-- canon $ clean $ toLower' "Aqui va el String"

readInt :: String -> Int
readInt numStr = (read :: String -> Int) numStr

--Funcion principal que retira carecteres de una cadena de texto
canon :: String -> String
canon [] = []
canon (x:xs)
 | (x=='.') = canon xs
 | (x==',') = canon xs
 | (x=='-') = canon xs
 | (x=='_') = canon xs
 | (x=='/') = canon xs
 | (x=='&') = canon xs
 | (x=='?') = canon xs
 | (x=='¿') = canon xs
 | (x==' ') = canon xs
 | otherwise = toLower x :canon xs

--Funcion que retira articulos de una oracion
clean :: String -> String
clean [] = []
clean (x:y:z:xs)
 | (x:y:z:[]) == "the" = clean xs
 | (x:y:z:[]) == " a " = clean xs
 | (x:y:z:[]) == " an " = clean xs
 | otherwise = x : clean (y:z:xs)
clean x = x

--Lectura de la cadena, que utiliza dos funciones auxiliares
toLower' :: String -> String
toLower' [] = []
toLower' (x:xs) = (toLower x) : (toLower' xs)

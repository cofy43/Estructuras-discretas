module Snoc where

 --Tipo de dato algebraico para definir listas Snoc
 data SnocList a = Mt
                 | Snoc (SnocList a) a
                 deriving (Eq, Ord, Show)

 --Ejercicio1.1
 addSnoc :: SnocList a -> a -> SnocList a
 --solo hay que tomar la snoc list y agregarle un elemento de la forma en la que esta diseñada snolist
 addSnoc a e = (Snoc (a) e)

 --Ejercicio1.2
 ultimo :: SnocList a -> a
 --solo hay que regresar la cabeza
 --solo com duda no se que debaria pasar si solo contiene Mt
 ultimo (Snoc (s) e) = e

 --Ejercicio1.3
 resto :: SnocList a -> SnocList a
 --solo hay que quitarle la cabeza de la lista
 resto (Snoc (s) e) = s

 --Ejercicio1.4
 cabeza :: SnocList a -> a
 --solo hay que regresar el ultimo elemento de la cola de la lista
 cabeza (Snoc (Mt) a) = a
 cabeza (Snoc (s) a) = cabeza (s)

 --Ejercicio1.5
 cola :: SnocList a -> SnocList a
 -- regresa la lista sin el ultimo lemento de la cola
 cola (Snoc (Mt) a) = Mt
 cola (Snoc (s) a) = (Snoc (cola (s)) a)

 --Ejercicio1.6
 longitud :: SnocList a -> Int
 --solo hay que ir sumando elemento por elemento en la recursion quitandole la cabeza cada vez hasta llegar al caso base
 longitud (Snoc (Mt) a) = 1
 longitud (Snoc (s) a) = 1+(longitud (s))

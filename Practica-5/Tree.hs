module Tree where

 -- Tipo de dato Algebraico para definir Árboles Binarios
 data BinaryTree a = Void
                   | Node (BinaryTree a) a (BinaryTree a)
                   deriving (Eq,Ord,Show)

 --Ejercicio 2.1
 addTree :: (Ord a) => BinaryTree a -> a -> BinaryTree a
 --
 addTree (Void) e = (Node Void e Void)
 addTree (Node (ai) r (ad) ) e 
  | e < r = (Node (addTree ai e) r ad )
  | e >= r = (Node ai r (addTree ad e) )
  
 --Ejercicio 2.2
 inorder :: BinaryTree a -> [a]
 --
 inorder (Void) = []
 inorder (Node Void r Void) = [r]
 inorder (Node (ai) r (ad)) = (inorder ai) ++ [r] ++ (inorder ad)

 --Ejercicio 2.3
 preorder :: BinaryTree a -> [a]
 --
 preorder (Void) = []
 preorder (Node Void r Void) = [r]
 preorder (Node ai r ad ) = [r] ++ (preorder ai ) ++ (preorder ad )

 --Ejercicio 2.4
 postorder :: BinaryTree a -> [a]
 postorder (Void) = []
 postorder (Node Void r Void ) = [r]
 postorder (Node ai r ad ) = (postorder ai ) ++ (postorder ad ) ++ [r]

 --Ejercicio 2.5
 maximo :: (Ord a) => BinaryTree a -> a
 --ya sabiendo que es un arbol binario ordenado solo hay que tomar el elemento mas a la derecha
 maximo (Node Void r Void) = r
 maximo (Node ai r ad ) = (maximo ad )

 --Ejercicio 2.6
 minimo :: (Ord a) => BinaryTree a -> a
 --sabiendo que es un arbol ordenado solo hay que tomar el elemto mas a la izquierda
 minimo (Node Void r Void) = r
 minimo (Node ai r ad ) = (minimo ai)

 --Ejercicio 2.7
 busca :: (Ord a) => a -> BinaryTree a -> Bool
 busca = error "Falta Implementar"

import Snoc
import Tree

--Tipo de dato algebraico para pruebas
data Test = P Bool

--Instancia de Show en donde se imprime si el resultado de la prueba es correcto o no
instance Show Test where
 show (P True) = "CORRECTO"
 show (P False) = "ERROR"

main = do
 -- Definiciones de listas Snoc para pruebas.
 let snoc1 = (Snoc (Snoc (Snoc (Snoc (Snoc Mt 1) 2) 3) 4) 5)
 let snoc2 = (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Mt 'H') 'O') 'L') 'A') 'M') 'U') 'N') 'D') 'O')
 let snoc3 = (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Mt []) [1]) [1,2]) [1,2,3]) [1,2,3,4]) [1,2,3,4,5])
 let snoc4 = (Snoc (Snoc (Snoc (Snoc (Snoc Mt Mt) (Snoc Mt 1)) (Snoc (Snoc Mt 1) 2)) (Snoc (Snoc (Snoc Mt 1) 2) 3)) Mt)
 let snoc5 = (Snoc (Snoc (Snoc (Snoc (Snoc Mt True) False) True) True) False)

 print "LISTAS SNOC"
 print "---------- Pruebas del Ejercicio 1.1; Función: addSnoc ----------"
 print (P((addSnoc snoc1 6) == (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Mt 1) 2) 3) 4) 5) 6)))
 print (P((addSnoc snoc2 '!') == (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Mt 'H') 'O') 'L') 'A') 'M') 'U') 'N') 'D') 'O') '!')))
 print (P((addSnoc snoc3 [1,2,3,4,5,6]) == (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Mt []) [1]) [1,2]) [1,2,3]) [1,2,3,4]) [1,2,3,4,5]) [1,2,3,4,5,6])))
 print (P((addSnoc snoc4 (Snoc (Snoc (Snoc (Snoc Mt 1) 2) 3) 4)) == (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Mt Mt) (Snoc Mt 1)) (Snoc (Snoc Mt 1) 2)) (Snoc (Snoc (Snoc Mt 1) 2) 3)) Mt) (Snoc (Snoc (Snoc (Snoc Mt 1) 2) 3) 4))))
 print (P((addSnoc snoc5 True) == (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Mt True) False) True) True) False) True)))

 print "---------- Pruebas del Ejercicio 1.2; Función: ultimo ----------"
 print (P((ultimo snoc1) == 5))
 print (P((ultimo snoc2) == 'O'))
 print (P((ultimo snoc3) == [1,2,3,4,5]))
 print (P((ultimo snoc4) == Mt))
 print (P(not $ ultimo snoc5))

 print "---------- Pruebas del Ejercicio 1.3; Función: resto ----------"
 print (P((resto snoc1) == (Snoc (Snoc (Snoc (Snoc Mt 1) 2) 3) 4)))
 print (P((resto snoc2) == (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Mt 'H') 'O') 'L') 'A') 'M') 'U') 'N') 'D')))
 print (P((resto snoc3) == (Snoc (Snoc (Snoc (Snoc (Snoc Mt []) [1]) [1,2]) [1,2,3]) [1,2,3,4])))
 print (P((resto snoc4) == (Snoc (Snoc (Snoc (Snoc Mt Mt) (Snoc Mt 1)) (Snoc (Snoc Mt 1) 2)) (Snoc (Snoc (Snoc Mt 1) 2) 3))))
 print (P((resto snoc5) == (Snoc (Snoc (Snoc (Snoc Mt True) False) True) True)))

 print "---------- Pruebas del Ejercicio 1.4; Función: cabeza ----------"
 print (P((cabeza snoc1) == 1))
 print (P((cabeza snoc2) == 'H'))
 print (P((cabeza snoc3) == []))
 print (P((cabeza snoc4) == Mt))
 print (P(cabeza snoc5))

 print "---------- Pruebas del Ejercicio 1.5; Función: cola ----------"
 print (P((cola snoc1) == (Snoc (Snoc (Snoc (Snoc Mt 2) 3) 4) 5)))
 print (P((cola snoc2) == (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Mt 'O') 'L') 'A') 'M') 'U') 'N') 'D') 'O')))
 print (P((cola snoc3) == (Snoc (Snoc (Snoc (Snoc (Snoc Mt [1]) [1,2]) [1,2,3]) [1,2,3,4]) [1,2,3,4,5])))
 print (P((cola snoc4) == (Snoc (Snoc (Snoc (Snoc Mt (Snoc Mt 1)) (Snoc (Snoc Mt 1) 2)) (Snoc (Snoc (Snoc Mt 1) 2) 3)) Mt)))
 print (P((cola snoc5) == (Snoc (Snoc (Snoc (Snoc Mt False) True) True) False)))

 print "---------- Pruebas del Ejercicio 1.6; Función: longitud ----------"
 print (P((longitud snoc1) == 5))
 print (P((longitud snoc2) == 9))
 print (P((longitud snoc3) == 6))
 print (P((longitud snoc4) == 5))
 print (P((longitud snoc5) == 5))

 --Definiciones de Arboles binarios para pruebas.
 let bt1 = (Node (Node (Node Void 1 Void) 2 (Node Void 3 Void)) 4 (Node (Node Void 5 Void) 6 (Node Void 7 Void)))
 let bt2 = (Node (Node (Node (Node Void 'a' Void) 'b' (Node Void 'c' Void)) 'd' (Node (Node Void 'e' Void) 'f' (Node Void 'g' Void))) 'h'
                 (Node (Node (Node Void 'i' Void) 'j' (Node Void 'k' Void)) 'l' (Node (Node Void 'm' Void) 'n' (Node Void 'o' Void))))
 let bt3 = (Node (Node (Node Void 'h' Void) 'i' Void) 'm' (Node (Node Void 'n' Void) 'o' Void))
 let bt4 = (Node Void 1 Void)
 let bt5 = (Node (Node (Node Void [1] Void) [2,1] (Node Void [3,2,1] Void)) [4,3,2,1]
           (Node (Node Void [5,4,3,2,1] Void) [6,5,4,3,2,1] (Node Void [7,6,5,4,3,2,1] Void)))

 print "ÁRBOLES BINARIOS"

 print "---------- Pruebas del Ejercicio 2.1; Función: addTree ----------"
 print (P((addTree (addTree bt1 0) 8) == (Node (Node (Node (Node Void 0 Void) 1 Void) 2 (Node Void 3 Void)) 4 (Node (Node Void 5 Void) 6 (Node Void 7 (Node Void 8 Void))))))
 print (P((addTree bt2 'z') == (Node (Node (Node (Node Void 'a' Void) 'b' (Node Void 'c' Void)) 'd' (Node (Node Void 'e' Void) 'f' (Node Void 'g' Void))) 'h' (Node (Node (Node Void 'i' Void) 'j' (Node Void 'k' Void)) 'l' (Node (Node Void 'm' Void) 'n' (Node Void 'o' (Node Void 'z' Void)))))))
 print (P((addTree bt3 's') == (Node (Node (Node Void 'h' Void) 'i' Void) 'm' (Node (Node Void 'n' Void) 'o' (Node Void 's' Void)))))
 print (P((addTree bt4 2) == (Node Void 1 (Node Void 2 Void))))
 print (P((addTree bt5 []) == (Node (Node (Node (Node Void [] Void) [1] Void) [2,1] (Node Void [3,2,1] Void)) [4,3,2,1] (Node (Node Void [5,4,3,2,1] Void) [6,5,4,3,2,1] (Node Void [7,6,5,4,3,2,1] Void)))))

 print "---------- Pruebas del Ejercicio 2.2; Función: inorder ----------"
 print (P((inorder bt1) == [1,2,3,4,5,6,7]))
 print (P((inorder bt2) == "abcdefghijklmno"))
 print (P((inorder bt3) == "himno"))
 print (P((inorder bt4) == [1]))
 print (P((inorder bt5) == [[1],[2,1],[3,2,1],[4,3,2,1],[5,4,3,2,1],[6,5,4,3,2,1],[7,6,5,4,3,2,1]]))

 print "---------- Pruebas del Ejercicio 2.3; Función: preorder ----------"
 print (P((preorder bt1) == [4,2,1,3,6,5,7]))
 print (P((preorder bt2) == "hdbacfegljiknmo"))
 print (P((preorder bt3) == "mihon"))
 print (P((preorder bt4) == [1]))
 print (P((preorder bt5) == [[4,3,2,1],[2,1],[1],[3,2,1],[6,5,4,3,2,1],[5,4,3,2,1],[7,6,5,4,3,2,1]]))

 print "---------- Pruebas del Ejercicio 2.4; Función: postorder ----------"
 print (P((postorder bt1) == [1,3,2,5,7,6,4]))
 print (P((postorder bt2) == "acbegfdikjmonlh"))
 print (P((postorder bt3) == "hinom"))
 print (P((postorder bt4) == [1]))
 print (P((postorder bt5) == [[1],[3,2,1],[2,1],[5,4,3,2,1],[7,6,5,4,3,2,1],[6,5,4,3,2,1],[4,3,2,1]]))

 print "---------- Pruebas del Ejercicio 2.5; Función: maximo ----------"
 print (P((maximo bt1) == 7))
 print (P((maximo bt2) == 'o'))
 print (P((maximo bt3) == 'o'))
 print (P((maximo bt4) == 1))
 print (P((maximo bt5) == [7,6,5,4,3,2,1]))

 print "---------- Pruebas del Ejercicio 2.6; Función: minimo ----------"
 print (P((minimo bt1) == 1))
 print (P((minimo bt2) == 'a'))
 print (P((minimo bt3) == 'h'))
 print (P((minimo bt4) == 1))
 print (P((minimo bt5) == [1]))
{--
 print "---------- Pruebas del Ejercicio 2.7; Función: busca ----------"
 print (P(busca 4 bt1))
 print (P(busca 'a' bt2))
 print (P(not $ busca 'a' bt3))
 print (P(not $ busca 1729 bt4))
 print (P(busca [6,5,4,3,2,1] bt5))

--}
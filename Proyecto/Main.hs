module Main
    where
--Aqui se importa las funciones principales del juego
import qualified Menu

main = do
    putStrLn Menu.options

    option <- getLine

    Menu.menu option

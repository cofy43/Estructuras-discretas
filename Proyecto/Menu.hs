module Menu
    where

import System.IO
import System.Random
--Aqui se importa las funciones para obtener la forma canonica de la respuesta del usuario
import qualified Util

--Funcion que muestran en pantalla las opciones del juego
options :: String
options = "[1] Jugar \129513 \n[2] Instrucciones 📜 \n[3] Salir ❎"

--Funcion auxiliar para verificar la respuesta en su forma canonica
verify :: String -> (String, String) -> Bool
verify r t = ((Util.canon $ Util.clean $ Util.toLower' r) == (Util.canon $ Util.clean $ Util.toLower' $ fst t))

--Funcion principal del juego
subPlay :: IO()
subPlay = do
    --Aqui se verifica el numero de vidas del jugador
    content <- readFile ".life-count.dat"
    --Contador de vidas del juego
    let lifeCount = Util.readInt $ head $ words $ content
    --Se verifica si el jugador no tiene vidas
    if (lifeCount <= 0) then do
        --En caso de no tener se envia un mensaje preguntando si se quiere seguir jugando
        putStrLn "Se te acabaron las vidas. ¿Quieres iniciar de nuevo? (s/n)"
        confirm <- getLine
        --Se verifica la respuesta del jugador
        if (confirm == "s") then do
            --En caso de confirmar con si se crean 5 vidas y comienza el juego
            writeFile ".life-count.dat" "5"
            subPlay
        else do
            --En caso de no confirmar entonces se envia de nuevo al menu principal
            putStrLn options
            opt <- getLine
            menu opt
    --En caso de tener suficientes vidas entonces comienza el juego
    else do
        -- Get data from emoji.txt
        --Aqui se hace la lectura de la lista de posibles peliculas a preguntar
        handle <- openFile "data.dat" ReadMode
        contents <- hGetContents handle
        let movies = map (read :: String -> (String, String)) (lines contents)
        -- Random tuple from data
        --Numero aleatorio de seleccion de pelicula
        num <- randomRIO (0, 119) :: IO Int
        --Se propone aleatoriamente la pelicula a preguntar
        let tuple = movies !! num
        putStrLn "¿Qué pélicula es?"
        --Se pregunta el titulo de la pelicula mostrando el segundo elemento de la tupla
        putStrLn $ snd tuple
        respuesta <- getLine
        --Se verifica la respuesta del jugador
        if ((verify respuesta tuple)) then do
            putStrLn "\9989"
            subPlay
        else if (respuesta == "/salir") then do
            putStrLn options
            opt <- getLine
            menu opt
        else do
            putStrLn "\10060"
            --En caso de haber propuesto una respuesta erronea se reduce una vida del contador
            writeFile ".life-count.dat" (show $ pred lifeCount)
            subPlay

--Intrucciones del juego
tutorial :: IO()
tutorial = putStrLn "\nInstrucciones: \n\
	\ El juego consiste en adivinar el nombre de la película \
	\ con los emojis que se mostrarán.\n \
	\ En el juego tendras 5 vidas, se te quitará una vida si respondes mal al nombre  \
	\ de la película, de constestar bien automáticamente se lanzará otra pregunta, si quieres salir del juego puedes responder con: /salir\
	\ \n\nPresiona intro para continuar\n"

--Opcion del menu para jugar que invoca a la funcion principal del juego
menu :: String -> IO()
menu "1" = subPlay

--Opcion que muestra las instrucciones del juego y que vuelve a preguntar por una opcion del menu principal
menu "2" = do
    tutorial
    putStrLn options
    opt <- getLine
    menu opt

--Opcion que da por terminado el juego mostrando un mensaje de despedida
menu "3" = putStrLn "Ok ciao 👋"

--En caso de recibir una opcion invalida vuelve a solicitar por una opcion del menu
menu n = do
    putStrLn "Ingrese una opción válida."
    putStrLn options
    opt <- getLine
    menu opt

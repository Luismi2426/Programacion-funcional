{-             Práctica de Programación Declarativa en Haskell por:
        
- Alvaro Rubio Reseco
- Luis Miguel Barbero Juvera

Esta práctica simula un juego de cartas con unas reglas parecidas a las usadas en el BlackJack
Para iniciar el juego basta con ejecutar la orden main
-}


import Data.List
import System.Random


-- Baraja de juego y repartir

data Palo = Corazones | Diamantes | Picas | Treboles
     deriving (Show, Eq, Enum)
     
data Valor = As | Dos | Tres| Cuatro | Cinco | Seis | Siete | Ocho | Nueve | Diez | Jota | Dama | Rey
     deriving (Show, Eq, Enum)
     
type Carta = (Valor, Palo)

type Baraja = [Carta] 

type Mano = [Carta]
  

crearBaraja :: Baraja
crearBaraja = [(valor, palo) | palo <- [Corazones ..], valor <- [As ..]]

darCartas :: Int -> Mano -> Baraja -> (Mano, Baraja)
darCartas n mano baraja = (mano', baraja')
     where cartas = take n baraja
           baraja' = drop n baraja
           mano' = mano ++ cartas

eliminarCarta :: Carta -> Baraja -> Baraja
eliminarCarta carta baraja = iz ++ tail dr
     where (iz,dr) = span (/= carta) baraja

barajar :: Baraja -> Baraja -> IO Baraja
barajar barajaAleatoria [] = return barajaAleatoria
barajar barajaAleatoria barajaSin = do
     numeroAleatorio <- randomRIO (0, length barajaSin -1)
     let cartaAleatoria = barajaSin !! numeroAleatorio
         nuevaBaraja = eliminarCarta cartaAleatoria barajaSin
         nuevaAleatoria = barajaAleatoria ++ [cartaAleatoria]
     barajar nuevaAleatoria nuevaBaraja  

barajaJuego :: IO Baraja
barajaJuego = barajar [] crearBaraja


-- Valorar la mano

data Puntuacion x =  Valor Int | Blackjack | Pasado 
     deriving (Show, Ord, Eq)

puntosCarta :: Carta -> [Int]
puntosCarta (As, _) = [1,11]
puntosCarta (Dos, _) = [2]
puntosCarta (Tres, _) = [3]
puntosCarta (Cuatro, _) = [4]
puntosCarta (Cinco, _) = [5]
puntosCarta (Seis, _) = [6]
puntosCarta (Siete, _) = [7]
puntosCarta (Ocho, _) = [8]
puntosCarta (Nueve, _) = [9]
puntosCarta (_, _) = [10]

puntosPosibles :: Mano -> [Int] -> [Int]
puntosPosibles [] acumulador = acumulador
puntosPosibles (carta:resto) acumulador = puntosPosibles resto (nuevoAcumulador (puntosCarta carta) acumulador)
     where nuevoAcumulador xs ys = [x + y | x <- xs, y <- ys]

puntuaciones21 :: Mano -> [Int]
puntuaciones21 mano = filter (<=21) (puntosPosibles mano [0])

puedeBlackjack :: Mano -> Bool
puedeBlackjack mano = length mano == 2

esDiez :: Carta -> Bool
esDiez x = puntosCarta(x) == [10]

esAs :: Carta -> Bool
esAs carta = elem carta [(As, Treboles), (As, Corazones), (As, Diamantes), (As, Picas)]

esBlackjack ::  Mano -> Bool
esBlackjack [carta1, carta2] = (esAs carta1 && esDiez carta2) || (esAs carta2 && esDiez carta1)

esPasado :: Mano -> Bool
esPasado mano = (puntuaciones21 mano) == []

puntosMano :: Mano -> Puntuacion Int
puntosMano mano
     | esPasado mano = Pasado
     | (puedeBlackjack mano) && (esBlackjack mano) = Blackjack
     | otherwise = Valor (last (sort (puntuaciones21 mano)))


-- Comparar manos del jugador y la banca

data Resultado = Ganar | Perder | Tablas | Black
     deriving (Show, Ord, Eq)

type Apuesta = Integer

compararPuntos :: Puntuacion Int -> Puntuacion Int -> Resultado
compararPuntos Pasado _ = Perder
compararPuntos Blackjack _ = Black
compararPuntos _ Pasado = Ganar
compararPuntos _ Blackjack = Perder
compararPuntos jugador banca
     | jugador < banca = Perder
     | jugador > banca = Ganar
     | otherwise = Tablas

compararMano :: Mano -> Mano -> Resultado
compararMano jugador banca = compararPuntos (puntosMano(jugador)) (puntosMano(banca))

premios :: Resultado -> Apuesta -> Apuesta
premios Ganar  apostado  = apostado
premios Perder apostado  = apostado * (-1)
premios Tablas apostado  = 0
premios Black  apostado  = fromIntegral (apostado) * 3


-- Movimientos y estrategia de la banca

data Movimiento = Pedir | Plantarse | Doblar
     deriving (Show, Ord, Eq)

rondaBanca :: Mano -> Mano -> Movimiento
rondaBanca jugador banca 
     | compararMano jugador banca == Perder = Plantarse
     | compararMano jugador banca == Black = Plantarse
     | (compararMano jugador banca == Tablas) && ((puntosMano banca) > Valor 16) = Plantarse
     | otherwise = Pedir

turnoJugador :: Movimiento -> Apuesta -> Mano -> Mano -> Baraja -> (Apuesta, Mano, Baraja)
turnoJugador movimiento apuesta mano banca baraja
     | movimiento == Pedir = (apuesta, mano1, baraja1)
     | movimiento == Doblar = (2*apuesta, mano1, baraja1)
     | otherwise = (apuesta, mano, baraja)
     where (mano1, baraja1) = darCartas 1 mano baraja

turnoBanca :: Movimiento -> Mano -> Baraja -> (Mano, Baraja)
turnoBanca movimiento banca baraja
     | movimiento == Pedir = (banca1, baraja1)
     | otherwise = (banca, baraja)
     where (banca1, baraja1) = darCartas 1 banca baraja

     
-- Cuerpo del juego

repetir :: Integer -> IO ()
repetir total = do
          putStr ("\nDesea jugar de nuevo (s/n): ")
          repe <- getLine
          if repe == "s"  
               then juego total
               else putStrLn ("\nHa finalizado su partida")	
        
salir :: IO ()        
salir = do 
     putStrLn ("\nSe ha arruinado")
     
final :: Integer -> IO ()     
final total
     | total > 0 = repetir total
     | otherwise = salir
     
juego :: Integer -> IO ()   
juego presupuesto = do

     -- Parte inicial: Introducimos valores, repartimos las cartas y las muestra

     putStr ("\nSu presupuesto actual es de: ")
     print presupuesto
     putStr ("\nCual va a ser su apuesta: ")
     apuesta <- getLine
     baraja <- barajaJuego
     let  (mano, baraja1)  = darCartas 2 [] baraja
          (banca, baraja2) = darCartas 2 [] baraja1
          (primeraBanca:resto) = banca
          puntos = puntosMano mano   
     putStr ("\nSus cartas son: ")
     print mano
     putStr ("Su mano hace un total de: ")
     print puntos
     putStr ("\nLa carta que muestra la banca es: ")
     print primeraBanca
     
     -- Parte Jugador: El jugador decide que es lo que quiere hacer y se ejecuta su accion

     putStr ("\nQue quiere hacer (Pedir = 1 | Doblar = 2 | Plantarse = 3): ")
     mov <- getLine  
     let  movimiento
               | mov == "1" = Pedir
               | mov == "2" = Doblar
               | otherwise = Plantarse
     putStr ("Usted ha decidido: ")
     print movimiento
     let (apuesta55, mano55, baraja55) = turnoJugador movimiento (read apuesta) mano banca baraja2
         puntosJugador = puntosMano mano55
     putStr ("Sus cartas tras finalizar su turno son: ")
     print mano55
     putStr ("Su mano final hace un total de: ")
     print puntosJugador

     -- Parte Banca: La banca ve la mano del jugador y actua en consecuencia

     let movimientoBanca = rondaBanca mano55 banca
     putStr ("\nLa mano de la banca es: ")
     print banca
     putStr ("La banca ha decidido: ")
     print movimientoBanca
     let (banca55, barajaFinal) = turnoBanca movimientoBanca banca baraja55
         puntosBanca = puntosMano banca55
     putStr ("Las cartas de la banca tras finalizar el turno son: ")
     print banca55
     putStr ("La mano de la banca hace un total de: ")
     print puntosBanca

     -- Parte resolucion: Compara las manos, muestra el resultado y el premio y lo devuelve

     let resultado = compararMano mano55 banca55
         premio = premios resultado apuesta55
     putStr ("\nEl resultado de esta partida es: ")
     print resultado
     putStr ("El premio recibido es de: ")
     print premio
     let total =  presupuesto + premio
     putStr ("Ahora tiene un total de: ")
     print total

     -- Parte final: Ve si estas arruinado y pregunta al jugador si desea inciar una nueva partida

     final total

          
-- Main

main :: IO ()  
main = do
     putStrLn ("\nBienvenido al BlackJack de Alvaro Rubio y Luismi Barbero")
     putStr ("\nCuanto presupuesto posee: ")
     presupuesto <- getLine
     juego (read presupuesto)
     putStrLn ("\nHa sido un placer jugar con usted")
	 
	 
      


     

module Library where
import PdePreludat

-- Lambda
cuadradoMasUno = (+1).(^2)
cuadradoMasUno' x = (x^2) + 1
-- (\x -> (x^2) + 1)
-- Mal: cuadradoMasUno'' = (\x -> (x^2) + 1)

listaDeCuadradosMasUno n = map cuadradoMasUno [1..n]
funcionLenta = listaDeCuadradosMasUno 10000000000432424
posibleFuncionLenta = head funcionLenta

-- funcion 1 2 3 4
-- funcion 1 2 3 4 = 1 + 2 + 3
-- 6

-- funcion 1 2 3 funcionLenta
funcion 1 2 3 funcionLenta = 1 + 2 + 3
-- 6

-- funcionBooleana 1 [2,3,4,60]
-- funcionBooleana (1 > 0) || any even (filter (>5) [2,3,4,60]) 
-- True

-- funcionBooleana (-1) [2,3,4,61, 80]
-- ((-1) > 0) || any even (filter (>5) [2,3,4,61, 80]) 
-- False || any even (filter (>5) [2,3,4,61,80]) 
-- any even (filter (>5) [2,3,4,61,80]) 
-- any even [61, cosas no necesariamente mayores a 5]
-- any even [61, 80]
-- True
funcionBooleana x y = (x > 0) || any even (filter (>5) y)  

-- otraFuncionBooleana (-1) [2,3,4,61, 80,5]
-- ((-1) > 0) || all even (filter (>5) [2,3,4,61, 80,5]) 
-- False || all even (filter (>5) [2,3,4,61,80,5]) 
-- all even (filter (>5) [2,3,4,61,80,5])
-- all even [61..cosas]
-- False

-- otraFuncionBooleana (-1) [2,3,4,60, 80,6]
-- ((-1) > 0) || all even (filter (>5) [2,3,4,60, 80,6]) 
-- False || all even (filter (>5) [2,3,4,60, 80,6]) 
-- all even (filter (>5) [2,3,4,60, 80,6])
-- all even [60..cosas]
-- all even [60, 80, .. cosas]
-- all even [60, 80, 6 .. cosas]
-- all even [60, 80, 6]
-- True

otraFuncionBooleana x y = (x > 0) || all even (filter (>5) y)  

-- [] | elemento : lista
--                [] | elemento : lista 
--                          [] | elemento : lista
-- ...
-- [2, 2, 2, 2, 2, 2, 2, 2, ...]
generarListaInfinita valor = valor : generarListaInfinita valor

-- map (+1) [1..]
-- any (>1) [1..]
-- all (>1) [1..]
-- filter (>1) [1..]

data Persona = UnaPersona Number deriving (Show, Eq)
data Carta = UnaCarta Number String deriving Show

palosPosibles = ["copas", "bastos", "oro", "espada"]
numerosDeCartas = [1..12]
obtenerCartasMayoresQue n = [ UnaCarta numero palo | numero <- numerosDeCartas, palo <- palosPosibles, numero > n]

posiblesParejas listaDePersonas = [ (persona1, persona2) | persona1 <- listaDePersonas, 
                                     persona2 <- listaDePersonas, 
                                     persona1 /= persona2]

ambosPares x y = even x && even y
ambosImpares x y = odd x && odd y

ambosParesOImpares conjunto1 conjunto2 = [(x,y) | x <- conjunto1, y <- conjunto2, ambosPares x y || ambosImpares x y]

unNombreDeFuncionCopado = (\x -> x^2 + 142342 - 249423)
sacarEspacios palabra = [ letra | letra <- palabra, letra /= ' ']

interseccion lista1 lista2 = [x | x <- lista1, elem x lista2]
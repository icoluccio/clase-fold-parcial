module Fold where
import PdePreludat

suma [] =  0
suma (x:xs) = x + suma xs

sumaIncorrecta [x] =  x
sumaIncorrecta (x:xs) = x + sumaIncorrecta xs

listaInfinita x = x:listaInfinita x

-- Fold
-- Primera versión
cuantosCaramelos (tiene, quiere) cuantosQuedan = min cuantosQuedan (quiere - tiene)

cuantosCaramelosQuedan cantidad [] = cantidad 
cuantosCaramelosQuedan cantidadDeCaramelos (persona: personas)
         | cantidadDeCaramelos > 0 = cuantosCaramelosQuedan (cantidadDeCaramelos - cuantosCaramelos persona cantidadDeCaramelos) personas
         | otherwise = 0

-- Haciendo unos pases mágicos...
sacarCaramelos cuantosQuedan persona = max (cuantosQuedan - cuantosCaramelos persona cuantosQuedan) 0

cuantosCaramelosQuedan' cantidad [] = cantidad 
cuantosCaramelosQuedan' cantidadDeCaramelos (persona: personas) = cuantosCaramelosQuedan' (sacarCaramelos cantidadDeCaramelos persona) personas

-- Si pensamos de forma genérica y lo transformamos en una función de orden superior...
foldear _ acumulador [] = acumulador
foldear funcion acumulador (x:xs) = foldear funcion (funcion acumulador x) xs

-- Pero en realidad esto es equivalente a:
cuantosCaramelosQuedan'' caramelos personas = foldl sacarCaramelos caramelos personas 
-- cuantosCaramelosQuedan'' = foldl sacarCaramelos
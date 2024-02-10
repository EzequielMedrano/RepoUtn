module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--clase del sabado 3/02

data Granuja = UnGranuja{
  nombre::String,
  fuerza::Number
}deriving Show

majinBoo= UnGranuja "Majin Boo" 100
gra1=UnGranuja "El caballero rojo" 70
gra2= UnGranuja "Momia" 10

cambiaFuerza :: Granuja -> Granuja -> Granuja
cambiaFuerza boo granuja = boo { fuerza = fuerza boo + ((length . nombre) granuja * 6) }

fusionarNombre :: Granuja -> Granuja -> Granuja
fusionarNombre boo granuja = boo { nombre = nombre boo ++ nombre granuja}

-- modificarBoo boo granuja = flip cambiaFuerza granuja . fusionarNombre boo granuja

modificarBoo boo granuja = (flip cambiaFuerza granuja . fusionarNombre boo )granuja --ESTE SI ANDA BIEN-ES DE LA PROFE
--{nombre = "Majin BooEl caballero rojoMomia", fuerza = 232}
-- modificarBoo boo granuja = ( cambiaFuerza granuja . fusionarNombre boo) granuja --ESTO ANDA MAL
-- cambiaFuerza majinBoo gra2 => UnGranuja {nombre = "Majin Boo", fuerza = 184} 50 de la fuerza de majin boo y 34 de
-- la fuerza de gra2

--fusionarNombre (boo granuja) esto si es un granuja 
-- fusionarNombre boo granuja -- esto no.
--VOY CON FOLDL
mataAEsosGranujas boo granujas = foldl modificarBoo boo granujas

--AHORA CON RECURSIVIDAD
matoAEsosGranujass boo [] = boo
matoAEsosGranujass boo (granuja:granujas) = matoAEsosGranujass ( modificarBoo boo granuja ) granujas



--DEFINIR UNA FUNCION ESMULTIPLODEALGUNO/2 , QUE DADO UN NUMERO Y UNA LISTA DE NUMEROS,DEVUELVE TRUE <=>
-- EL NUMERO ES MULTIPLO DE ALGUNO DE LOS NUMEROS DE LA LISTA

-- esMultiploDeAlguno valor lista = any(\list-> (mod valor list) == 0) lista 
--AHORA PRUEBO CON ORDEN SUPERIOR
esMultiploDeAlguno valor lista = any((== 0) . mod valor) lista 


------------------------
-- VISTO EN UNA CLASE DE YT

--FOLDR
hacerRight :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
hacerRight funcion valorBase [] =valorBase
hacerRight funcion valorBase (c:cola) = funcion c (hacerRight funcion valorBase cola) 

--FOLDL
hacerLeft :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
hacerLeft funcion valorBase [] = valorBase
hacerLeft funcion valorBase (c:cola) = hacerLeft funcion (funcion valorBase c)cola

menorDeUna lista = foldl min ( head lista ) lista


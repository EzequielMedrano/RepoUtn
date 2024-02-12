module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
data Postre = UnPostre{
  sabores :: [String],
  peso :: Number,
  temperatura :: Number
}deriving (Show,Eq)

calculoElPorcentajeDelPeso :: Number->Postre->Number
calculoElPorcentajeDelPeso valor postre = (valor * peso postre ) / 100

cambiaDeTemperaturaElPostre::Number->Postre->Postre
cambiaDeTemperaturaElPostre valor postre = postre{
  temperatura = temperatura postre + valor
}
cambiaElPesoDelPostre::Number->Postre->Postre
cambiaElPesoDelPostre valor postre = postre {
    peso = peso postre + calculoElPorcentajeDelPeso valor postre
}
agregaSabor::String->Hechizo
agregaSabor sabor postre = postre{
  sabores = sabores postre ++ [sabor]
}
obtengoLosPesosDeLosPostre postres = map peso (listaDePostresListos postres)

congelado postre = temperatura postre == 0
--PUNTO 1-A
bizcochoDeFruta = UnPostre ["Fruta"] 100 25

tartaDeMelaza = UnPostre ["Melaza"] 50 0


--- PUNTO 1-B
type Hechizo = Postre->Postre


incendio::Hechizo
incendio = cambiaDeTemperaturaElPostre 1 . cambiaElPesoDelPostre (-5)

immobulus::Hechizo
immobulus postre = postre {
  temperatura = 0
}

wingardiumLeviosa ::  Hechizo
wingardiumLeviosa postre = postre{
  peso = peso postre - calculoElPorcentajeDelPeso 10 postre,
  sabores = sabores postre ++ sabores (agregaSabor "concentrado" postre)
}

diffindo :: Number->Hechizo
diffindo valor = cambiaElPesoDelPostre (-valor)

riddikulus::String->Hechizo
riddikulus sabor = agregaSabor (reverse sabor)

avadaKedavra :: Hechizo
avadaKedavra postre = immobulus  postre{
  sabores = []--pierde todos sus sabores
}-- PROBAR SI ESTO ES LO MISMO QUE APLICARLO CON COMPOSICION DE FUNCIONES??

-- 1-c

condicionesParaUnPostreListo::Postre->Bool
condicionesParaUnPostreListo postre = peso postre > 0 && length (sabores postre) > 0 && (not . congelado) postre

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo = all (condicionesParaUnPostreListo.hechizo)  

hechizoDejaListoAUnPostre::Postre->Hechizo->Bool
hechizoDejaListoAUnPostre postre hechizo = condicionesParaUnPostreListo (hechizo postre)

--1-D
pesoPromedioDeLosPostresListos::[Postre]->Number
pesoPromedioDeLosPostresListos = sum . obtengoLosPesosDeLosPostre

listaDePostresListos::[Postre]->[Postre]
listaDePostresListos = filter condicionesParaUnPostreListo
-- 2-a
data Mago = UnMago{
  hechizosAprendidos :: [Hechizo],
  horrorcruxes :: Number
}deriving (Show,Eq)

----------------------
--VUELVO A INTENTAR EL 2-A

practicaUnHechizo::Hechizo->Postre->Mago->Mago
practicaUnHechizo hechizo postre mago = (agregaHechizoAprendido hechizo . sumahorrorcrux hechizo postre ) mago

agregaHechizoAprendido::Hechizo->Mago->Mago
agregaHechizoAprendido hechizo mago = mago {
  hechizosAprendidos = hechizo : hechizosAprendidos mago
}

sumahorrorcrux::Hechizo->Postre->Mago->Mago
sumahorrorcrux hechizo postre  mago
 | mismoResultadoQueAvadaKedavra hechizo postre = mago{
  horrorcruxes = horrorcruxes mago + 1
 }
 | otherwise = mago

mismoResultadoQueAvadaKedavra:: Hechizo->Postre->Bool
mismoResultadoQueAvadaKedavra hechizo postre  = hechizo postre == avadaKedavra postre



--2-B


mejorHechizoV1 :: Postre -> Mago -> Hechizo
mejorHechizoV1 postre mago = elMejor postre (hechizosAprendidos mago)

elMejor :: Postre -> [Hechizo] -> Hechizo
elMejor postre [hechizo] = hechizo
elMejor postre (primer:segundo:restohechizos) 
  |esMejor postre primer segundo = elMejor postre (primer:restohechizos)
  |otherwise = elMejor postre (segundo:restohechizos)

-- esMejor :: Postre -> Hechizo -> Hechizo -> Bool
-- esMejor postre hechizo1 hechizo2 = (length . sabores . hechizo1) postre > (length . sabores . hechizo2) postre
esMejor::Postre->Hechizo->Hechizo->Bool
esMejor postre hechizo1 hechizo2 = length (sabores  (hechizo1 postre)) > length (sabores  (hechizo2 postre))
--------

mejorhechizoV2 ::  Postre -> Mago -> Hechizo
mejorhechizoV2 postre mago = foldl (elMejorEntre postre) (head (hechizosAprendidos mago))(hechizosAprendidos mago)

elMejorEntre :: Postre -> Hechizo -> Hechizo -> Hechizo
elMejorEntre postre hechizo1 hechizo2 
    | esMejor postre hechizo1 hechizo2 = hechizo1
    | otherwise = hechizo2
--------------

--3
listaInfinitaDePostres::[Postre]
listaInfinitaDePostres = repeat bizcochoDeFruta

magoConInfinitosHechizos mago = mago{hechizosAprendidos = repeat avadaKedavra, horrorcruxes = 0 }





-- Punto 3B)
{-
Verdadero, existe la consulta:
Prelude> estanListos avadaKedabra mesaInfinita
La ejecución devuelve falso pues debido a la evaluación diferida, el all cuando encuentra el primer postre que no está listo ya retorna y no requiere construir la lista infinita.
-}


-- Punto 3C)
{-
No existe ninguna forma de conocer el mejor hechizo del mago porque para hacerlo hay que evaluar todos los elementos lista, aún teniendo lazy evaluation.
-}
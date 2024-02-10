module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


data Postre = UnPostre{
  sabores :: [String],
  peso :: Number,
  temperatura :: Number
}deriving (Show,Eq)

-- data Hechizo = UnHechizo{

-- }
--PUNTO 1-A
bizcochoDeFruta = UnPostre ["Fruta"] 100 25 

tartaDeMelaza = UnPostre ["Melaza"] 50 0


--- PUNTO 1-B
incendio::Hechizo
incendio postre = (cambiaDeTemperaturaElPostre 1 . cambiaElPesoDelPostre (-5)) postre 

cambiaDeTemperaturaElPostre::Number->Postre->Postre
cambiaDeTemperaturaElPostre valor postre = postre{
  temperatura = temperatura postre + valor
}
cambiaElPesoDelPostre::Number->Postre->Postre
cambiaElPesoDelPostre valor postre = postre {
    peso = peso postre + calculoElPorcentajeDelPeso 5 postre 
}

calculoElPorcentajeDelPeso :: Number->Postre->Number
calculoElPorcentajeDelPeso valor postre = (valor * peso postre ) / 100


type Hechizo = Postre->Postre


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
diffindo valor postre = cambiaElPesoDelPostre (-valor) postre

riddikulus::String->Hechizo
riddikulus sabor postre = agregaSabor sabor postre -- ver como se agregan invertido esto.

agregaSabor::String->Hechizo
agregaSabor sabor postre = postre{
  sabores = sabores postre ++ [sabor]
}

avadaKedavra :: Hechizo
avadaKedavra postre = immobulus  postre{
  sabores = []--pierde todos sus sabores
}
-- 1-c

condicionesParaUnPostreListo::Postre->Bool
condicionesParaUnPostreListo postre = peso postre > 0 && length (sabores postre) > 0 && temperatura postre > 0


hechizoDejaListoAUnPostre::Postre->Hechizo->Bool
hechizoDejaListoAUnPostre postre hechizo = condicionesParaUnPostreListo (hechizo postre)

--1-D


pesoPromedioDeLosPostresListos::[Postre]->Number
pesoPromedioDeLosPostresListos postres = sum  (map peso (listaDePostresListos postres))

listaDePostresListos::[Postre]->[Postre]
listaDePostresListos postres = filter condicionesParaUnPostreListo postres


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
  hechizosAprendidos = hechizosAprendidos mago ++ [hechizo]
}
sumahorrorcrux::Hechizo->Postre->Mago->Mago
sumahorrorcrux hechizo postre  mago 
 | mismoResultadoQueAvadaKedavra hechizo postre = mago{
  horrorcruxes = horrorcruxes mago + 1
 }
 | otherwise = mago

mismoResultadoQueAvadaKedavra:: Hechizo->Postre->Bool
mismoResultadoQueAvadaKedavra hechizo postre  = (hechizo postre) == (avadaKedavra postre)


--2-B
-- mejorHechizo::Postre->Mago->Hechizo
-- mejorHechizo postre mago = foldl1 (maximo postre) (hechizosAprendidos mago)
-- mejorHechizo::Postre->Mago->Hechizo
-- mejorHechizo postre mago = foldl (min 0 (usaElPostre postre)) (head (hechizosAprendidos mago)) (hechizosAprendidos mago)

usaElPostre :: Postre->Hechizo->Number
usaElPostre postre hechizo =  length (sabores (hechizo postre))

maximo postre hechizo = max 0 (usaElPostre postre hechizo)


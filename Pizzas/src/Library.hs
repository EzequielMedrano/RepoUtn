module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- data Pizza = Pizza{
--  ingredientes:: [String],
--  tamanio::Number,
--  calorias::Number
-- }deriving (Show,Eq)
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

--FALTA EL 2-A

-- magoAsisteALaClaseDeDefensa::Hechizo->Postre->Mago->Mago
-- magoAsisteALaClaseDeDefensa hechizo postre mago =  (magoPracticaHechizosobreUnPostre hechizo postre . sumaUnHorrorcrux) mago  --mago (hechizo postre)

-- magoPracticaHechizosobreUnPostre::Hechizo->Postre->Mago->Mago
-- magoPracticaHechizosobreUnPostre hechizo postre mago =  mago {
--   hechizosAprendidos = hechizosAprendidos mago ++ [hechizo]
-- }

-- aplicaHechizoSobreUnPostre::Hechizo->Postre->Postre-- como incluyo esto en el metodo de arriba.
-- aplicaHechizoSobreUnPostre hechizo postre = hechizo postre 

-- mismoEfectoQueAvadaKedavra::Hechizo->Postre->Bool
-- mismoEfectoQueAvadaKedavra hechizo postre = sabores( aplicaHechizoSobreUnPostre hechizo postre) == sabores (avadaKedavra hechizo)

-- sumaUnHorrorcrux::Hechizo->Postre->Mago->Mago
-- sumaUnHorrorcrux hechizo postre mago
--  | mismoEfectoQueAvadaKedavra hechizo postre = mago{
--   horrorcruxes = horrorcruxes mago + 1
--  }
--  | otherwise = mago 


--2B

obtengoMejorHechizoDelMago::Postre->Mago->Hechizo
obtengoMejorHechizoDelMago postre mago = head ( filter ( mejorHechizo postre  ) (hechizosAprendidos mago ))

mejorHechizo::Postre->Hechizo->Bool
mejorHechizo postre hechizo = length (sabores (hechizo postre)) > length (sabores postre )


-- 3






module Library where
import PdePreludat
import Data.List (nub)

doble :: Number -> Number
doble numero = numero + numero


data Material = UnMaterial{
    nombre :: String,
    calidad :: Number
} deriving(Show ,Eq)

madera = UnMaterial "madera" 25

data Edificio = UnEdificio{
    tipoEdificio :: String,
    materiales :: [Material]
} deriving (Show ,Eq)

data Aldea = UnaAldea{
    poblacion :: Number,
    materialesDisponibles :: [Material],
    edificios :: [Edificio]
} deriving (Show, Eq) 

--funciones Auxiliares

esUnMaterialConElMismoNombre  nombreDelMaterial material = nombreDelMaterial == nombre material

sumatoriaDeTodosLosMaterialesDisponibles :: [Material] -> Number
sumatoriaDeTodosLosMaterialesDisponibles materiales = sum  (map calidad materiales) -- == (sum . map calidad)  materiales 


obtengoListaDeMateriales :: Aldea -> [[Material]]
obtengoListaDeMateriales aldea = map materiales (edificios aldea)

---------1-----------

esValioso :: Material -> Bool
esValioso = (> 25).calidad

unidadesDisponibles :: String -> Aldea -> Number
unidadesDisponibles nombreDelMaterial aldea = length  (filter ((== nombreDelMaterial). nombre) (materialesDisponibles aldea))

valorTotal :: Aldea -> Number
valorTotal aldea = sumatoriaDeTodosLosMaterialesDisponibles (materialesDisponibles aldea) + sum (map sumatoriaDeTodosLosMaterialesDisponibles (obtengoListaDeMateriales aldea))
-- (sum . map sumatoriaDeTodosLosMaterialesDisponibles . obtengoListaDeMateriales $ aldea)               
--PUNTO INTERESANTE.
aldea1 = UnaAldea 100 [] []
--- PUNTO 2
type Tarea = Aldea -> Aldea

tenerGnomito :: Tarea
tenerGnomito aldea = aldea{
  poblacion = poblacion aldea + 1
}

ilustrarMaderas :: Tarea
ilustrarMaderas aldea = aldea{
  materialesDisponibles = aumentoLacalidadDeLosMateriales aldea
}
aumentoLacalidadDeLosMateriales :: Aldea -> [Material]
aumentoLacalidadDeLosMateriales aldea = map aumentoEnCalidad (materialesDisponibles aldea)

aumentoEnCalidad :: Material -> Material
aumentoEnCalidad material 
 | nombre material == "Madera" = material{
  calidad = calidad material + 5
 }
 | otherwise = material

recolectar :: Number -> Material -> Tarea
recolectar   cantidadARecolectar material  aldea = aldea{
  materialesDisponibles = materialesDisponibles aldea ++ replicate cantidadARecolectar material
}

------------ PUNTO 3

obtengoEdificiosChetos :: Aldea -> [Edificio]
obtengoEdificiosChetos aldea = filter edificioCheto (edificios aldea)


edificioCheto :: Edificio -> Bool
edificioCheto edificio = any esValioso (materiales edificio)


----- 3-b
materialesComunes :: Aldea -> [Material]
materialesComunes aldea = nub . unirTodasLasListas . map (esComun aldea) . listaDeMaterialesDeCadaEdificio $ aldea

esComun :: Aldea -> [Material] -> [[Material]]
esComun aldea primeraLista = map (  filtradoMaterialComun (listaDeMaterialesDeCadaEdificio aldea) ) $ primeraLista


quitarReptidos materiales = undefined

filtradoMaterialComun :: [[Material]] -> Material -> [Material]
filtradoMaterialComun listasDeMateriales material
    |  all (elem material) listasDeMateriales = [material]
    | otherwise = []

unirTodasLasListas :: [[[Material]]]-> [Material]
unirTodasLasListas listas = foldl1 (++) . foldl1 (++) $ listas

listaDeMaterialesDeCadaEdificio :: Aldea -> [[Material]]
listaDeMaterialesDeCadaEdificio aldea = map materiales . edificios $ aldea

--PUNTO 4
type Criterio = Aldea->Bool
realizarLasQueCumplen::[Tarea]->Criterio->Aldea->Aldea
realizarLasQueCumplen [] criterio aldea = aldea
realizarLasQueCumplen (tarea:tarea2:tareas) criterio aldea 
 |tareaValida tarea aldea criterio = realizarLasQueCumplen (tarea:tareas) criterio (tarea aldea)
 |otherwise = realizarLasQueCumplen (tarea2:tareas) criterio (tarea2 aldea)

realizoTarea :: Tarea->Aldea->Aldea
realizoTarea tarea aldea = tarea aldea


tareaValida :: Tarea->Aldea->Criterio->Bool
tareaValida tarea aldea criterio = (criterio. tarea) aldea
--------
realizarLasQueCumplen2 :: [Tarea]->Criterio->Aldea -> Aldea
realizarLasQueCumplen2 tareas criterio aldea = foldl (aplicarTarea criterio ) aldea (tareas) 

aplicarTarea :: Criterio -> Aldea -> Tarea -> Aldea
aplicarTarea criterio aldea tarea
  --  | criterio . tarea $ aldea =  tarea aldea
    | criterio ( tarea aldea )= tarea aldea
    | otherwise = aldea
------PUNTO 4-B
tareas :: [Tarea]
tareas = [tenerGnomito,tenerGnomito,tenerGnomito]
masComidaQuePoblacion aldea tarea = unidadesDisponibles "Comida" aldea > poblacion (tenerGnomito aldea)

--4-b
maderaDePino = replicate 30 (UnMaterial "maderaDePino" (calidadMaxima aldea1) )


calidadMaxima :: Aldea -> Number
calidadMaxima aldea =  foldl1 max (map calidad ((esUnMaterialValioso . materialesDisponibles) aldea))

esUnMaterialValioso materiales = filter(esValioso) materiales
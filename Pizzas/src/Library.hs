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

aumentoLaclidadDeLosMateriales :: Aldea -> [Material]
aumentoLaclidadDeLosMateriales aldea = map aumentoEnCalidad (unidadesDisponibles "Madera" aldea)


aumentoEnCalidad :: Material -> Material
aumentoEnCalidad material = material {
  calidad = calidad material + 5
}
obtengoListaDeMateriales :: Aldea -> [[Material]]
obtengoListaDeMateriales aldea = map materiales (edificios aldea)

---------1-----------

esValioso :: Material -> Bool
esValioso = (> 25).calidad

unidadesDisponibles :: String -> Aldea -> [Material]
unidadesDisponibles nombreDelMaterial aldea = filter ((== nombreDelMaterial). nombre) (materialesDisponibles aldea)

valorTotal :: Aldea -> Number
valorTotal aldea = sumatoriaDeTodosLosMaterialesDisponibles (materialesDisponibles aldea) + (sum . map sumatoriaDeTodosLosMaterialesDisponibles . obtengoListaDeMateriales $ aldea)
--PUNTO INTERESANTE.

--- PUNTO 2
type Tarea = Aldea -> Aldea

tenerGnomito :: Tarea
tenerGnomito aldea = aldea{
  poblacion = poblacion aldea + 1
}

ilustrarMaderas :: Tarea
ilustrarMaderas aldea = aldea{
  materialesDisponibles = aumentoLaclidadDeLosMateriales aldea
}

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
------PUNTO 4-B


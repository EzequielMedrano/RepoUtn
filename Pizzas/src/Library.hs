module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Animal = UnAnimal{
  nombre::String,
  tipo::String,
  peso::Number,
  edad::Number,
  estaEnfermo::Bool,
  diagnosticoMed::[DiagnosticoMedico] -- lo cual puede requerir una visita medica de alguna veterinaria
  -- que diagnostica a los dias de recuperacion y le cobra un costo por la atencion
}deriving (Show)

data DiagnosticoMedico = UnMedico{
  diasDeRec :: Number,
  monto :: Number
}deriving Show


--FUNCIONES AUXILIARES
aumentaPeso animal alimentoBalanceado = animal {
  peso = peso animal + ajusteDePeso alimentoBalanceado
}
ajusteDePeso alimentoBalanceado
 | alimentoBalanceado <= 5 = alimentoBalanceado / 2
 | otherwise = 5

-- registraUnaVisita :: Number -> Number -> DiagnosticoMedico
-- registraUnaVisita diasDeRecuperacion costo = UnMedico{monto = costo ,diasDeRec = diasDeRecuperacion}

registraUnaVisita :: Number -> Number -> Actividad
registraUnaVisita diasDeRecuperacion costo animal = animal{
  diagnosticoMed = UnMedico {monto = costo ,diasDeRec = diasDeRecuperacion} : diagnosticoMed animal 
} 
estaBienDePeso animal pesoNormal = peso animal > pesoNormal

diagnostico :: DiagnosticoMedico
diagnostico = UnMedico 35 100
 --------------------------
--PUNTO 1
laPasoMal :: Animal -> Bool
laPasoMal animal = any ((>30).diasDeRec) (diagnosticoMed animal)

nombreFalopa :: Animal -> Bool
nombreFalopa animal = last (nombre animal) == 'i'

--PUNTO 2
type Actividad = Animal->Animal

engorde :: Number -> Actividad
engorde alimentoBalanceado animal = aumentaPeso animal alimentoBalanceado

revisacion :: Number->Number->Actividad
revisacion dias costo actividad 
 | estaEnfermo actividad = (engorde 2 . registraUnaVisita costo dias ) actividad
 | otherwise = actividad

festejoCumple :: Actividad
festejoCumple animal = animal{ edad = edad animal + 1}


-- chequeDePeso :: Number -> Number -> Number -> Actividad
-- chequeDePeso  pesoX diasDeRecuperacion costo animal
--  | estaBienDePeso animal pesoX = animal
--  | otherwise = revisacion diasDeRecuperacion costo animal -- entendi Mal EL ENUNCIADO

chequeoDePeso :: Number -> Actividad
chequeoDePeso unPeso animal = animal { estaEnfermo = peso animal <= unPeso}

-- PUNTO 3



proceso :: [Actividad]
proceso = [festejoCumple , engorde 10 , revisacion 10 5 , chequeoDePeso 5]--OJO ,ANTE UNA DUDA DEL ENUNCIADO, PREGUNTAR.
animal1 = UnAnimal 

elAnimalRecorreElProceso animal = foldr ($) animal proceso-- ME FALTO ESTO-- ESTO HACE QUE EL ANIMAL RECORRA LA LISTA DE ACTIVIDADES.

--PUNTO 4

mejoraONoMejora :: [Actividad] -> Animal -> Bool
mejoraONoMejora [] animal = False
mejoraONoMejora (actv1:actividades) animal = mejoraElPeso animal actv1 (head actividades) -- no creo que este bien esto


mejoraElPeso::Animal->Actividad->Actividad->Bool
mejoraElPeso animal actividad1 actividad2 = peso (actividad1 animal) >= peso (actividad2 animal) && noSubeMasDe3Kilos animal actividad1

noSubeMasDe3Kilos::Animal->Actividad->Bool
noSubeMasDe3Kilos animal actividad1  = peso ( actividad1 animal ) <= 3


-- mejorhechizoV2 ::  Postre -> Mago -> Hechizo
-- mejorhechizoV2 postre mago = foldl1 (elMejorEntre postre) (hechizos mago)

-- elMejorEntre :: Postre -> Hechizo -> Hechizo -> Hechizo
-- elMejorEntre postre hechizo1 hechizo2 
--     | esMejor postre hechizo1 hechizo2 = hechizo1
--     | otherwise = hechizo2

-- esMejor :: Postre -> Hechizo -> Hechizo -> Bool
-- esMejor postre hechizo1 hechizo2 = (length . sabores . hechizo1) postre > (length . sabores . hechizo2) postre


--PUNTO 5

primerosNombresFalopas = take 3 . filter nombreFalopa

animalesInfinitos = repeat animal1

-- Si seria posible poder obtener un valor computable , ya que al encontrar 3  animales con nombre falopa, entonces 
-- por evaluacion diferida no generaria la lista infinita.
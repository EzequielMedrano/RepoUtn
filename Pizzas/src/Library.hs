module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Animal = UnAnimal{
  nombre::String,
  tipo::String,
  peso::Number,
  edad::Number,
  estaEnfermo::DiagnosticoMedico -- lo cual puede requerir una visita medica de alguna veterinaria
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

registraUnaVisita diasDeRecuperacion costo = UnMedico{monto = costo ,diasDeRec = diasDeRecuperacion}

estaBienDePeso animal pesoNormal = peso animal > pesoNormal
 --------------------------


laPasoMal medico = diasDeRec medico > 30
nombreFalopa animal = last (nombre animal) == 'i'

--PUNTO 2

type Actividad = Animal->Animal
engorde :: Number -> Actividad
engorde alimentoBalanceado animal = aumentaPeso animal alimentoBalanceado

-- revisacion diasDeRecuperacion costo animal medico  
--  | diasDeRec medico > 0 = (registraUnaVisita diasDeRecuperacion costo  . engorde 2) animal 
--  | otherwise = animal

festejoCumple :: Actividad
festejoCumple animal = animal{ edad = edad animal + 1}

animal1 = UnAnimal "lola" "vaca" 100 2 diagnostico
diagnostico = UnMedico 35 100

-- chequeDePeso :: Animal -> Number -> Number -> Number -> Animal
-- chequeDePeso animal pesoNormal diasDeRecuperacion costo 
--  | estaBienDePeso animal pesoNormal = animal
--  | otherwise = registraUnaVisita diasDeRecuperacion costo (estaEnfermo animal)

-- PUNTO 3, no entiendo bien que pidio 

proceso animal = festejoCumple . engorde 10

--festejoCumple animal1 : UnAnimal {nombre = "lola", tipo = "vaca", peso = 100, edad = 3, estaEnfermo = UnMedico {diasDeRec = 35, monto = 100}}
--engorde 10 animal1 : UnAnimal {nombre = "lola", tipo = "vaca", peso = 105, edad = 2, estaEnfermo = UnMedico {diasDeRec = 35, monto = 100}}
-- hay que testear las otras dos

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
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Elemento = UnElemento {
  tipo::String,
  ataque ::Ataque,
  defensa::Defensa
}deriving Show


type Ataque = Personaje->Personaje
type Defensa = Personaje->Personaje


data Personaje = UnPersonaje{
  nombre::String,
  salud::Number,
  elementos::[Elemento],
  anioPresente::Number
} deriving Show

--FUNCIONES AUXILIARES
bajaLaSalud  valor personaje  = personaje {salud = max 0 (salud personaje - valor)}

saludIgualACero :: Personaje -> Personaje -> Bool
saludIgualACero personaje enemigo = danioQueProduce personaje ((head . elementos) enemigo) == 0

personaje1 = UnPersonaje "Pepe" 100 [] 100 

esBirro elemento = elemento{
  tipo = "Maldad",
  ataque = causarDanio  1 
}
--PUNTO 1 

type Efecto = Number->Personaje -> Personaje

mandarAlAnio :: Efecto
mandarAlAnio  anio personaje = personaje { anioPresente= anio}

meditar :: Efecto
meditar valor personaje  = personaje{salud = salud personaje + valor/2}

causarDanio :: Efecto
causarDanio = bajaLaSalud

--PUNTO 2
esMalvado personaje = "Maldad" `elem`  map tipo (elementos personaje)

danioQueProduce::Personaje->Elemento->Number
danioQueProduce personaje elemento = salud personaje -  (salud . ataque elemento) personaje


enemigosMortales personaje enemigos = filter ( saludIgualACero personaje ) enemigos


--PUNTO 3
-- concentracion elemento nivelConcentracion = elemento{
--   defensa = head (meditarSegunNivelDeConcentracion nivelConcentracion),
--   tipo = "Magia"
-- }
meditarSegunNivelDeConcentracion nivelConcentracion = replicate nivelConcentracion meditar

esBirrosMalvados cantidad elemento= replicate cantidad (esBirro elemento)

jack elemento = UnPersonaje "jack" 300 [katanaMagica elemento] 200

katanaMagica elemento = elemento {
  tipo = "Magia",
  ataque = causarDanio 100
}

aku anio cantSalud elemento = UnPersonaje "aku" cantSalud  (esBirrosMalvados anio elemento) anio

elementoDeAku :: Number -> Elemento -> [Elemento]
elementoDeAku anio elemento =  esBirrosMalvados anio elemento
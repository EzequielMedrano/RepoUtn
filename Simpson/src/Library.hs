module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Personajes = Personajes{
  nombre :: String,
  felicidad :: Number,
  dinero :: Number
}deriving Show

type Actividades = Personajes->Personajes

escuelaElemental::Actividades
escuelaElemental personaje 
 | nombre personaje /= "Lisa" = cambioDeFelicidad 20 personaje
 | otherwise = cambioDeFelicidad (-20) personaje 

cambioDeFelicidad :: Number->Personajes->Personajes
cambioDeFelicidad nuevoNivel personaje = personaje {
   felicidad = max 0 (felicidad personaje - nuevoNivel)
}

comeDonas :: Number->Actividades
comeDonas cantidad personaje = (cambioDeFelicidad (cantidad * 10) . modificoPlata ( cantidad*10) )  personaje

modificoPlata :: Number->Personajes->Personajes
modificoPlata plata personaje =  personaje { dinero = dinero personaje - plata }

irAtrabajar :: String->Actividades
irAtrabajar nombreDelTrabajo personaje = modificoPlata (-(length nombreDelTrabajo)) personaje

trabajarComoDirector :: Actividades 
trabajarComoDirector personaje = ( irAtrabajar "escuela Elemental" . escuelaElemental) personaje -- (escuelaElemental . irAtrabajar "escuela Elemental") personaje


homero = Personajes "Homero Simpson" 50 100
skinner = Personajes "Skinner" 10 500
lisa = Personajes "Lisa Simpson" 100 0
burns = Personajes "Burns" 0 100000

{-
    Invocacion: ghci> serDirector skinner 

    Resultado: ghci> UnPersonaje {nombre = "Skinner", felicidad = 0, dinero = 517}
-}
type Logro = Personajes->Bool

serMillonario :: Logro
serMillonario personaje = dinero personaje > dinero burns

alegrarse :: Number -> Logro
alegrarse nivelDeFelicidad personaje = ((>nivelDeFelicidad).felicidad) personaje  --felicidad personaje > nivelDeFelicidad

programaKrosti :: Logro
programaKrosti personaje = dinero personaje > 10

random :: Logro
random personaje = dinero personaje < 10

-- orden superior repasar 
unaActividadResultaDecisivaParaLograrUnLogro :: Actividades -> Logro -> Personajes -> Bool
unaActividadResultaDecisivaParaLograrUnLogro actividad logro personaje
 | logro personaje = False
 | otherwise = ( logro . actividad ) personaje 

puntoB :: Personajes -> Logro -> [Actividades]->Personajes
puntoB personaje logro [] = personaje
puntoB personaje logro (actividad : actividades) 
 | unaActividadResultaDecisivaParaLograrUnLogro actividad logro personaje = actividad personaje  -- no uso el ":" porque solo quiero el primer
 -- elemento que cumpla.
 | otherwise = puntoB personaje logro actividades 
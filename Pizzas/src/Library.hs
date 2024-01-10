module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)



-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)
data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

type Palo = Habilidad->Tiro


putter :: Palo
putter habilidad = UnTiro{
velocidad = 10,
precision = precisionJugador habilidad * 2,
altura = 0
}
madera :: Palo
madera habilidad = UnTiro{
  velocidad = 100,
  altura = 5,
  precision = precisionJugador habilidad / 2
}

hierro :: Number->Palo
hierro valor habilidad = UnTiro{
  velocidad = fuerzaJugador  habilidad * valor,
  precision = precisionJugador habilidad / valor,
  altura =  max (valor -3) 0
}

palos::[Palo]
palos = [putter ,madera ] ++ map hierro [1..10]

--- PUNTO 2

golpe :: Jugador ->Palo->Tiro
golpe persona palo = (palo . habilidad) persona


---PUNTO 3

-- OBSTACULOS

-- type Obstaculo = Tiro->Bool

data Obstaculo =  Obstaculo{
puedeSuperar :: Tiro->Bool,
efectoLuegoDeSuperar ::Tiro->Tiro
}

intentaSuperarObstaculo::Obstaculo->Tiro->Tiro
intentaSuperarObstaculo obstaculo tiro 
 | puedeSuperar obstaculo tiro = efectoLuegoDeSuperar obstaculo tiro
 | otherwise = noSuperaElObstaculo

tunelConRampita::Tiro->Bool
tunelConRampita tiro = precision tiro > 90   

laguna::Tiro->Bool
laguna tiro =  velocidad tiro > 80 && altura tiro `elem` [1..5]

hoyo::Tiro->Bool
hoyo tiro  = precision tiro > 95

--Si supera los obstaculos
superaTunelConRampita::Tiro->Tiro
superaTunelConRampita tiro
 | tunelConRampita tiro = UnTiro{
  velocidad = velocidad tiro * 2,
  precision = 100,
  altura = 0
 }
 | otherwise = noSuperaElObstaculo


type Largo = Number
superalaguna::Largo->Tiro->Tiro
superalaguna  largo tiro
 | laguna tiro = tiro{
  altura = altura tiro  / largo
 }
 | otherwise = noSuperaElObstaculo


noSuperaElObstaculo :: Tiro
noSuperaElObstaculo = UnTiro{
   velocidad = 0,
    precision = 0,
    altura = 0
  }

superaHoyo :: Tiro->Tiro
superaHoyo  tiro 
 | hoyo tiro = valoresDelTiroEnCero
 | otherwise  = noSuperaElObstaculo

valoresDelTiroEnCero = noSuperaElObstaculo

-- palosUtiles::Jugador->Obstaculo->[Palo]
-- palosUtiles jugador obstactulo = filter ( habilidad jugador  ) palos










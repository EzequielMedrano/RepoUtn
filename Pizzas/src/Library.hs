module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Participante = UnParticipante {
  nombre :: String,
  edad ::  Number,
  nivelDeAtractivo :: Number,
  nivelDePersonalidad :: Number,
  nivelDeInteligencia :: Number,-- todos numeros decimales en un principio
  criterioDeVoto :: CriterioDeVoto
}deriving (Show)


-- Las pruebas semanales son eventos que tienen un criterio para ser
-- superadas, pero también tienen un índice de éxito (decimal) que es un
-- número entre 0 y 100 y determina qué tan bien se supera la prueba. Si la
-- prueba no se supera, el índice de éxito es 0.
------PRUEBAS
type Prueba = Participante->Number

baileDeTikTok::Prueba
baileDeTikTok  persona 
 | requierePersonalidad 20 persona = indiceDeExitoTikTok persona
 | otherwise = 0

botonRojo ::  Prueba
botonRojo  persona 
 | requierePersonalidad 10 persona && requiereInteligencia 20 persona = indiceDeExitoBotonRojo persona--aplicar composicion
 | otherwise = 0

cuentasRapidas::Prueba
cuentasRapidas  persona 
  | requiereInteligencia 40 persona = indiceDeExitoDeCuentasRapidas persona
  | otherwise = 0

type Indice = Participante->Number

indiceDeExitoTikTok::Indice
indiceDeExitoTikTok  persona =  nivelDePersonalidad persona + (nivelDeAtractivo persona * 2 )

indiceDeExitoBotonRojo::Indice
indiceDeExitoBotonRojo persona = 100

indiceDeExitoDeCuentasRapidas ::Indice
indiceDeExitoDeCuentasRapidas persona = nivelDeInteligencia persona + nivelDePersonalidad persona - nivelDeAtractivo persona

requierePersonalidad::Number->Participante->Bool
requierePersonalidad valor persona = valor >= nivelDePersonalidad persona

-- requiereInteligencia::Number->Participante->Bool
-- requiereInteligencia valor persona = valor >= nivelDeInteligencia persona
requiereInteligencia::Number->Participante->Bool
requiereInteligencia valor persona = valor >= nivelDeInteligencia persona

-- PUNTO 2-a

quienesSuperanLaPruebas::Prueba->[Participante]->[Participante]
quienesSuperanLaPruebas  prueba personas = filter (superaUnaPrueba prueba) personas

superaUnaPrueba::Prueba->Participante->Bool
superaUnaPrueba prueba persona = prueba persona > 0

--Punto 2-b
-- promedioDelIndiceDeExito :: [Participante]->Prueba->Number
-- promedioDelIndiceDeExito participantes prueba =   / length participantes

-- sumatoriaDeIndicesDeExito :: [Number]->Number
-- sumatoriaDeIndicesDeExito valores = sum valores

--2-c
participanteFavorito::Participante->[Prueba]->Bool
participanteFavorito participante pruebas = all(\prueba->superaUnaPruebaConUnIndiceMayorACincuenta participante prueba ) pruebas

superaUnaPruebaConUnIndiceMayorACincuenta::Participante->Prueba->Bool
superaUnaPruebaConUnIndiceMayorACincuenta participante prueba = prueba participante > 50

--3
type CriterioDeVoto = Participante -> [Number] -> Number

menosInteligente :: CriterioDeVoto
menosInteligente personas = foldl min ( nivelDeInteligencia personas)

masAtractivo :: CriterioDeVoto
masAtractivo personas = foldl max (nivelDeAtractivo personas)

masViejo :: CriterioDeVoto
masViejo personas = foldl max (edad personas)

--4
javierTulei = UnParticipante "Javier Tulei" 52 30 70 35 menosInteligente

minimoKirchner = UnParticipante "Minimo Kirchner" 46 0 40 50 masAtractivo

horacioBerreta = UnParticipante "Horacio Berreta" 57 10 60 50 masAtractivo

myriamBregwoman = UnParticipante "Myriam Bregwoman" 51 40 40 60 masViejo

--5
-- Luego de votar, nos interesa saber quiénes están en placa, esos son
-- todos los participantes que, al menos, una persona votó.
-- participantesEnPlaca participantes 




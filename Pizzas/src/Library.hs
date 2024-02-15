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

---FUNCIONES AUXILIARES
type Indice = Participante->Number

indiceDeExitoTikTok::Indice
indiceDeExitoTikTok  persona = ((*2).nivelDeAtractivo) persona + nivelDePersonalidad persona


indiceDeExitoBotonRojo::Indice
indiceDeExitoBotonRojo persona = 100

indiceDeExitoDeCuentasRapidas ::Indice
indiceDeExitoDeCuentasRapidas persona = nivelDeInteligencia persona + nivelDePersonalidad persona - nivelDeAtractivo persona


requierePersonalidad::Number->Participante->Bool
requierePersonalidad valor = (>= valor) . nivelDePersonalidad

requiereInteligencia::Number->Participante->Bool
requiereInteligencia valor = (>= valor) . nivelDeInteligencia

nuevo1 = UnParticipante "nuevo1" 52 30 70 35 menosInteligente

superaUnaPruebaConUnIndiceMayorACincuenta::Participante->Prueba->Bool
superaUnaPruebaConUnIndiceMayorACincuenta participante prueba = prueba participante > 50

personaMasAtractiva persona1 persona2 
 | nivelDeAtractivo persona1 > nivelDeAtractivo persona2 = persona1
 | otherwise = persona2

personaConMasEdad persona1 persona2
 | edad persona1 > edad persona2 = persona1
 | otherwise = persona2
  
-------------PUNTO1 PRUEBAS

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

-------------- PUNTO 2

quienesSuperanLaPruebas::Prueba->[Participante]->[Participante]
quienesSuperanLaPruebas prueba = filter ((> 0).prueba)

-- 2-b ESTE PUNTO FUE MUY COMPLICADO, LO COPIE , ES MUY INTERESANTE
promedioDeIndiceDeExito :: [Participante] -> Prueba -> Number
promedioDeIndiceDeExito participante prueba = calculoPromedio (map prueba  (quienesSuperanLaPruebas prueba participante))

calculoPromedio :: [Number]-> Number
calculoPromedio listaDeIndice = (sum listaDeIndice) / (length listaDeIndice)

--2-c
participanteFavorito::Participante->[Prueba]->Bool
participanteFavorito participante = all (superaUnaPruebaConUnIndiceMayorACincuenta participante )

--3
-- type CriterioDeVoto = Participante -> [Number] -> Number
type CriterioDeVoto = [Participante] -> Participante


menosInteligente :: CriterioDeVoto
menosInteligente listaParticipantes = foldl1 personaMenosInteligente listaParticipantes
                where
                    personaMenosInteligente participante1 participante2
                        | nivelDeInteligencia participante1 < nivelDeInteligencia participante2 = participante1
                        | otherwise = participante2

masAtractivo :: CriterioDeVoto
masAtractivo personas = foldl1 personaMasAtractiva personas -- es INTERESANTE COMO PUEDO PASARLE UNA LISTA Y 
-- ADENTRO DE LA FUNCION , LA SEPARA EN DOS PERSONAS Y ESO ME AYUDA A COMPARAR PERSONAS

masViejo :: CriterioDeVoto
masViejo personas = foldl1 personaConMasEdad personas

--4


javierTulei = UnParticipante "Javier Tulei" 52 30 70 35 menosInteligente

minimoKirchner = UnParticipante "Minimo Kirchner" 46 0 40 50 masAtractivo

horacioBerreta = UnParticipante "Horacio Berreta" 57 10 60 50 masAtractivo

myriamBregwoman = UnParticipante "Myriam Bregwoman" 51 40 40 60 masViejo

listaDePersonas :: [Participante]
listaDePersonas = [javierTulei,minimoKirchner,horacioBerreta,myriamBregwoman]

--5
-- Luego de votar, nos interesa saber quiénes están en placa, esos son
-- todos los participantes que, al menos, una persona votó.
-- participantesEnPlaca participantes 


-- osea si una persona te voto , ya estas en placa
personasEnPlaca participantes = foldl1 (fueVotado) participantes


fueVotado :: [Participante]->Participante->Participante
fueVotado participantes persona = (criterioDeVoto persona)  participantes   


-- 6 
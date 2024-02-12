module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Persona = UnaPersona{
  nick::String,
  felicidad::Number,
  librosQueAdquirio::[Libro],
  librosLeidos::[Libro]
}deriving (Eq,Show) 

data Libro = UnLibro{
  titulo :: String,
  loEscribio:: Persona,
  cantPaginas :: Number,
  efecto:: Efecto -- cómo afecta el género a las personas que lo lean.
}deriving (Eq, Show)

aumentaFelicidad ::Number->Persona->Persona
aumentaFelicidad valor persona = persona{
  felicidad = felicidad persona + valor
}
adquiereLibro::Libro->Persona->Persona
adquiereLibro libro persona = persona {
  librosLeidos = librosLeidos persona ++ [libro]
}
condicionParaQueSeConsidereLeido::Persona->Libro->Libro->Bool
condicionParaQueSeConsidereLeido persona libro1 libro2  = titulo libro1 == titulo libro2 && (loEscribio libro1) == persona

romero :: Persona
romero = UnaPersona "romero" 100 [patoruzito] [patoruzito]

patoruzito = UnLibro "patoruzito" romero 100 libroCienciaFiccion
type Efecto = Persona->Persona

-- libroComedia::String->Efecto
-- libroComedia tipoDeComedia persona 
--  | tipoDeComedia == "dramáticas" = persona
--  | tipoDeComedia == "absurdas" = aumentaFelicidad 5 persona 
--  | tipoDeComedia == "satíricas" = aumentaFelicidad (felicidad persona) persona
--  | otherwise = aumentaFelicidad 10 persona
----EFECTOS

libroCienciaFiccion :: Efecto
libroCienciaFiccion = \p -> p { nick = reverse (nick p) }-- simulo el reverse

libroTerror::Efecto
libroTerror persona = persona{
  librosQueAdquirio = []
}

leeUnLibro::Libro->Efecto->Persona->Persona
leeUnLibro libro efecto persona = (adquiereLibro libro . efecto ) persona 


sePusoAlDiaConLosLibros::Libro->Persona->Bool
sePusoAlDiaConLosLibros libro persona = any (condicionParaQueSeConsidereLeido persona libro ) (librosLeidos persona)




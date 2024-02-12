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

condicionParaQueSeConsidereLeido::Persona->Libro->Libro->Bool
condicionParaQueSeConsidereLeido persona libro1 libro2  = titulo libro1 == titulo libro2 -- && loEscribio libro1 == persona

aumentaFelicidad ::Number->Persona->Persona
aumentaFelicidad valor persona = persona{
  felicidad = felicidad persona + valor
}
adquiereLibro::Libro->Persona->Persona
adquiereLibro libro persona = persona {
  librosLeidos = librosLeidos persona ++ [libro]
}
cantidadDePaginasMayorQue libro cantidad = cantPaginas libro > cantidad
cantidadDePaginasMenorQue libro cantidad = cantPaginas libro < cantidad


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

--PARTE C
leeUnLibro::Libro->Efecto->Persona->Persona
leeUnLibro libro efecto persona = (adquiereLibro libro . efecto ) persona


sePusoAlDiaConLosLibros::Libro->Persona->Bool
sePusoAlDiaConLosLibros libro persona = any (condicionParaQueSeConsidereLeido persona libro ) (librosLeidos persona)

fanaticoDelEscritor::Libro->Persona->Bool
fanaticoDelEscritor libro persona = all(\libroleido-> loEscribio libroleido == persona) (librosLeidos persona)

-- Una persona no puede ponerse al dia con una lista infinita de libros porque no hay un tope o algo que frene esa busqueda como podria
-- ser un take , Esto es posible debido a la evaluación perezosa.


--PARTE D

type TipoDeLibro = Libro -> Bool
cuento :: TipoDeLibro
cuento libro = cantidadDePaginasMenorQue libro 100

novelaCorta :: TipoDeLibro
novelaCorta libro = cantidadDePaginasMayorQue libro 100 && cantidadDePaginasMenorQue libro 200

novela :: TipoDeLibro
novela libro = cantidadDePaginasMayorQue libro 200

librosAdquiridosSegunElTipo :: Persona -> TipoDeLibro -> [Libro]
librosAdquiridosSegunElTipo persona tipoDeLibro = filter tipoDeLibro (librosQueAdquirio persona)


titulosDeLosLibrosAdquiridos::Persona->TipoDeLibro->[String]
titulosDeLosLibrosAdquiridos persona tipoDeLibro = map titulo (librosAdquiridosSegunElTipo persona tipoDeLibro)

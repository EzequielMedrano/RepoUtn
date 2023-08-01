module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Auto = Auto{
 color :: String,
 velocidad :: Number,
 distancia::Number
} deriving (Show,Eq)


type Carrera = Auto->Auto



distanciaEntreAutos::Auto->Auto->Number
distanciaEntreAutos autoAdelantado autoAtrasado = distancia autoAdelantado - distancia autoAtrasado

distanciaEnValorAbsoluto::Auto->Auto->Number->Bool
distanciaEnValorAbsoluto autoAdelantado autoAtrasado distanciaEsperada = distanciaEntreAutos autoAdelantado autoAtrasado > distanciaEsperada || distanciaEntreAutos autoAdelantado autoAtrasado < distanciaEsperada

estaCerca::Auto->Auto->Bool
estaCerca autoAdelantado autoAtrasado = autoAdelantado /= autoAtrasado && distanciaEnValorAbsoluto autoAdelantado autoAtrasado 10

vaTranquilo :: Auto->[Auto]->Carrera->Bool
vaTranquilo auto autos carrera = all ( \ restoDeAutos -> distancia (carrera auto) >distancia (carrera restoDeAutos) ) autos
-- como hago para probar vaTranquilo??? por consola
--vaTranquilo (cars3 [cars,cars2])-- quise probarlo asi
cars::Auto
cars = Auto "azul" 0 100

cars2::Auto
cars2 = Auto "azul" 0 300

cars3::Auto
cars3 = Auto "azul" 0 200


puestoDelAuto::Carrera->[Auto]->Auto->Number
puestoDelAuto carrera autos auto = cantidadDeAutosAdelantados carrera autos auto + 1

cantidadDeAutosAdelantados::Carrera->[Auto]->Auto->Number
cantidadDeAutosAdelantados carrera autos auto = length (filter (\restoDeAutos-> distancia (carrera auto) < distancia (carrera restoDeAutos) ) autos)

--PUNTO 2

corra::Number->Carrera->Auto->Auto
corra tiempoRecorrido carrera auto = auto {
    distancia = distanciaSegunElTiempoRecorrido tiempoRecorrido carrera auto
}

distanciaSegunElTiempoRecorrido :: Number->Carrera->Auto->Number
distanciaSegunElTiempoRecorrido tiempoRecorrido carrera auto = distancia (carrera auto ) + (tiempoRecorrido  * velocidad (carrera auto))

type Modificador = Number-> Number

alterarLaVelocidad :: Modificador->Auto->Auto
alterarLaVelocidad modificador  auto =  auto{
    velocidad = modificador (velocidad auto)  --lo hice por que me pide la velocidad final , osea la del auto una vez que esta terminando la carrera
}
-- alterarLaVelocidad :: Modificador->Auto->Auto
-- alterarLaVelocidad modificador  auto = auto{
--     velocidad = modificador (velocidad auto) --lo hice por que me pide la velocidad final , osea la del auto una vez que esta terminando la carrera
-- }
    --autoEnCarrera carrera auto{
  --  velocidad = modificador ( velocidad auto )
--} 

-- autoEnCarrera::Carrera->Auto->Auto -- lo hice por que me pide la velocidad final , osea la del auto una vez que esta terminando la carrera
-- autoEnCarrera carrera auto = carrera auto 


-- bajarLaVelocidad :: Number->Modificador->Auto->Auto
-- bajarLaVelocidad leQuitoVelocidad modificador auto = auto {
--     velocidad = max 0 (velocidad auto - velocidad (alterarLaVelocidad ( )auto))
-- }
bajarLaVelocidad :: Number->Auto->Auto
bajarLaVelocidad leQuitoVelocidad auto = auto {
    velocidad = max 0 (velocidad auto - leQuitoVelocidad)
}
-- -- afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
-- afectarALosQueCumplen funcionA funcionB lista = 
type Autos = [Auto]

terremoto::Auto->Autos->Autos
terremoto  auto autos = map (bajarLaVelocidad (50) ) (listaDeAutosQueEstanCerca auto autos)

listaDeAutosQueEstanCerca ::Auto->[Auto]->[Auto]
listaDeAutosQueEstanCerca auto autos = filter (estaCerca auto) autos


miguelitos::Number->Auto->Autos->Autos
miguelitos disminuyevelocidad auto  autos  = map (bajarLaVelocidad disminuyevelocidad) (autosAfectados auto autos) 

autosAfectados::Auto->Autos->Autos
autosAfectados auto = filter (\resto->distancia auto > distancia resto)

-- jetPack:: Number->Carrera->Auto->Auto
-- jetPack tiempo carrera auto 
--  | corra 




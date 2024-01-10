module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
 -- este parcial lo empece 18 y 10
data Turista =  Turista{
  cansancio :: Number,
  stress :: Number,
  viajaSolo:: Bool,
  idiomas :: [String]
} deriving Show

data Marea = Marea{
   fuerte:: Bool,
   moderada :: Bool, --,
   tranquila :: Bool
}deriving Show

type Paisaje = Turista->Turista
type Excursion = Turista->Turista

irAlaPlaya :: Paisaje
irAlaPlaya turista
 | viajaSolo turista = turista{
    cansancio = cansancio turista - 5
 }
 | otherwise = turista { stress = stress turista - 1}

apreciarAlgunElementoDelPaisaje :: String -> Excursion
apreciarAlgunElementoDelPaisaje paisaje turista = turista {
    stress = stress turista - length paisaje
}

salirAhablarUnIdiomaEspecifico :: String->Excursion
salirAhablarUnIdiomaEspecifico idioma turista = turista {
    viajaSolo = False,
   -- idiomas = idioma ++ idiomas turista  , esto si quiero concatenar dos listas
   idiomas = idioma : idiomas turista -- esto lo saque con chatgpt, ojo con las listas, aca fue importante
}
caminarCiertosMinutos:: Number->Excursion
caminarCiertosMinutos minutosCaminados turista = turista {
    cansancio = cansancio turista + (minutosCaminados/4) ,
    stress = stress turista - (minutosCaminados/4)
}

-- nivelDeIntensidad :: Number -> Number
-- nivelDeIntensidad minutosCaminados = minutosCaminados / 4

paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea turista
 | fuerte marea = turista {
    stress = stress turista + 6,
    cansancio = cansancio turista + 10
 }
 | moderada marea = turista
 | tranquila marea = (caminarCiertosMinutos 10  .  apreciarAlgunElementoDelPaisaje "Mar". salirAhablarUnIdiomaEspecifico "Aleman") turista -- esta no la vi ,
 -- la hice con chat gpt , composicion de funciones .
 -- cuando quiero que algo o alguien cumpla todas las funciones , uso aplicacion parcial 
 | otherwise =  turista

-- 1 )
ana = Turista 0 21 False ["Espanol"] -- 3 8 FALSE 
betto = Turista  15 15 True ["Aleman"]
cathi = Turista 15 15 True ["Aleman","Catalán"]

 -- 2) 
--a)
turistaHaceUnaExcursion :: Excursion -> Turista -> Turista
turistaHaceUnaExcursion excursion turista = excursion  turista {
    stress = stress turista - reducePorcentajeDelEstress 10 turista
}
reducePorcentajeDelEstress:: Number->Turista->Number
reducePorcentajeDelEstress valor turista = (stress turista * valor ) /100


--b)
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista->Number)-> Turista -> Excursion -> Number 
deltaExcursionSegun  indice turista excursion = deltaSegun indice  (turistaHaceUnaExcursion excursion turista) turista

 -- c 
-- laExcursionEsEducativa :: Excursion -> Turista ->Bool
-- laExcursionEsEducativa excursion turista = length ( idiomas (turistaHaceUnaExcursion excursion turista)) > 0
laExcursionEsEducativa :: Excursion -> Turista ->Bool
laExcursionEsEducativa excursion turista = length (idiomas turista) < length (idiomas (turistaHaceUnaExcursion excursion turista))

excursionesDesestresantes:: [Excursion]->Turista->[Excursion]
excursionesDesestresantes excursiones turista = filter ( flip estaBajaEstress turista) excursiones 
    -- filter ( flip estaBajaEstress turista ) excursiones 

estaBajaEstress :: Excursion->Turista->Bool
estaBajaEstress excursion turista  = stress (excursion turista) + 3 <= stress turista
--3)
type Tour = [Excursion]

completo :: Tour
completo = [caminarCiertosMinutos 20 , apreciarAlgunElementoDelPaisaje "cascada", caminarCiertosMinutos 40 ,irAlaPlaya ,salirAhablarUnIdiomaEspecifico "melmacquiano"]

--El operador $ se utiliza para aplicar cada función al turista. En este caso, foldr ($) se utiliza para
-- aplicar cada función de la lista excursiones al turista de manera sucesiva.




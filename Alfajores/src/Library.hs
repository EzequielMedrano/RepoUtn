module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Alfajor = Alfajor{
  relleno :: [Rellenos],
  peso :: Number,
  dulzor :: Number,
  nombre  :: String
} deriving Show

data Rellenos = Rellenos{
  nombreDelRelleno:: String,
  precio :: Number
}deriving (Show,Eq)

sumatoriaDeLosRellenos::[Rellenos]->Number
sumatoriaDeLosRellenos rellenos = (sum . map ( precio )) rellenos

dulceDeLeche = Rellenos "DulceDeLeche" 12

mousse = Rellenos "Mousse" 15

fruta = Rellenos "Fruta" 10

jorgito :: Alfajor
jorgito = Alfajor [dulceDeLeche] 80 8 "jorgito"

havanna :: Alfajor
havanna = Alfajor [mousse,mousse] 60 12 "havanna"

capitanDelEspacio :: Alfajor
capitanDelEspacio = Alfajor [fruta] 40 12 "capitanDelEspacio"


coeficienteDelDulzorDeUnAlfajor :: Alfajor->Number
coeficienteDelDulzorDeUnAlfajor alfajor = dulzor alfajor / peso alfajor

elPrecioDeUnAlfajor:: Alfajor->Number
elPrecioDeUnAlfajor alfajor = (2 * peso alfajor) + sumatoriaDeLosRellenos ( relleno alfajor)

alfajorPotable::Alfajor->Bool
alfajorPotable alfajor = verificaCapazDeRelleno (relleno alfajor) && (coeficienteDelDulzorDeUnAlfajor  alfajor) > 0.1
-- unAlfajorEsPotable :: Alfajor -> Bool
-- unAlfajorEsPotable alfajor = verCapasRelleno (capasDeRelleno alfajor) && cumpleConCoeficienteDeDulzorEstablecido 0.1 alfajor
verificaCapazDeRelleno :: [Rellenos]->Bool
verificaCapazDeRelleno [] = False
verificaCapazDeRelleno [relleno] = True
verificaCapazDeRelleno (relleno:rellenos)= all ( == relleno ) rellenos

abaratarUnAlfajor::Alfajor->Alfajor
abaratarUnAlfajor alfajor = (reducirElPesoDelAlfajor 10 . reducirElDulcorDelAlfajor 7 ) alfajor

reducirElPesoDelAlfajor:: Number->Alfajor->Alfajor
reducirElPesoDelAlfajor pesoareducir  alfajor = alfajor {
    peso = peso alfajor - pesoareducir
}
reducirElDulcorDelAlfajor:: Number->Alfajor->Alfajor
reducirElDulcorDelAlfajor pesoareducir alfajor = alfajor {
    dulzor = dulzor alfajor - pesoareducir
}

cambiarleElNombre:: String->Alfajor->Alfajor
cambiarleElNombre nombreNuevo  alfajor = alfajor {
    nombre = nombreNuevo
}

agregarleUnaCapaDeRelleno::Rellenos->Alfajor->Alfajor
agregarleUnaCapaDeRelleno rellenos alfajor = alfajor {
    relleno =  rellenos : relleno alfajor 
}

intentarhacerAlfajorPremium::Alfajor->Alfajor
intentarhacerAlfajorPremium alfajor 
 | alfajorPotable alfajor = hacerPremium alfajor
 | otherwise = alfajor

hacerPremium::Alfajor->Alfajor
hacerPremium alfajor = (agregarleUnaCapaDeRelleno (head (relleno alfajor)) . cambiarleElNombre (nombre alfajor ++ "premiun") )  alfajor 

ciertoGradoDePremium::Number-> Alfajor -> Alfajor
ciertoGradoDePremium 0 alfajor = alfajor 
ciertoGradoDePremium cantidadDeVecesPremium alfajor 
 | cantidadDeVecesPremium > 0 = ciertoGradoDePremium (cantidadDeVecesPremium - 1) (intentarhacerAlfajorPremium alfajor)
 |otherwise = alfajor

jorgitito :: Alfajor
jorgitito = (cambiarleElNombre "jorgitito" . abaratarUnAlfajor) jorgito

jorgelin:: Alfajor
jorgelin = (cambiarleElNombre "jorgelin" . agregarleUnaCapaDeRelleno dulceDeLeche ) jorgito




-- jorgitito = (renombrarAlfajor "Jorgitito" . abaratarUnAlfajor) jorgito

-- intentarHacerPremium :: Alfajor -> Alfajor
-- intentarHacerPremium alfajor
--     | unAlfajorEsPotable alfajor = hacerPremium (head (capasDeRelleno alfajor)) (nombreDelAlfajor alfajor ++ " premium") alfajor
--     | otherwise = alfajor

-- hacerPremium :: Relleno -> String -> Alfajor -> Alfajor
-- hacerPremium rellenoParaAgregar nombrePremium = agregarCapa rellenoParaAgregar . renombrarAlfajor nombrePremium

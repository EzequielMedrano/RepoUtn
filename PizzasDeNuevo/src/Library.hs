module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Pizza =  Pizza {
 ingredientes :: [String],
 tamanio :: Number,
 calorias::Number
}deriving (Show,Eq)

grandeDeMuzza :: Pizza
grandeDeMuzza = Pizza ["salsa","mozzarella","orégano"] 8 350

nivelDeSatisfaccionDeUnaPizza :: Pizza->Number
nivelDeSatisfaccionDeUnaPizza pizza
 | "palmito" `elem` ingredientes pizza = 0
 | calorias pizza >= 500 = nivelDeSatisfaccionSegunElNivelDeCalorias pizza
 | otherwise =  nivelDeSatisfaccionSegunElNivelDeCalorias pizza / 2

nivelDeSatisfaccionSegunElNivelDeCalorias:: Pizza->Number
nivelDeSatisfaccionSegunElNivelDeCalorias pizza =  length (ingredientes pizza) * 80

valorDeUnaPizza::Pizza->Number
valorDeUnaPizza pizza = (length ( ingredientes pizza) * 120) * tamanio pizza

nuevoIngrediente :: String->Pizza->Pizza
nuevoIngrediente ingrediente pizza = pizza {
    ingredientes = ingrediente : ingredientes pizza ,
    calorias = (length ingrediente * 2) + calorias pizza
}
agrandar::Pizza->Pizza
agrandar pizza 
 |tamanio pizza <= 8 = pizza {
    tamanio = tamanio pizza + 2
}
 |otherwise = pizza

mezcladita :: Pizza->Pizza->Pizza
mezcladita pizza1 pizza2 = mezcloLosIngredientesDeLasPizzas pizza1 pizza2


mezcloLosIngredientesDeLasPizzas :: Pizza -> Pizza -> Pizza
mezcloLosIngredientesDeLasPizzas pizza1 pizza2 = pizza2 { 
    ingredientes = ingredientes pizza1 ++ ingredientes pizza2 -- el ++ se usa para cuando quiero unir dos listas
    }

eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (filter (/= x) xs)


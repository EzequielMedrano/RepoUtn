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
mezcladita::Pizza->Pizza->Pizza
mezcladita primerPizza segundaPizza = (nuevaPizza primerPizza . subeCalorias primerPizza) segundaPizza

nuevaPizza::Pizza->Pizza->Pizza
nuevaPizza primerPizza segundaPizza = segundaPizza{
    ingredientes = sacarRepetidos ( ingredientes segundaPizza ++ ingredientes primerPizza)
}
sacarRepetidos::[String]->[String]
sacarRepetidos [] = []
sacarRepetidos (ingrediente:ingredientes)
 | elem ingrediente ingredientes = sacarRepetidos ingredientes
 | otherwise = ingrediente : sacarRepetidos ingredientes

subeCalorias::Pizza->Pizza->Pizza
subeCalorias primerPizza segundaPizza = segundaPizza {
    calorias = calorias segundaPizza + (calorias primerPizza / 2)
}

-- No duplicar lógica

nivelDeSatisfaccionDeUnPedido :: [Pizza]->Number
nivelDeSatisfaccionDeUnPedido pizzas = (sum.map nivelDeSatisfaccion) pizzas --sum(map nivelDeSatisfaccion pizzas)

pizzeriaLosHijosDePato::[Pizza]->[Pizza]
pizzeriaLosHijosDePato  pizzas = map pizzaConPalmito pizzas

pizzaConPalmito::Pizza->Pizza
pizzaConPalmito pizza = pizza{
ingredientes = "palmito" : ingredientes pizza
}

pizzeriaElResumen :: [Pizza] -> [Pizza]
pizzeriaElResumen pizzas = zipWith mezcladita pizzas (drop 1 pizzas)

muza :: Pizza
muza = Pizza ["jamon"] 0 0

queso :: Pizza
queso = Pizza ["queso"] 0 0

chedar :: Pizza
chedar = Pizza ["chedar"] 0 0

listaDePizzas ::[Pizza]
listaDePizzas = [muza ,queso , chedar]

pizzeriaEspecial::Pizza->Pizzeria
pizzeriaEspecial saborEspecial pizzas = map (mezcladita saborEspecial ) pizzas
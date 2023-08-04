module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Pizza = Pizza{
 ingredientes:: [String],
 tamanio::Number,
 calorias::Number
}deriving (Show,Eq)



grandeDeMuzza::Pizza
grandeDeMuzza = Pizza ["Salsa","mozzarella","oregano"] 8 350

nivelDeSatisfaccion::Pizza->Number
nivelDeSatisfaccion pizza
 | elem "palmito" (ingredientes pizza) = 0
 | calorias pizza < 500 = length (ingredientes pizza) * 80
 | otherwise =( length (ingredientes pizza) * 80 ) / 2

valorDeUnaPizza::Pizza->Number
valorDeUnaPizza pizza = (length (ingredientes pizza) * 120 ) * tamanio pizza

nuevoIngrediente:: String->Pizza->Pizza
nuevoIngrediente ingrediente pizza = (agregaIngrediente ingrediente . aumentaCalorias ingrediente) pizza

agregaIngrediente::String->Pizza->Pizza
agregaIngrediente ingrediente pizza = pizza{
    ingredientes = ingrediente : ingredientes pizza
}
aumentaCalorias::String->Pizza->Pizza
aumentaCalorias ingrediente pizza = pizza{
    calorias = calorias pizza + (2 * ( length ingrediente))
}

agrandar::Pizza->Pizza
agrandar pizza = pizza{
    tamanio = min 10 (tamanio pizza + 2)
}

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

pizzeriaPescadito :: Pizzeria
pizzeriaPescadito pizzas = pizzeriaEspecial anchoas pizzas

anchoas::Pizza
anchoas = Pizza ["salsa" ,"anchoas"] 8 270

pizzeriaGourmet::Number->Pizzeria
pizzeriaGourmet exquisitez pizzas = (agrandarVariasPizzas . satisfaccionMayoraExquisitez exquisitez ) pizzas

satisfaccionMayoraExquisitez::Number->[Pizza]->[Pizza]
satisfaccionMayoraExquisitez exquisitez pizzas = filter ( \pizza->nivelDeSatisfaccion pizza > exquisitez  ) pizzas

agrandarVariasPizzas::[Pizza]->[Pizza]
agrandarVariasPizzas pizzas = map ( agrandar ) pizzas

pizzeriaLaJauja::Pizzeria
pizzeriaLaJauja pizzas = pizzeriaGourmet 399 pizzas

type Pizzeria = [Pizza]->[Pizza]
type Pedido = [Pizza]


sonDignasDeCalleCorrientes::[Pizza]->[Pizzeria]->[Pizzeria]
sonDignasDeCalleCorrientes  pizzas pizzeria = filter ( pizzeriaQuemejoraLaSatisfaccionDelPedido pizzas ) pizzeria


pizzeriaQuemejoraLaSatisfaccionDelPedido::[Pizza]->Pizzeria->Bool
pizzeriaQuemejoraLaSatisfaccionDelPedido pizzas pizzeria = nivelDeSatisfaccionDeUnPedido (pizzeria pizzas) > nivelDeSatisfaccionDeUnPedido pizzas

-- maximizaLaSatisfaccionDelPedido::[Pizza]->[Pizzeria]->Pizzeria
-- maximizaLaSatisfaccionDelPedido [] [pizzeria] = pizzeria
-- maximizaLaSatisfaccionDelPedido pizzas (pizzeria1:pizzeria2:pizzerias) --(pizzeria:pizzerias1:pizzerias2) 
--  |  pizzeriaQuemejoraLaSatisfaccionDelPedido pizzas pizzeria1 >pizzeriaQuemejoraLaSatisfaccionDelPedido pizzas pizzeria2 = pizzeria
--  |  otherwise =  maximizaLaSatisfaccionDelPedido pizzas pizzerias
maximizaLaSatisfaccionDelPedido::[Pizza]->[Pizzeria]->Pizzeria
maximizaLaSatisfaccionDelPedido [] [pizzeria] = pizzeria
maximizaLaSatisfaccionDelPedido pizzas (pizzeria1:pizzeria2:pizzerias) = maximizaLaSatisfaccionDelPedido pizzas (  ( comparar pizzas pizzeria1 pizzeria2 ) : pizzerias)

comparar::[Pizza]->Pizzeria->Pizzeria->Pizzeria
comparar pizzas pizzeria1 pizzeria2 
 | nivelDeSatisfaccionDeUnPedido (pizzeria1 pizzas) > nivelDeSatisfaccionDeUnPedido (pizzeria2 pizzas) = pizzeria1 
 | otherwise = pizzeria2



yoPidoCualquierPizza :: (a -> Number) -> (b -> Bool) -> [(a, b)] -> Bool
--yoPidoCualquierPizza  x y z = any (odd . x . fst) z && all (y . snd) z
yoPidoCualquierPizza  funcionA funcionB lista = any (odd . funcionA . fst) lista && all (funcionB . snd) lista
-- basicamente en esta funcion el any verifica si el primer elemento de la funcionA es un numero par
-- y en el all , verifica que todos los segundos elementos de la funcionb deben cumplir con los elementos que estan 
-- dentro de la lista

laPizzeriaPredilecta::Pedido->[Pizzeria]->Pedido
laPizzeriaPredilecta pedidos pizzerias = foldl ( realizaUnPedido  ) pedidos  pizzerias

realizaUnPedido::Pedido->Pizzeria->[Pizza]
realizaUnPedido pedido pizzeria = pizzeria pedido
frases = [
        "el arroz tiene vitaminas", 
        "el hambre se quita comiendo", 
        "la lombriz vive mucho tiempo",
        "con este calor deberias tomarte una cerveza"
    ]

simbols = [',', '.', '!', '?']

stopwords = ["para", "el", "la", "es", "muy", "lo", "tiene", "se", "tengo"]

calcula string = let
        -- Limpio el texto de simbolos tipograficos = "Hola! para todos" -> "Hola para todos"
        newString = [ caracter | caracter <- string, not(caracter `elem` simbols)] 
        
        -- Elimino los stopwords de mi cadena ingresada = "Hola para todos" -> ["Hola", "todos"]
        lStr = deleteStopWords newString

        -- Elimino los stopwords de cada una de mis frases (se crea una lista de listas)
        -- ejemplo: [ ["hambre", "feroz"], ["coma", "tortas"] ... ]
        lFrs = map deleteStopWords frases

        -- Eliminamos las palabras que se repiten en cada frase que estan contenidas en mi cadena ingresada
        newFrs = map (clean lStr) lFrs

        -- Eliminamos las palabras que se repiten entre frases


        -- Concateno la lista de mi cadena junto a la lista de listas
        -- Creo una sola lista con la funcion "unir". (Quito la lista de listas)
        -- ejemplo: ["Hola", "Todos", "hambre", "feroz", "coma", ...]
        -- Asi ya tenemos una lista que sera nuestro diccionario
        lDicc = unir ([lStr]++newFrs)

        -- Creamos una lista con los vectores 
        vString = vector lDicc lStr
        vFrases = map (vector lDicc) lFrs

        -- Creamos el vector con la funcion coseno
        similitudes = map (coseno vString) vFrases

        -- Sacamos la posicion del valor mas alto en el vector de similitudes
        n = posMayor similitudes (head similitudes) 0 0
        -- n = posMayor similitudes
    in
        frases !! n

-- [0,0.5, 0.6, 0.3] 0 0 0
-- [0.5, 0.6, 0.3] 0 0 1
-- [0.6, 0.3] 0.5 1 2
-- [0.3] 0.6 2 3
-- [] 0.3 2 4 -> la posicion del mayor es el 2,
-- Si regresamos a la lista de similitudes, la posicion 2 efectivamente es el mayor
-- El cual corresponde a la posicion 2 de la lista de frases original
posMayor [] v pos cont = pos
posMayor (x:xs) v pos cont = if x > v 
    then posMayor xs x cont (cont+1) 
    else posMayor xs x pos (cont+1)

-- ["hambre", "alimento"] ["hambre", "quita", "comiendo"] -> ["quita", "comiendo"]
clean [] [] = []
clean xs ys = [ y | y <- ys, not(y `elem` xs) ]

-- Creo una lista intencional donde saco el primer elemento del diccionario
-- y pregunto si dicho elemento pertenece a la lista de frases
vector lDicc listaFrases = [ if x `elem` listaFrases then 1 else 0 | x <- lDicc]

-- Recibe una lista de este estilo [["uno", "dos"],["tres","cuatro"]]
-- y retorna las uniones ["uno","dos","tres","cuatro"]
unir [] = []
unir (x:xs) = x ++ unir xs
    
deleteStopWords string = let
        -- Creo una lista separado por palabras = "Hola para todos" -> ["Hola", "para", "todos"]
        listWords = words string
        -- Elimino los stopwords de mi lista = ["Hola", "para", "todos"] -> ["Hola", "todos"]
        newList = [ word | word <- listWords, not(word `elem` stopwords)] 
    in
        newList

coseno l1 l2 = (productoPunto l1 l2) / ((euclidiana l1) * (euclidiana l2))

productoPunto l1 l2 = suma (mult l1 l2)

mult [] [] = [] 
mult (x:xs) (y:ys) = (x*y):(mult xs ys)

euclidiana l = sqrt (suma (map cuadrado l))

cuadrado x = x*x

suma [] = 0
suma (x:xs) = x + suma xs

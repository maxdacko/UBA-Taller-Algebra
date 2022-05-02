------------------------------Trabajo Práctico: El Juego de Pop-It---------------------------------------
type Posicion = [Int]
type Jugada   = (Int,Int)
-- Ejercicio 1:

reemplazo :: Int -> Int -> [Int] -> [Int]       --Reemplaza un elemento por otro.
reemplazo n a (x:xs)
    | n == 0    = (a:xs)
    | otherwise = (x:reemplazo (n - 1) a xs)

pertenece :: Int-> [Int] -> Bool         --Función que dice si un elemento pertenece o no a la lista.
pertenece i []  = False          
pertenece i (x:xs)                              
    | i == x    = True           
    | otherwise = pertenece i xs 

ubicacion :: [Int] -> Int -> Int --Función de haskell que selecciona un elemento en cierta ubicación
ubicacion l n = l!!n             --También se podría hacer una función como la siguiente:

{-
enésimoElemento :: [Int] -> Int -> Int
enésimoElemento (x:_) 0  =  x
enésimoElemento (_:xs) n = xs `enésimoElemento` (n-1)  
-}

quitar :: Int -> [Int] -> [Int]                 
quitar i []     = []                     --Elimina de una lista cierto objeto.
quitar i (x:xs) 
    | i == x    = xs
    | otherwise = (x:quitar i xs)

jugarSinCero :: [Int] -> [Int]                  --Auxiliar para que cuando una lista quede en 0
jugarSinCero p                                  --no figure el 0 en ella.
    | pertenece 0 p = quitar 0 p
    | otherwise     = p

jugar :: Posicion -> Jugada -> Posicion
jugar p (a, b) = jugarSinCero (reemplazo (a - 1) ((ubicacion p (a - 1)) - b) p)

{- Luego de entregarlo y revisando como podía optimizarlo ví que se podía hacer así:
jugarOpti :: Posicion -> Jugada -> Posicion
jugarOpti (x:xs) (1,m) 
    | (x - m) == 0 = xs
    | otherwise    = ((x - m) : xs)
jugarOpti (x:xs) (n,m) = x : jugarOpti xs (n - 1,m)
-}

-- Ejercicio 2:

longitud :: [Int] -> Int          --Devuelve como Int, el valor de la longitud de la lista.
longitud []     = 0               --Equivalente a usar 'lenght'.
longitud (x:xs) = 1 + longitud xs

combinacion :: Int -> [Int] -> [(Int, Int)]       -- Hace todas las combinaciones de un elemento con un conjunto.
combinacion _ []     = []                         -- La lista es del 1 hasta donde llega la 'cant. de piedras' 
combinacion n (x:xs) = ((n,x):combinacion n xs)   -- n tiene que ser la posicion, osea qué conjunto de 'piedras' es.      

enlistar :: Int -> [Int]
enlistar n = [1..n]

piedrasEn :: [Int] -> Int -> Int                                    --Esta función me arma una lista
piedrasEn (x:xs)  n                                                 --dependiendo cuántas 'piedras'
    | n == 1               = ubicacion (x:xs) (0)                   --haya en cada posición, por ejempo:
    | n == longitud (x:xs) = ubicacion (x:xs) (longitud (x:xs) - 1) --en [4,2,3], en 1 hay [1,2,3,4], en 2
    | otherwise            = piedrasEn xs (n - 1)                   --hay [1,2] y en 3 hay [1,2,3].

listaPiedras :: [Int] -> Int -> [Int]                             --Esta es la función que arma 
listaPiedras (x:xs) n                                             --la lista en si.
    | longitud (x:xs) == 1 = [1..x]
    | otherwise            = enlistar (piedrasEn (x:xs) n)

{-                         
(Usar ++ es lo mismo que usar una función del estilo concatenar) 

concatenar :: [Int] -> [Int] -> [Int]
concatenar [] ys = ys           
concatenar (x:xs) ys = (x : (xs `concatenar` ys))

Esto era mi idea original, de ahí saqué cémo sería el caso recursivo:          
posiblesJugadas :: [Int] -> [(Int, Int)]                                  
posiblesJugadas (x:xs)                                                   
    | xs == [] = []                                      
    | otherwise = combinacion 1 (listaPiedras (x:xs) 1) ++ combinacion 2 (listaPiedras (x:xs) 2) ++ etc
-}

jugadas :: [Int] -> Int -> [(Int, Int)]   -- Uso un 'contador' que vaya sumando de a uno para cuando avanza
jugadas [] _      = []                    -- de conjunto en conjunto.
jugadas (x:xs) n  =  combinacion (n + 1) (listaPiedras (x:xs) 1) ++ (jugadas (xs) (n + 1))  
              
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas p  =  jugadas p 0 

-- Ejercicio 3:

--'Una posición es ganadora si existe una jugada tal que la posición obtenida al
-- realizar dicha jugada no es ganadora'. 

ganadora :: [Int] -> [(Int, Int)] -> Bool                      -- Forma una lista de posibles jugadas              
ganadora p []     = False                                      -- y hago que vaya probando con cada una.
ganadora p (y:ys) = not(esPosicionGanadora (jugar p y)) || ganadora p ys 

esPosicionGanadora :: Posicion -> Bool                     -- La función en si necesita una auxiliar para ir
esPosicionGanadora p  =  ganadora p (posiblesJugadas p)    -- recorriendo todas las jugadas, y devuelve True
                                                           -- cuando la posición resultante a la jugada aplicada
                                                           -- es literalmente no-ganadora, es decir [].

{- La forma original, pero usando un renglón mas:

ganadora2 ::[Int] -> [(Int, Int)] -> Bool
ganadora2 p []                                = False
ganadora2 p (y:ys) 
    | not (esPosicionGanadora (jugarOPT p y)) = True
    | otherwise                               = ganadora2 p ys

-}

-- Ejercicio 4:

jugadaElegida :: [Int] -> [(Int, Int)] -> (Int, Int)    -- Aprovechando la función del ejercicio 3
jugadaElegida p (y:ys)                                  -- devuelve una jugada con la cual dejás al
    | not(esPosicionGanadora (jugar p y)) = y           -- rival en posición perdedora.
    | otherwise                           = jugadaElegida p ys

jugadaGanadora ::  Posicion -> Jugada                               
jugadaGanadora p = jugadaElegida p (posiblesJugadas p) 

-- Ejercicio 5:

contadorGanadoras :: [Int] -> [(Int, Int)] -> Int   -- Similar al ejercicio 4 del TP1, aprovecho la función
contadorGanadoras p []                       = 0    -- anterior, pero en vez de devolver la jugada, sumo 1
contadorGanadoras p (y:ys)                          -- y hago la recursión hasta que no haya mas posibilidades.
    | not (esPosicionGanadora (jugar p y))   = 1 + contadorGanadoras p ys 
    | otherwise                              = contadorGanadoras p ys

numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras [] = 0
numeroDeJugadasGanadoras p  = contadorGanadoras p (posiblesJugadas p)
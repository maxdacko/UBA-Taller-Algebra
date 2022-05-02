--LISTAS
--[1, 2, 3]
--[True, False, True]
--[] -- vacia

--[True, False, False] :: [Bool]
--[1, 2, 3, 4] :: [Int]
--[div 10 5, div 2 2] :: [Int]
--[[1], [2, 3], [], [1, 1000, 2, 0]] :: [[Int]]
--DEBEN TENER EL MISMO TIPO LOS INTEGRANTES DE LA LISTA

--head :: [a] -> a --devuelve primer elemento
--tail :: [a] -> a -- devuelve final del elemento (todo menos el primero)
--(:) :: a -> [a] -> [a] --agregar elemento a lista
--3:[4,2,3]

sumatoria :: [Int] -> Int
sumatoria l 
    | l == [] = 0
    | otherwise = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud l 
     | l == [] = 0
     | otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece x l 
    | l == [] = False
    | otherwise = (x == head l) || pertenece x (tail l)

primermultiplode45345 :: [Int] -> Int
primermultiplode45345 l
    | mod (head l) 45345 == 0 = (head l)
    | otherwise = primermultiplode45345 (tail l)

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

primerMultiploDe45345 :: [Int] -> Int
primerMultiploDe45345 n | null n = 0
                          | mod (head n) 45345 == 0 = head n
                          | otherwise = primerMultiploDe45345 (tail n)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x: xs ) = sumatoria xs + x

longAux :: [Int] -> Int -> Int
longAux n i | null n = i
            | otherwise = longAux (tail n) (i+1)


longitud:: [a] -> Int
longitud [] = 0
longitud(_:xs) = 1 + longitud xs

pertenece :: Int-> [Int] -> Bool
pertenece i [] = False
pertenece i (x:xs) | i == x = True
              | otherwise = pertenece i xs


productoria :: [Int] -> Int
productoria = product


sumaAux :: Int-> [Int] -> [Int] -> [Int]
sumaAux i (x:xs) k | null xs = [i+x]
                   | otherwise = i+x : sumaAux i xs k


sumarN :: Int -> [Int] -> [Int]
sumarN i [] = []
sumarN i n = sumaAux i n []

sumarPriAux :: [Int]->Int->[Int]
sumarPriAux (x:xs) i | null xs = [i+x]
                     | otherwise = i+x : sumarPriAux xs i

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarPriAux (x:xs) x

sUltAux2 :: [Int]->Int->[Int]
sUltAux2 (x:xs) i | null xs = [i+x]
                  | otherwise = i+x : sUltAux2 xs i

sUltAux :: [Int]->[Int]->[Int]
sUltAux n (x:xs) | null xs = sUltAux2 n x
                 | otherwise = sUltAux n xs


sumarElUltimo :: [Int] -> [Int]
sumarElUltimo [] = []
sumarElUltimo n = sUltAux n n

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | even x = x : pares xs
             | otherwise = pares xs

quitar :: Int -> [Int] -> [Int]
quitar i [] = []
quitar i (x:xs) | i == x = xs
                | null xs = [x]
                | otherwise = x : quitar i xs

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas i [] = []
quitarTodas i (x:xs) | i == x = quitarTodas i xs
                | null xs = [x]
                | otherwise = x : quitarTodas i xs


repeticion :: Int->[Int]->Bool
repeticion i [] = False
repeticion i (x:xs) | i == x = True
                    | otherwise = repeticion i xs

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | repeticion x xs = True
                    | otherwise = hayRepetidos xs

revisarrepe :: Int->Int->[Int] -> Bool
revisarrepe i 0 (x:xs) = True
revisarrepe i j (x:xs) | i == x = False
                       | otherwise = revisarrepe i (j-1) xs

repe :: [Int]->[Int]->Int->[Int]
repe [] l n = [] 
repe (x:xs) l n | revisarrepe x (length l - length (x:xs)) l = x : repe xs l n 
                | otherwise = repe xs l (n-1) 

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal n = repe n n (length n)


repein :: Int->[Int]->Bool 
repein n [] = False
repein n (x:xs) | n == x = True 
                | otherwise = repein n xs


eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = []
eliminarRepetidosAlInicio (x:xs) | repein x xs = eliminarRepetidosAlInicio xs
                                 | otherwise = x : eliminarRepetidosAlInicio xs

grande :: [Int]->Int->Int 
grande [] i = i
grande (x:xs) i | x > i = grande xs x
                | otherwise = grande xs i

maximo :: [Int] -> Int
maximo n = grande n 0 

vuelta :: [Int]->[Int]->[Int]
vuelta (x:xs) l | null (x:xs) = l
                | null xs = x : l
                | x < head xs = x : l ++ vuelta xs l
                | otherwise = head xs: l ++ vuelta (x:tail xs) l

pequeño :: [Int]->[Int]->Int->[Int]
pequeño n l 0 = n
pequeño n l q = pequeño (vuelta n l) l (q-1)

ordenar :: [Int] -> [Int]
ordernar [] = []
ordenar n = pequeño n  [] (length n)

vueltas :: [Int]->[Int]->[Int]
vueltas (x:xs) l | null (x:xs) = l
                | null xs = x : l
                | x > head xs = x : l ++ vueltas xs l
                | otherwise = head xs: l ++ vueltas (x:tail xs) l

vueltero :: [Int]->[Int]->Int->[Int]
vueltero n l 0 = n
vueltero n l q = vueltero (vueltas n l) l (q-1)

reverso :: [Int] -> [Int]
reverso [] = []
reverso n = vueltero n [] (length n)


juntar :: [a] -> [b] -> Int -> [(a,b)] -> [(a,b)]
juntar (x:xs) (y:ys) i n | i < 2 = [(x,y)]
                       | otherwise = n ++ [(x,y)] ++ juntar xs ys (i-1) n

zipi :: [a] -> [b] -> [(a,b)]
zipi [] b = []
zipi a [] = []
zipi a b | length a > length b = juntar a b (length b) []
         | otherwise = juntar a b (length a) []

des :: Integer -> Integer -> [Integer]
des 0 d = [0]
des a d = des (div a d) d ++ [mod a d]



reemplazo :: Int -> Int -> [Int] -> [Int]       --Reemplaza un elemento por otro.
reemplazo n a (x:xs)
    | n == 0    = (a:xs)
    | otherwise = (x:reemplazo (n - 1) a xs)




ubicacion :: [Int] -> Int -> Int                --Función de haskell que selecciona un elemento de cierta 
ubicacion l n = l!!n                            --ubicación. También se podría hacer una función
                                                --del estilo 'enésimoElemento' escrita aqui debajo: 
{-
                                                enesimoE :: [Int] -> Int -> Int
                                                (x:_)  `enesimoE` 0 =  x
                                                (_:xs) `enesimoE` n =  xs `enesimoE` (n-1)
-}



jugarSinCero :: [Int] -> [Int]                  --Auxiliar para que cuando una lista quede en 0
jugarSinCero (x:xs)                             --no figure el 0 en ella.
    | pertenece 0 (x:xs) = quitar 0 (x:xs)
    | otherwise          = (x:xs)

jugar :: [Int] -> (Int, Int) -> [Int]
jugar (x:xs) (a, b) = jugarSinCero (reemplazo (a - 1) ((ubicacion (x:xs) (a - 1))-b) (x:xs))
     
--combinatoria:
--recordando fact:
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

combinatorio :: Int -> Int -> Int
combinatorio n k = (fact n) `div` ((fact k) * (fact (n - k)))

combinatorio2 :: Int -> Int -> Int
combinatorio2 n 0 = 1
combinatorio2 n k 
    | n == k = 1
    | otherwise = (combinatorio2 (n - 1) k) + (combinatorio2 (n - 1) (k - 1))

--variaciones con repeticion:

type Set a = [a]

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

agregarElementosAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementosAdelante x [] = []
agregarElementosAdelante x (ys:yss) = agregar (x:ys) (agregarElementosAdelante x yss)

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _ = []
agregarElementosAListas (x:xs) c = union (agregarElementosAdelante x c) (agregarElementosAListas xs c)

variaciones :: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k - 1))

 --permutaciones

insertarEn :: [Int] -> Int -> Int -> [Int] 
insertarEn xs n i 
    | i == 1 = n:xs
    | otherwise = (head xs) : (insertarEn (tail xs) n (i - 1))

insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos xs c 1 = agregar (insertarEn xs c 1) vacio
insertarEnCadaPos xs c i = agregar (insertarEn xs c i) (insertarEnCadaPos xs c (i - 1))


insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] c = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = (insertarEnCadaPos xs c (length xs + 1)) `union` 
                                               (insertarEnCadaPosDeTodasLasListas xss c)

permutaciones :: Set Int -> Set [Int] 
permutaciones [] = [[]]  
permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c


--ejercicios

--EJERCICIO 1
{-| todas las formas de ubicar n bolitas numeradas en k cajas-}
bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas n k = bolitasEnCajasAux n k (bolita k)
    where
        
        bolitasEnCajasAux :: Int -> Int -> Set [Int] -> Set [Int]
        bolitasEnCajasAux n k (x:xs) | n == 1        = (x:xs)
                                     | otherwise     = bolitasEnCajasAux (n-1) k (agregarBolita (x:xs) k) 
            

agregarBolita :: Set [Int] -> Int -> Set [Int] --agregar una bolita de todas las formas posibles a cada caso
agregarBolita   []   k = []
agregarBolita (x:xs) k = (agregarUnaBolita x k)++(agregarBolita xs k)

                        
agregarUnaBolita :: [Int] -> Int -> Set [Int] --ubicar una bolita de todas las formas posibles a 1 caso
agregarUnaBolita x k | k == 1 = [k:x]
                     | otherwise = agregar (k:x) (agregarUnaBolita x (k-1)) 

bolita :: Int -> Set [Int] --ubicar 1 sola bolita
bolita k | k == 1 = [[1]]
         | otherwise = agregar [k] (bolita (k-1))



--EJERCICIO 2
-- |formas de ubicar n bolitas en k cajas de forma que la primer caja nunca esté vacía
primerCajaCon :: Int -> Int -> Set [Int]
primerCajaCon n k = filtrar (bolitasEnCajas n k)
    where
        filtrar :: Set [Int] -> Set [Int]
        filtrar (x:xs) | xs == []     = []
                       | pertenece1 x = x: (filtrar xs)
                       | otherwise    = filtrar xs
            where
                pertenece1 :: [Int] -> Bool
                pertenece1 []     = False
                pertenece1 (x:xs) | x == 1 = True
                                  | otherwise = pertenece1 xs



--EJERCICIO 3
-- |Todas las listas ordenadas de k n´umeros distintos tomados del conjunto {1, . . . , n}
listaKElementosOrdenadosHastaN :: Int -> Int -> Set [Int]
listaKElementosOrdenadosHastaN k n = soloOrdenados (listaKElementosHastaN k n)
    where
        soloOrdenados :: Set [Int] -> Set [Int]
        soloOrdenados []     = []
        soloOrdenados (x:xs) | ordenados x = x:soloOrdenados xs
                             | otherwise   = soloOrdenados xs 


ordenados :: Ord a => Set a -> Bool
ordenados (x:xs) | length (x:xs) <= 1 = True 
                 | x `menorATodos` xs = ordenados xs
                 | otherwise          = False

menorATodos :: Ord a => a -> Set a -> Bool
menorATodos x   []   = True
menorATodos x (y:ys) | x < y       = menorATodos x ys  
                     | otherwise   = False


-- | Todas las listas de k n´umeros distintos tomados del conjunto {1, . . . , n}
listaKElementosHastaN :: Int -> Int -> Set [Int]
listaKElementosHastaN k n = sinElemRepetidos (bolitasEnCajas k n)
    where
        sinElemRepetidos :: Eq a => Set [a] -> Set [a]
        sinElemRepetidos   []   = []
        sinElemRepetidos (x:xs) | elemDistintos x = x: (sinElemRepetidos xs)
                                | otherwise       = sinElemRepetidos xs
            where
                elemDistintos :: Eq a => [a] -> Bool
                elemDistintos   []   = True
                elemDistintos (x:xs) | x `pertenece` xs = False
                                     | otherwise        = elemDistintos xs
                    where
                        pertenece :: Eq a => a -> Set a -> Bool
                        pertenece _  []    = False
                        pertenece x (y:ys) | x == y    = True
                                           | otherwise = pertenece x ys




--EJERCICIO 4
-- |Todas las sucesiones de los caracteres ’a’ y ’b’ de longitud n y m respectivamente.
permutacionesDeNAyMB :: Int -> Int -> Set String
permutacionesDeNAyMB n m = auxPermutacionesDeNAyMB n m [['a'],['b']]
    where
        auxPermutacionesDeNAyMB :: Int -> Int -> Set String -> Set String
        auxPermutacionesDeNAyMB n m (x:xs) 
            | length x == n + m = (x:xs)
            | otherwise = auxPermutacionesDeNAyMB n m (duplicarYAgrandar (x:xs) n m) 
            where
            
                duplicarYAgrandar :: Set String -> Int -> Int -> Set String
                duplicarYAgrandar   []   n m = []
                duplicarYAgrandar (x:xs) n m 
                    | listoA    = masB:(duplicarYAgrandar xs n m) 
                    | listoB    = masA:(duplicarYAgrandar xs n m) 
                    | otherwise = masA:masB:(duplicarYAgrandar xs n m)
                    where
                        listoA = (numeroDeVeces 'a' x 0) == n
                        listoB = (numeroDeVeces 'b' x 0) == m
                        masA = 'a':x
                        masB = 'b':x
                        
numeroDeVeces :: Eq a => a -> [a] -> Int -> Int
numeroDeVeces k   []   c = c
numeroDeVeces k (x:xs) c | k == x    = numeroDeVeces k xs (c+1)
                         | otherwise = numeroDeVeces k xs c 


--EJERCICIO 5
-- |5 Todas las sucesiones de ’a’, ’b’ y ’c’ de longitud n, m y k respectivamente
permutacionesDeNAMBKC :: Int -> Int -> Int -> Set String
permutacionesDeNAMBKC n m k = auxPermutacionesDeNAMBKC n m k [['a'],['b'],['c']]
    where
        auxPermutacionesDeNAMBKC :: Int -> Int -> Int -> Set String -> Set String
        auxPermutacionesDeNAMBKC n m k (x:xs) 
            | length x == n + m + k = (x:xs)
            | otherwise = auxPermutacionesDeNAMBKC n m k (duplicarYAgrandar (x:xs) n m k) 
            where
            
                duplicarYAgrandar :: Set String -> Int -> Int -> Int -> Set String
                duplicarYAgrandar   []   n m k = []
                duplicarYAgrandar (x:xs) n m k
                    | listoA && listoB = masC:(duplicarYAgrandar xs n m k) 
                    | listoA && listoC = masB:(duplicarYAgrandar xs n m k)
                    | listoB && listoC = masA:(duplicarYAgrandar xs n m k)
                    | listoA           = masB:masC:(duplicarYAgrandar xs n m k)
                    | listoB           = masA:masC:(duplicarYAgrandar xs n m k)
                    | listoC           = masA:masB:(duplicarYAgrandar xs n m k)
                    | otherwise        = masA:masB:masC:(duplicarYAgrandar xs n m k)
                    where
                        listoA = (numeroDeVeces 'a' x 0) == n
                        listoB = (numeroDeVeces 'b' x 0) == m
                        listoC = (numeroDeVeces 'c' x 0) == k
                        masA = 'a':x
                        masB = 'b':x
                        masC = 'c':x


--EJERCICIO 6
-- | Dado un conjunto de enteros y un entero k, genera todos los subconjuntos de k elementos
subConjuntos :: Int -> Set Int -> Set (Set Int)
subConjuntos 0 _      = [[]]
subConjuntos _ []     = []
subConjuntos k (x:xs) = (agregarATodos x (subConjuntos (k-1) xs)) `union` (subConjuntos k xs)


agregarATodos :: Eq a => a -> Set (Set a) -> Set (Set a)
agregarATodos _ []     = []
agregarATodos p (x:xs) = (p:x) : (agregarATodos p xs) 


enesimoE                :: [Int] -> Int -> Int
(x:_)  `enesimoE` 0         =  x
(_:xs) `enesimoE` n         =  xs `enesimoE` (n-1)

concatt :: [a] -> [a] -> [a]
[]     `concatt` ys = ys
(x:xs) `concatt` ys = x : (xs `concatt` ys)
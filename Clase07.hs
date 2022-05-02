--Conjuntos
--usamos listas sin importar el orden en este caso y sin numeros repetidos

--el vacio []
--union: no sirven los dos puntos de agregar siemrpe, pero si no son repetidos si
--pertenece:

type Set a = [a]

vacio :: [Int]
vacio = []

union :: Set Int -> Set Int -> Set Int
union [] c     = c
union (x:xs) c | pertenece x c = union xs c
               | otherwise    = x : (union xs c)

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

agregar :: Int -> [Int] -> [Int]
agregar x c | pertenece x c = c 
            | otherwise = x : c

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

-- -------
pertenecec :: Int -> [Int] -> Bool
pertenecec _ [] = False
pertenecec x (y:ys) | x == y = True
                    | otherwise = pertenecec x ys

agregarc :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarc x c | pertenece x c = c 
             | otherwise = x : c

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarc (agregar x c) (agregarATodos x cs)


partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = union (partes xs) (agregarATodos x (partes xs))

{-pertenece para conjuntos en conjuntos de conjuntos-}
pertenceC :: Set Int -> Set (Set Int) -> Bool
pertenceC xs []       = False
pertenceC xs (ys:yss) = iguales xs ys || pertenceC xs yss


{-agregar conjunto a conjunto de conjuntos-}
agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | pertenceC xs xss = xss
                | otherwise        = xs:xss


{-union de conjuntos de conjuntos-}
unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] c       = c
unionC (xs:xss) c | pertenceC xs c = unionC xss c
                  | otherwise      = xs : (unionC xss c)




{-conjunto de partes de un conjunto con todos los naturales hasta n-}
partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = (partesN (n-1)) ++ (agregarATodos n (partesN (n-1))) 
--con n=3 hace esto:
--[] + [1] -> [] [1] + [2] [1,2] -> [] [1] [2] [1,2] + [3] [1,3] [2,3] [1,2,3] 
-- n = 1   ->       n = 2        ->                 n=3


productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano    _     []   = []
productoCartesiano   []     _    = []
productoCartesiano (x:xs) (y:ys) = (productoCartesianoAux (x) (y:ys)) ++ (productoCartesiano xs (y:ys)) --prod cartesiano del primer elemento con y, sumo el resto recursivamente

productoCartesianoAux :: Int -> Set Int -> Set (Int, Int) --hace todas las cobinaciones de un elemento con un conjunto
productoCartesianoAux _   []   = [] 
productoCartesianoAux x (y:ys) = (x,y) : productoCartesianoAux x ys


-- sabemos el algoritmo de division a= q.d + r

-- | Division de numeros naturales_0 : a `divNat ` d = a `div ` d
divNat :: Int -> Int -> Int
divNat a d | a < d = 0
           | otherwise = (a - d ) `divNat ` d + 1
-- | Resto de numeros naturales_0 : a `modNat ` d = a `mod ` d
modNat :: Int -> Int -> Int
modNat a d = a - d *( a `divNat ` d)

-- | Modulo de numeros enteros a `modulo ` d = a `mod ` d
modulo :: Int -> Int -> Int
modulo a d | a >= 0 || r' == 0 = r'
           | otherwise = abs d - r'
        where r' = abs a `modNat ` abs d

-- | Division de numeros enteros : n `dividido ` m = n `div ` m
dividido :: Int -> Int -> Int
dividido a d = sgq * absq -- obs 1
    where absq = abs (a -r) `divNat ` (abs d ) -- obs 2
          sgq = ( signum a ) * ( signum d) -- obs 3
          r = a `modulo ` d


mcdDef :: Int -> Int -> Int
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo ( interseccion ( divisores a) ( divisores b ) )

type Set a = [a]

-- |Division de numeros naturales_0: a `divNat d = a `div` n
divNat :: Int -> Int -> Int
divNat a d | a < d = 0
           | otherwise = 1 + (a-d) `divNat` d


-- |Resto de numeros naturales_0: a `modNat`d = a `mod`d
modNat :: Int -> Int -> Int
modNat a d = a - d*(a `divNat` d)


-- |Modulo de numeros enteros a `modulo` d = a `mod` d
modulo :: Int -> Int -> Int
modulo a d | a >= 0 || r' == 0 = r'
           | otherwise         = (abs d) - r'
           where r' = (abs a) `modNat` (abs d)


-- |Division de numeros enteros: b `dividido`m = n `div`m
dividido :: Int -> Int -> Int
dividido a d = sgq * absq
    where 
        absq = abs (a-r) `divNat` (abs d)
        sgq  = (signum a) * (signum d)
        r    = a `modulo` d



--EJERCICIO 1

-- | Transforma un n de base 10 a base b. Nota: el output está dado vuelta ej en b=2, 2 = [1,0] pero la función da [0,1]
digitos :: Integer -> Integer -> [Integer]
digitos n b | n == 0 = []
            | otherwise = (n `mod` b) : digitos (n `div` b) b 


--EJERCICIO 2

-- | Deshace digitos
numero :: [Integer] -> Integer -> Integer
numero   []   _ = 0
numero (x:xs) b = x + b*(numero xs b)


--EJERCICIO 3

-- | Dado un n natural devuelve todos sus divisores positivos
divisores :: Int -> Set Int
divisores n = dividen n [1..floor(sqrt((fromIntegral n)))]
    where
        dividen :: Int -> Set Int -> Set Int
        dividen _   []   = [] 
        dividen n (x:xs) | n `mod` x == 0 = n`div`x:x:dividen n xs
                         | otherwise      =           dividen n xs


--EJERCICIO 4

-- |Minimo común divisor de dos enteros por definición
mcdDef :: Int -> Int -> Int
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo ( interseccion ( divisores a) ( divisores b ) )
    
    where
        interseccion :: Set Int -> Set Int -> Set Int 
        interseccion   []    _                    = []
        interseccion (x:xs) c | x `elem` c = x : interseccion xs c
                              | otherwise  =     interseccion xs c
            
        maximo :: Ord a => Set a -> a
        maximo  (x:[])              = x
        maximo (x:y:xs) | x > y     = maximo (x:xs)
                        | otherwise = maximo (y:xs)


--EJERCICIO 6

-- | mcd usando el algoritmo de Euclides
mcd :: Int -> Int -> Int
mcd a 0 = abs a
mcd a b = mcd b (a `mod` b)


-- EJERCICIO 8

-- | Minimo comun multiplo entre dos enteros no negativos
mcm :: Int -> Int -> Int
mcm a b = a*b `div` mcd a b



--EJERCICIO 9

-- | Dados a y b, utiliza el algoritmo de Euclides extendido para obtener una tripla ((a : b), s,t) tal que sa + tb = (a : b)
emcd :: Int -> Int -> (Int, Int, Int) 
emcd a 0
    | a > 0 = (a, 1, 0)
    | a < 0 = (-a, -1, 0)
emcd a b = (d, t, s - t * (a `div` b)) 
    where 
        (d, s, t) = emcd b (a `mod` b)


--EJERCICIO 10

-- | Minimo s >= 0 tq sa + tb = (a:b)
menorS :: Int -> Int -> Int
menorS a b = auxMenorS (emcd a' b')
    where
        a' = a `div` mcd a b
        b' = b `div` mcd a b

        auxMenorS :: (Int,Int,Int) -> Int  
        auxMenorS (_,s0,_) = s0 `mod` abs b'    --s = s0 + kb', el abs es porque mod anda mal >:(

















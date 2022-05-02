-- 1 ----------------------------
ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente (a, b, m) | b `mod` d /= 0 = error "No existe Solución"
                        | otherwise = (a `div` d, b `div` d , m `div` d)
         where d = mcd a m

solucionEcConPropAdic :: (Int, Int, Int) -> (Int,Int)
solucionEcConPropAdic (a, b, m) = ((s*b) `mod` m, m)
         where (d, s, t) = emcd a m

solucionEc :: (Int, Int, Int) -> (Int, Int)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

-- 2 ----------------------------
sistemaSimplifEquiv :: [(Int, Int, Int)] -> [(Int,Int)]
sistemaSimplifEquiv [] = []
sistemaSimplifEquiv (e:es) = (solucionEc e) : (sistemaSimplifEquiv es)

-- 3 ----------------------------
modulos :: [(Int,Int)] -> [Int]
modulos [] = []
modulos ((r, m):es) = m : (modulos es)

mayorModulo :: [(Int, Int)] -> Int
mayorModulo sist = maximum (modulos sist)

cotaParaPrimoMaloDesde :: [(Int, Int)] -> Int -> Int
cotaParaPrimoMaloDesde sist n | nEsimoPrimo (n+1) > (mayorModulo sist) = n
                              | otherwise = cotaParaPrimoMaloDesde sist (n+1)

cotaParaPrimoMalo :: [(Int, Int)] -> Int
cotaParaPrimoMalo sist = cotaParaPrimoMaloDesde sist 1 -- ??????

cantidadMultiplos :: [Int] -> Int -> Int
cantidadMultiplos [] _ = 0
cantidadMultiplos (m:ms) n | m `mod` (nEsimoPrimo n) == 0 = 1 + cantidadMultiplos ms n
                           | otherwise = cantidadMultiplos ms n

esPrimoMalo :: [(Int, Int)] -> Int -> Bool
esPrimoMalo sist n = cantidadMultiplos (modulos sist) n >=2

todosLosPrimosMalosHasta :: [(Int, Int)] -> Int -> [Int]
todosLosPrimosMalosHasta _ 0 = []
todosLosPrimosMalosHasta sist n | esPrimoMalo sist n = (nEsimoPrimo n) : (todosLosPrimosMalosHasta sist (n-1))
                                | otherwise = todosLosPrimosMalosHasta sist (n-1)

todosLosPrimosMalos :: [(Int, Int)] -> [Int]
todosLosPrimosMalos [] = []
todosLosPrimosMalos sist = todosLosPrimosMalosHasta sist (cotaParaPrimoMalo sist)

-- 4 ----------------------------
solucDosEcPotenciasPrimoOrd :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2) | (r2-r1) `mod` m1 == 0 = (r2, m2)
                                              | otherwise = error "No existe Solución"

solucDosEcPotenciasPrimo :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimo (r1, m1) (r2, m2) | m1 <= m2  = solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2)
                                           | otherwise = solucDosEcPotenciasPrimoOrd (r2, m2) (r1, m1)

solucSistemaPotenciasPrimo :: [(Int, Int)] -> (Int, Int)
solucSistemaPotenciasPrimo [e] = e
solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo ((solucDosEcPotenciasPrimo e1 e2):es)

-- 5 ----------------------------
desdoblarSistemaEnFcionPrimo :: [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
desdoblarSistemaEnFcionPrimo [] _ = ([], [])
desdoblarSistemaEnFcionPrimo ((r, m):es) p 
    | k == 0 = (pri, (r, m):seg)
    | m == p^k = ((r,m):pri, seg)
    | otherwise = ( (r `mod` (p^k) , p^k):pri  , (r `mod` (m `div` (p^k)) , m `div` (p^k)):seg)
         where (pri, seg) = desdoblarSistemaEnFcionPrimo es p
               k= quePotenciaLoDivide m p


sistemaEquivSinPrimosMalosAux :: [(Int, Int)] -> [Int] -> [(Int, Int)]
sistemaEquivSinPrimosMalosAux sist [] = sist
sistemaEquivSinPrimosMalosAux sist (p:ps) = (solucSistemaPotenciasPrimo pri) : (sistemaEquivSinPrimosMalosAux seg ps)
         where (pri, seg) = desdoblarSistemaEnFcionPrimo sist p

sistemaEquivSinPrimosMalos :: [(Int, Int)] -> [(Int, Int)]
sistemaEquivSinPrimosMalos sist = sistemaEquivSinPrimosMalosAux sist (todosLosPrimosMalos sist)

-- 6 ----------------------------
solucSistemaModCoprimos :: [(Int, Int)] -> (Int, Int)
solucSistemaModCoprimos [e] = e
solucSistemaModCoprimos ((r1, m1):(r2, m2):es) = solucSistemaModCoprimos ((r, m1*m2):es)
         where (d,s,t) = emcd m1 m2
               r= (r1*t*m2 + r2*s*m1) `mod` (m1*m2)

solucSistema :: [(Int, Int, Int)] -> (Int, Int)
solucSistema sist = solucSistemaModCoprimos (sistemaEquivSinPrimosMalos (sistemaSimplifEquiv sist))

mcd :: Int -> Int -> Int
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

emcd :: Int -> Int -> (Int, Int, Int)
emcd a 0 = (a, 1, 0)
emcd a b = (d, t, s - t * k)
             where (k, r) = (div a b, mod a b)
                   (d, s, t) = emcd b r

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise      = menorDivisorDesde n (k+1)

quePotenciaLoDivide :: Int -> Int -> Int
quePotenciaLoDivide m p | m `div` p >= 1 && m `mod` p == 0 = 1 + quePotenciaLoDivide (m `div` p) p
                        | otherwise = 0
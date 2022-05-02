-------------Trabajo Práctico: La conjetura de Goldbach

--Ejercicio 1:
--Auxiliares para hayar números primos:
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise = menorDivisorDesde n (k + 1)

divisor :: Integer -> Integer
divisor n = menorDivisorDesde n 2

primo :: Integer -> Bool --Da True si el menor divisor (sacando el 1) es el mismo
primo 1 = False
primo n = n == divisor n

{-
auxiliarG :: Integer -> Integer -> Bool    con esta función inicié el planteo
auxiliarG a n                              del ejercicio, que luego reemplacé 
    | primo a && primo (n - a) = True      por 'obtengoA' y 'obtengoB'
    | otherwise = auxiliarG (a + 1) n
-}

obtengoB :: Integer -> Integer -> Integer -- Separo el b de 'a + b = n'
obtengoB a n 
    | primo a && primo (n - a) = n - a
    | otherwise = obtengoB (a + 1) n

obtengoA :: Integer -> Integer -> Integer -- Separo el a
obtengoA a n 
    | primo a && primo (n - a) = a
    | otherwise = obtengoA (a + 1) n

sacoB :: Integer -> Integer  --Saco el b para poder usarlo como 
sacoB n = obtengoB 0 n       --un solo número

sacoA :: Integer -> Integer  --Lo mismo con el a
sacoA n = obtengoA 0 n

condiciones :: Integer -> Bool -- Hago que sea par y mayor que 2
condiciones n
    | n > 2 && n `mod` 2 == 0 = True 
    | otherwise = False

satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n
    | condiciones n && n == sacoA n + sacoB n = True
    | otherwise = False

--Ejercicio 2:

{-
verificarConjeturaHasta :: Integer -> Bool                           --Va probando la conjetura entre los 
verificarConjeturaHasta n                                            --números pares menores que n 
    | n == 4 = True                                                  --por eso se hace la recursión con n - 2
    | satisfaceGoldbach n && verificarConjeturaHasta (n - 2) = True
    | otherwise = False

Recién repasando descubrí un error en el ejercicio 2, ya que lo que yo creia que me calculaba para 
todos los pares menores, en realidad me decia nada mas que para el anterior, la función debería quedar como:
-}


verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n 
    | n == 4 = True
    | satisfaceGoldbach n = verificarConjeturaHasta (n - 2)
    | otherwise = False


--Ejercicio 3:

descomposicionEnPrimos :: Integer -> (Integer, Integer)
descomposicionEnPrimos n = (sacoA n, sacoB n)  

--Ejercicio 4: 

contadorDePares :: Integer -> Integer -> Integer   --Uso una f. auxiliar que cuando se cumple
contadorDePares a n                                --que a y n-a es primo, sume 1 y repita la  
    | n == a = 0                                   --función diciendo cuántos pares ord. hay
    | primo a && primo (n - a) = 1 + contadorDePares (a + 1) n
    | otherwise = contadorDePares (a + 1) n
    
numeroDeDescomposiciones :: Integer -> Integer  
numeroDeDescomposiciones n = contadorDePares 2 n     










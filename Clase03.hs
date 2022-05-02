factorial :: Int->Int
factorial n | n == 0 = 1
            | n  > 0 = n * factorial (n-1)

--otro factorial haria una recursion infinita para numeros negativos:
--ya que el anterior ni admite negativos

otrofactorial :: Int->Int 
otrofactorial n | n == 0 = 1
                | otherwise = n * otrofactorial (n-1)

--con pattern matching (tambien con problema para negativos):

factorialpm :: Int->Int 
factorialpm 0 = 1
factorialpm n = n * factorialpm (n-1)

--mas ejemplos, siempre hay que llegar a un caso base, a veces se necesitan dos:


esPar :: Int -> Bool 
esPar n | n == 0 = True 
        | n == 1 = False 
        | otherwise  = esPar (n-2)

--ejercicio

fib :: Int -> Int 
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n - 1) + fib(n - 2)

parteEnt :: Float -> Int 
parteEnt n | n < 0 = undefined 
           | n < 1 = 0
           | otherwise  = 1 + parteEnt(n - 1)

--volviendo a ej viendo como se usaba div:

digitoDecenas :: Int -> Int
digitoDecenas n = div ( mod n 100 ) 10

multiplo3 :: Int -> Bool
multiplo3 n | n == 0 = True
            | n == 1 = False
            | n == 2 = False
            | otherwise = multiplo3 (n-3) 


{- 2_sumaImpares -}

sumaImpares :: Int -> Int
sumaImpares n | n == 0 = 0
              | n > 0 = (-) ((*) n 2) 1  + sumaImpares (n-1)
              

{- 3_medioFact -}

medioFact  :: Int -> Int
medioFact n | n == 1 = 1
            | n == 2 = 2
            | otherwise = (*) n (medioFact (n-2))


{- 4_sumaDigitos -}

sumaDigitos :: Int -> Int
sumaDigitos n | n < 10 = n
              | otherwise =  n `mod` 10 + sumaDigitos (n `div` 10) 


{- 5_digitosIguales  -}

digitosIguales :: Int -> Bool
digitosIguales n | n < 10 = True 
                 | mod (div n 10) 10 == mod n 10 = digitosIguales (div n 10)              
                 | otherwise = False 




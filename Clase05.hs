fact :: Int -> Int
{-|factorial de toda la vida-}
fact 0 = 1
fact n = n * fact (n-1)

{-| productoria de i=d hasta i= h de i-}
prod :: Int -> Int -> Int
prod d h | d == h = d
         | otherwise = h * prod d (h-1) -- calculado de h a d 

{-|factorial usando prod-}
factv2 :: Int -> Int
factv2 n = prod 1 n 

{-| productoria de i=d hasta i=h de i-}
prodv2 :: Int -> Int -> Int
prodv2 d h | d == h = h
           | otherwise = d * prodv2 (d+1) h --calculado de d a h

--EJERCICIO 1

{-|suma de divisores de n menores iguales a k-}
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1         = 1 --cuando k llega a 1, k va a dividir a n, entonces sumamos k
                       | n `mod` k == 0 = k + sumaDivisoresHasta n (k-1) --si k divide lo sumamos y vamos con k-1
                       | otherwise      = sumaDivisoresHasta n (k-1)-- si k no divide vamos con k-1 sin sumar k

--EJERCICIO 2

{-|suma divisores de n-}
sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n -- uso la función anterior con divisores menores iguales a n, y como n es el mayor divisor de n, voy a tener en cuenta todos los divisores

{-|suma de divisores de n mayores iguales a k-}
sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n k | k > n          = 0 --por si sos tan forrp de poner un k mayor a n, obviamente no hay divisores de n mayores a n
                       | k == n         = k --cuando k llega a n, k va a dividir a n, entonces sumamos k
                       | n `mod` k == 0 = k + sumaDivisoresDesde n (k+1) --si k divide lo sumamos y vamos con k+1
                       | otherwise      = sumaDivisoresDesde n (k+1)-- si k no divide vamos con k+1 sin sumar k

{-|suma de divisores de n-}
sumaDivisoresv2 :: Int -> Int
sumaDivisoresv2 n = sumaDivisoresDesde n 1 --uso la función anterior


--EJERCICIO 3

{-|devuelve el menor divisor de n que sea mayor/igual a k-}
menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | k > n          = undefined --si k es mayor a n no vamos a encontrar nigun divisor
                      | k == n         = k --si k = n k es divisor
                      | n `mod` k == 0 = k --si k divide a n es porque k devuelve k la primera vez que pasa por la linea
                      | otherwise      = menorDivisorDesde n (k+1) --si k no era divisor vamos con k+1


{-| devuelve el menor divisor de n distinto de 1-}
menorDivisorNoTrivial :: Int -> Int
menorDivisorNoTrivial 1 = 1
menorDivisorNoTrivial n = menorDivisorDesde n 2

--EJERCICIO 4

{-|devuelve True si es primo (para la función los negativos no son primos), False si no es primo-}
esPrimo :: Int -> Bool
esPrimo n | n <= 1                       = False --menor a 1 no hay ningún primo
          | menorDivisorNoTrivial n == n = True -- True si el menor divisor que no sea 1 es el mismo n, si hay algún divisor en el medio es porque no es primo (tendría más divisores que 1 y si mismo)
          | otherwise                    = False --si no sos menor a 1 pero tu menor divisor no trivial no sos vos mismo, no sos primo


--EJERCICIO 5

{-|devuelve el primer primo mayor/igual a n-}
minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n --le pregunto si n es primo
                   | otherwise = minimoPrimoDesde (n+1) --si n no es primo vamos con el n que sigue


{-|devuelve el n-esimo primo-}
nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2 --2 es el primer primo
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1)) -- el n-esimo primo va a ser el primer primo desde el n-1 primo


--EJERCICIO 6


{-|devuelve el minimo k!>=i que sea mayor a m. Es decir el menor factorial mayor/igual a m, buscando desde i en adelante-}
menorFactorialDesdeDesde :: Int -> Int -> Int
menorFactorialDesdeDesde i m | fact i >= m = fact i -- si i! ya es mayor/igual a m, ya encontramos lo que buscabamos
                             | otherwise = menorFactorialDesdeDesde (i+1) m --si i! no es mayor/igual a m, vamos a probar con (i+1)!


{-|devuelve el menor factorial que sea mayor/igual al input-}
menorFactorialDesde :: Int -> Int
menorFactorialDesde n = menorFactorialDesdeDesde 1 n --buscamos el menor factorial mayor/igual a n partiendo desde i!


--EJERCICIO 7

{-|devuelve el mayor k! tal que k>=i y que k! < m. 
En criollo, el primer factorial que sea mayor a i! pero menor a m.
Si se cumple que i! es mayor a m, va a devolver el anterior aunque sea menor a la cota inferior (i!) porque no va a encontrar otro ya que todos van a ser mayores a m-}
mayorFactorialHastaDesde :: Int -> Int -> Int
mayorFactorialHastaDesde i m | fact i > m  = fact (i-1) --si i! es mayor a m, es porque nos pasamos de m, el factorial anterior es lo que buscabamos
                             | fact i == m = fact i --si i! es igual a m, esa es nuestra respuesta, el mayor factorial menor/igual a m
                             | otherwise   = mayorFactorialHastaDesde (i+1) m --si no lo encontramos vamos con el siguiente factorial


{-|devuelve el mayor factorial que sea menor a n, es decir el factorial anterior al numero inputeado-}
mayorFactorialHasta :: Int -> Int
mayorFactorialHasta n = mayorFactorialHastaDesde 1 n --buscamos el menor factorial mayor/igual a n partiendo desde i!


--EJERCICIO 8

{-|recive un int y devuelve un bool según si el input es un factorial o no-}
esFact :: Int -> Bool
esFact n = mayorFactorialHasta n == menorFactorialDesde n --si son iguales es porque ambos son n, entonces n es un factorial



--EJERCICIO 9

{-|devuelve el n-esimo número de fibonacci-}
nEsimoFibonacci :: Int -> Int
nEsimoFibonacci 0 = 0
nEsimoFibonacci 1 = 1
nEsimoFibonacci n = nEsimoFibonacci (n-1) + nEsimoFibonacci (n-2)

{-|recibe un número y te da un bool según sea o no un fibo-}
esFibonacci :: Int -> Bool
esFibonacci n = esFiboDesde n 0 --nos fijamos desde el fibo 0 si algún fibo coincide con n

{-|busca desde el fibo número f, si hay algún fibo que sea igual a n -}
esFiboDesde :: Int -> Int -> Bool
esFiboDesde n f | n == nEsimoFibonacci f = True --si encontramos uno igual, n es un fibo
                | n  > nEsimoFibonacci f = esFiboDesde n (f+1) --si n es más grande que el fibo que probamos recién probemos con uno más
                | n  < nEsimoFibonacci f = False --si el fibo que probamos recién se pasó es porque o n no es un fibo, o n es un fibo menor al fibo f


--EJERCICIO 10

{-|dado un numero entero n ≥ 0 decide si n es igual a la suma de los m primeros numeros primos, para algun m-}
esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde n 0


{-|recibe un n y un p y se fija si n es la suma de los primeros p primos, si no lo es prueba con p+1 y así hasta
que se asegura que lo es o no, devuelve un bool en base a eso-}
esSumaInicialDePrimosDesde :: Int -> Int -> Bool
esSumaInicialDePrimosDesde n p | n == sumaInicialDePrimosHasta p = True --si n es la suma de los primeros p primos da true
                               | n >  sumaInicialDePrimosHasta p = esSumaInicialDePrimosDesde n (p+1) --si n es más chica, probemos sumando un primo más
                               | n <  sumaInicialDePrimosHasta p = False --si n es más chico que la suma de los primeros p primos es porque nos pasamos (o porque arrancamos muy adelante) 


{-|suma los primeros p primos-}
sumaInicialDePrimosHasta :: Int -> Int
sumaInicialDePrimosHasta p | p == 0    = 0
                           | otherwise = nEsimoPrimo p + sumaInicialDePrimosHasta (p-1)



--EJERCICIO 11
{-|dado un n1 >=1 y un n2 >= n1, devuelve el m entre n1 y n2 con la mayor suma de divisores-}
tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 = mayorSumaDivisores n1 n1 n2


mayorSumaDivisores :: Int -> Int -> Int -> Int
mayorSumaDivisores n1 n n2 | (n-1) == n2                         = n1 --si n se pasó de n2 es porque ya probamos todos y n tiene la mayor suma posible
                           | sumaDivisores n1 >= sumaDivisores n = mayorSumaDivisores n1 (n+1) n2 --si n1 tiene una mayor suma probamos con n+1
                           | sumaDivisores n1 <  sumaDivisores n = mayorSumaDivisores n (n+1) n2 --si n tiene una mayor suma que n, n toma el lugar de n1 y empezamos a probar con los que están entre n y n2


--EJERCICIO 12
{-|dado un n1 >=1 y un n2 >= n1, devuelve el m entre n1 y n2 con la menor suma de divisores-}
tomaValorMin :: Int -> Int -> Int
tomaValorMin n1 n2 = menorSumaDivisores n1 n1 n2


menorSumaDivisores :: Int -> Int -> Int -> Int
menorSumaDivisores n1 n n2 | (n-1) == n2                         = n1 --si n se pasó de n2 es porque ya probamos todos y n tiene la menor suma posible
                           | sumaDivisores n1 <= sumaDivisores n = menorSumaDivisores n1 (n+1) n2 --si n1 tiene una menor suma probamos con n+1
                           | sumaDivisores n1 >  sumaDivisores n = menorSumaDivisores n (n+1) n2 --si n tiene una menor suma que n, n toma el lugar de n1 y empezamos a probar con los que están entre n y n2



--EJERCICIO 13 

{-|cuenta cuantos pares de primos gemelos menores a n hay. Dos primos son gemelos si b = a+2-}
primosGem :: Int -> Int
primosGem n = primosGemDesde n 1 0 --buscamos primos gemelos desde el primer primo hasta el mayor que sea menor a n

primosGemDesde :: Int -> Int -> Int -> Int --vamos desde el p-esimo primo hasta n, el número de gemelos es g
primosGemDesde n p g | n < nEsimoPrimo p           = g --porque b <= n, entonces terminamos y devuelve g, todos los gemelos encontrados
                     | esPrimo ((nEsimoPrimo p)-2) = primosGemDesde n (p+1) (g+1) -- b = a + 2, con b primo, si a es primo suma un gemelo y prueba con el siguiente primo (el siguiente b)
                     | otherwise                   = primosGemDesde n (p+1) (g) --si no se cumplió lo anterior vamos con el siguiente primo sin sumar un gemelo



--EJERCICIO 14

{-|busca el primer par de primos gemelos mayor a n-}
proxPrimosGem :: Int -> (Int,Int)
proxPrimosGem n = buscadorDeGem n 1 --arranca desde el primer primo


buscadorDeGem :: Int -> Int -> (Int,Int)
buscadorDeGem n p | n >= nEsimoPrimo p          = buscadorDeGem n (p+1) -- el primo a tiene que ser mayor a n, si no lo es buscamos el primo siguiente hasta que uno lo sea
                  | esPrimo ((nEsimoPrimo p)+2) = (nEsimoPrimo p, ((nEsimoPrimo p)+2)) --ya con a>n, probamos si a+2 (b) es primo, si es primo ahí tenemos nuestro primer par
                  | otherwise                   = buscadorDeGem n (p+1) -- si no es primo vamos con el siguiente primo en el lugar de a



--EJERCICIO 15 A

{-|ejecuta un paso de la conjetura de Lothar en n-}
lothar :: Int -> Int
lothar n | n`mod`2 == 0 = n `div` 2 --si n es par, n/2
         | otherwise    = 3*n + 1 --si n es impar, 3n+1

{-|devuelve la cantidad de veces que hay que aplicarle lothar a un n para llegar a 1-}
largoSecuencia :: Int -> Int
largoSecuencia n | n == 1        = 0 --si n es 1 hay que hacer cero pasos para llegar a 1
                 | otherwise     = largoSecuencia (lothar n) + 1 --si n no era 1, contamos 1 paso y le hacemos lothar a n


--EJERCICIO 15 B

{-|recibe n1, n y n2. n1 es el menor número con el que tiene que probar, va a empezar a comparar a partir de n hasta llegar a n2. 
Compara quién tiene la secuencia de Lothar más larga-}
mayorLargoSecuencia :: Int -> Int -> Int -> Int
mayorLargoSecuencia n1 n n2 | (n-1) == n2                           = n1 --si n-1 ==n2 es porque en la vuelta anterior comparamos con n2
                            | largoSecuencia n1 >= largoSecuencia n = mayorLargoSecuencia n1 (n+1) n2 --si gana n1 probamos con n1 vs n+1
                            | largoSecuencia n1 <  largoSecuencia n = mayorLargoSecuencia n (n+1) n2 --si n le gana a n1, probamos con n vs n+1

--para encontrar el número menor a 10 mil con la secuencia de Lothar más grande, ejecutar mayorLargoSecuencia 1 1 10mil.
--si hay dos que tienen el mismo largo va a devolver el menor de los dos números
--La respuesta es 6171 con 261 pasos







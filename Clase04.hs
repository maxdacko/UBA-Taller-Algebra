--sumatorias


sumatoria :: Int -> Int  
sumatoria 0 = 0
sumatoria n = n + sumatoria (n - 1)

sumatoria2 :: Int -> Int
sumatoria2 n = n * (n + 1) `div` 2

sumadiv :: Int -> Int 
sumadiv n = div (n * (n + 1)) 2

-- |||||||||||||||||||||ejercicio 1

sum1 :: Int -> Int
sum1 0 = 1
sum1 n = 2 ^ n + sum1_2 (n - 1)

sum1_2 :: Int -> Int 
sum1_2 n | n == 0 = 1
         | otherwise = 2 ^ n + sum1_2 (n - 1)
--sin recursion
sum1_3 :: Int -> Int
sum1_3 n = 2 ^ (n + 1) - 1
        
-- ||||||||||||||||||||| ejercicio 2

s2 :: Int -> Float -> Float
s2 0 q = 0
s2 n q = q ^ n + s2 (n - 1) q

-- ||||||||||||||||||||| ejercicio 3

s3 :: Int -> Float -> Float
s3 0 q = 0
s3 n q = (s3 (n - 1) q) + q ^ (2 * n - 1) + q ^ (2 * n)

--usando s2
s3_2 :: Int -> Float -> Float
s3_2 n q = s2 (2*n) q



-- |||||||||||||||||||||| ejercicio 4

s4 :: Int -> Float -> Float
s4 0 q = 1
s4 n q = q ^ (2 * n - 1) + q ^  (2 * n) - q ^ (n - 1) + (s4 (n - 1) q)


s42 :: Int -> Float -> Float
s42 n q = (s3 n q) - (s2 (n - 1) q)



-- ||||||   func e aprox

fact :: Int -> Int
fact 1 = 1
fact n = n * (fact (n - 1))

eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = eAprox (n - 1) + 1 / (fromIntegral (fact n))

-- |||||| definir e con terminos de eaprox

e :: Float 
e = eAprox 10





--  sumatorias dobles con nxm terminos
--1
sdoble :: Int -> Int -> Int
sdoble 0 m = 0
sdoble n m = (sdoble (n - 1) m) + round (s2 m (fromIntegral n))
--2
sp :: Float -> Int -> Int -> Float
sp q n 0 = 0
sp q n m = (sp q n (m - 1)) + q ^ m * (s2 n q)
--3
sr :: Int -> Int -> Float
sr n 0 = 0
sr n m = (sr n (m - 1)) + (fromIntegral (sumatoria n)) / (fromIntegral m)


--tarea:

g1 :: Integer -> Integer -> Integer
g1 i n 
  | n < i = 0
  | i == n = i^n                 
  | otherwise = i^n + g1 i (n-1)

g2 :: Integer -> Integer
g2 n = interna n n  
    where interna 0 _ = 0 
          interna i n = g1 i n + interna (i-1) n


g3 :: Integer -> Integer
g3 0 = 0
g3 n 
  | mod n 2 == 0 = 2^(n) + g3 (n-1)
  | otherwise = g3 (n-1)


todosIguales :: Integer -> Bool
todosIguales n = interna n /= -1
    where interna x 
            | div i 10 == 0 = i
            | mod i 10 == interna (div i 10) = mod i 10
            | otherwise = -1 --
            where i = abs x


sumaIguales :: Integer -> Integer
sumaIguales 0 = 0
sumaIguales n 
  | todosIguales n = n + sumaIguales (n-1)
  | otherwise = sumaIguales (n-1)
{-
prodInt :: (Float, Float) -> (Float, Float) -> (Float)
prodInt (a,b) (c,d) = a*c+b*d

decenas :: Int -> Int
decenas x   | x >= 10   = div ((mod x 100) - (mod x 10)) 10
            | otherwise = error "Decenas menor que 10"

{-
dejar espacios entre todos los operadores-}

decc x = div x 10 `mod` 10

prod x y = x*y
-}

{-
susuma v w = ((fst v) + (fst w) , (snd v) + (snd w))
suma (vx, vy) (wx, wy) = (vx + wx, vy + wy)
normaVect1 (x, y) = sqrt ( x^2 + y^2 )
normaSuma1 v1 v2 =normaVect1 (suma v1 v2)
-}
{-
rela :: Float->Float->Bool  
rela x y |   3 < x && x <= 7 && 3 < y && y <= 7  = True
         |   x <= 3 && y <= 3 = True
         |   x > 7 && y > 7 = True
         |   otherwise = False


sumaTerma :: Int -> Int -> Int -> Int 
sumaTerma x y z = x + y + z


crearPar a b = (a, b)

invertir (a, b) = (b, a) 


posicPriPar :: (Integer, Integer) -> Integer
posicPriPar (a, b) | mod a 2 == 0 = a
                   | mod b 2 == 0 = b
                   | otherwise = 4
-}
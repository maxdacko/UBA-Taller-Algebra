module ModuloMedio2
where
    import ModuloBase
    import ModuloBase2
    import ModuloBase3
    
    maximo3 :: Int -> Int -> Int -> Int
    maximo3 x y z = ModuloBase.maximo (ModuloBase.maximo x y) z

    maximoCuadrado :: Float -> Float -> Float
    maximoCuadrado x y = ModuloBase3.maximo (x^2) (y^2)


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

susuma v w = ((fst v) + (fst w) , (snd v) + (snd w))
suma (vx, vy) (wx, wy) = (vx + wx, vy + wy)
normaVect1 (x, y) = sqrt ( x^2 + y^2 )
normaSuma1 v1 v2 =normaVect1 (suma v1 v2)
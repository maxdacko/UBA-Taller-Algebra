type Polinomio  = [Float] --No arranca con 0
type Monomio    = (Float, Int) --El coef es /= 0
type Racional   = (Int, Int) --Fracciones irreducibles, denominador pocitivo
type PolinomioZ = [Int] --Z[x]
type Set a      = [a]

{-|Dada una lista cualquiera de n´umeros reales, le borra los 0s iniciales si los hubiera-}
limpiar :: [Float] -> Polinomio
limpiar (0:xs) = limpiar xs
limpiar   p    = p


grado :: Polinomio -> Int
grado   []   = undefined
grado  [x]   = 0
grado (x:xs) = 1 + grado xs


evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar p  x = (head p) * x^n + evaluar (limpiar (tail p)) x
    where
        n = grado p

suma :: Polinomio -> Polinomio -> Polinomio
suma p q = limpiar (sumaAux p q)
    where        
        sumaAux :: Polinomio -> Polinomio -> Polinomio
        sumaAux [] q = q
        sumaAux p [] = p
        sumaAux p q  = sumaAux (init p) (init q) ++ [(last p) + (last q)]


productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar 0 _  = []
productoPorEscalar a [] = []
productoPorEscalar a p  = a*(head p) : productoPorEscalar a (tail p) 


resta :: Polinomio -> Polinomio -> Polinomio
resta p q = suma p (productoPorEscalar (-1) q)


productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a,0) p = productoPorEscalar a p
productoPorMonomio (a,n) p = (productoPorMonomio (a, (n-1)) p) ++ [0]


producto :: Polinomio -> Polinomio -> Polinomio
producto [] q = []
producto p  q = productoPorMonomio (head p, grado p) q `suma` producto (tail p) q


hacerPolinomio :: Monomio -> Polinomio
hacerPolinomio (a,n) = a : (ceros n)
    where
        ceros :: Int -> [Float]
        ceros 0 = []
        ceros n = 0 : (ceros (n-1))


derivadaMonomio :: Monomio -> Polinomio
derivadaMonomio (a,n) = hacerPolinomio (a*(fromIntegral n),(n-1))


derivada :: Polinomio -> Polinomio
derivada [x] = []
derivada  p  = derivadaMonomio ((head p),(grado p)) `suma` derivada (tail p)


derivadaNEsima :: Polinomio -> Int -> Polinomio
derivadaNEsima p 1 = derivada p
derivadaNEsima p n = derivadaNEsima (derivada p) (n-1)


{-|Calcula el primer monomio del cociente de la division de dos polinomios-}
primerCociente :: Polinomio -> Polinomio -> Monomio
primerCociente p q | grado p >= grado q = ((head p)/(head q), (grado p) - (grado q)) 
                   | otherwise          = (0,0)


primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto p q = p `resta` productoPorMonomio (primerCociente p q) q


division :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
division p [] = undefined
division [] q = ([],[])
division p  q 
    | grado p < grado q = ([],p)
    | otherwise         = (hacerPolinomio (primerCociente p q) `suma` c', r') 
        where
            (c',r') = division (primerResto p q) q



mcdPNM :: Polinomio -> Polinomio -> Polinomio
mcdPNM p [] = p
mcdPNM p q  = mcdPNM q (snd (division p q))


hacerMonico :: Polinomio -> Polinomio
hacerMonico [] = []
hacerMonico p  = productoPorEscalar (1/(head p)) p


{-|Algo euclides mcd entre polinomios-}
mcdP :: Polinomio -> Polinomio -> Polinomio
mcdP p q = hacerMonico (mcdPNM p q)



{-|Dado un real x y un polinomio P(x) determina la multiplicidad de x como raíz de P-}
multiplicidad :: Float -> Polinomio -> Int
multiplicidad x p | r' == []  = 1 + multiplicidad x c'
                  | otherwise = 0
    where
        (c',r') = division p [1,-x]


{-|determina si un polinomio tiene raices multiples-}
raicesMultiples :: Polinomio -> Bool
raicesMultiples [] = True -- ?? no estoy seguro que hacer con el vacio
raicesMultiples p | mcdP p p' /= [1] = True
                  | otherwise       = False
    where
        p' = derivada p

--Esto es porque una raiz de p con multiplicidad mayor a 1 tmb lo será de p'
--Esto implica que aparecera como factor en ambas factorizaciones
--Entonces el mcd entre p y p' no sería de 1
--Esto es porque aparecerian las raices de multiplicidad > 1



--Segunda parte

armaR :: Int -> Int -> Racional
armaR num den 
    | den == 0  = undefined
    | den < 0   = armaR (-num) (-den) 
    | otherwise = (div num d, div den d)
        where
            d = mcd num den


mcd :: Int -> Int -> Int
mcd a 0 = abs a
mcd a b = mcd b (a `mod` b)


sumaR :: Racional -> Racional -> Racional
sumaR (a,b) (c,d) = armaR (a*d+c*d) (b*d)


multiplicaR :: Racional -> Racional -> Racional
multiplicaR (a,b) (c,d) = armaR (a*c) (b*d)


potenciaR :: Racional -> Int -> Racional
potenciaR _ 0 = (1,1)
potenciaR r n = multiplicaR (potenciaR r (n-1)) r


evaluarZ :: PolinomioZ -> Racional -> Racional
evaluarZ [] _ = (0,1)
evaluarZ p  x = multiplicaR (head p, 1) (potenciaR x n) `sumaR` evaluarZ (limpiarZ (tail p)) x
    where
        n = gradoZ p


limpiarZ :: [Int] -> PolinomioZ
limpiarZ (0:xs) = limpiarZ xs
limpiarZ   p    = p


gradoZ :: PolinomioZ -> Int
gradoZ   []   = undefined
gradoZ  [x]   = 0
gradoZ (x:xs) = 1 + gradoZ xs



esRaizRacional :: PolinomioZ -> Racional -> Bool
esRaizRacional p x = evaluarZ p x == (0,1)
                   

raicesRacEnConjunto :: PolinomioZ -> Set Racional -> Set Racional
raicesRacEnConjunto _   [] = []
raicesRacEnConjunto p (x:xs) 
    | esRaizRacional p x = x : (raicesRacEnConjunto p xs)
    | otherwise          =      raicesRacEnConjunto p xs



divisores :: Int -> Set Int
divisores n = divisoresHasta n (abs n)
    where
        divisoresHasta :: Int -> Int -> Set Int
        divisoresHasta n 1 = [1,-1]
        divisoresHasta n k | n `mod` k == 0 = [k,-k] ++ divisoresHasta n (k-1)
                           | otherwise      =           divisoresHasta n (k-1)


divisoresPos :: Int -> Set Int
divisoresPos n = divisoresPosHasta n (abs n)
    where
        divisoresPosHasta :: Int -> Int -> Set Int
        divisoresPosHasta n 1 = [1]
        divisoresPosHasta n k | n `mod` k == 0 = k : divisoresPosHasta n (k-1)
                              | otherwise      =     divisoresPosHasta n (k-1)


agregarRac :: Racional -> Set Racional -> Set Racional
agregarRac r c | r `elem` c = c
               | otherwise  = r:c


{-|Los hace de tipo Racional y se fija que no haya repetidos-}
armarSetRac :: Set (Int, Int) -> Set Racional
armarSetRac       []       = []
armarSetRac ((num,den):xs) = agregarRac (armaR num den) (armarSetRac xs)


candidatosRaices :: PolinomioZ -> Set Racional
candidatosRaices [] = undefined
candidatosRaices p 
    | last p == 0 = agregarRac (0,1) (candidatosRaices (init p))
    | otherwise   = armarSetRac (productoCartesiano (divisores (last p)) (divisoresPos (head p)))


productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano    _     []   = []
productoCartesiano   []     _    = []
productoCartesiano (x:xs) (y:ys) = (productoCartesianoAux (x) (y:ys)) ++ (productoCartesiano xs (y:ys)) --prod cartesiano del primer elemento con y, sumo el resto recursivamente
    where
        productoCartesianoAux :: Int -> Set Int -> Set (Int, Int) --hace todas las cobinaciones de un elemento con un conjunto
        productoCartesianoAux _   []   = [] 
        productoCartesianoAux x (y:ys) = (x,y) : productoCartesianoAux x ys


raicesRacionales :: PolinomioZ -> Set Racional
raicesRacionales p = raicesRacEnConjunto p (candidatosRaices p)
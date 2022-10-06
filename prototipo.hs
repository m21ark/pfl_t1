-- Trabalho 1 PFL

{- OBJETIVOS

            a) normalizar polinómios
            b) adicionar polinómios
            c) multiplicar polinómios
            d) calcular a derivada de um polinómio
            e) parsing string <-> polinomios

-}


-- Types definition
type Monomio = ((Int, [Int]), String) -- 3yx^2 = ([3,], 2, "xy")
type Polinomio = [Monomio]




============================ <UTILITIES> ============================
-}


-- Get exponent in monomio
monoExp ::  Monomio -> Int
monoExp m = snd $ fst m

-- Get coeficient in monomio
monoCoef ::  Monomio -> Int
monoCoef m = fst $ fst m

-- Get variable in monomio
monoVar ::  Monomio -> String
monoVar m = snd m


-- Filtra monimios nulos de polinomio
noZeroCoef :: Polinomio -> Polinomio 
noZeroCoef poli = filter (\mono -> 0 /= monoCoef mono) poli 


{-
============================ <UTILITIES> ============================
-}











{-
============================ <PARSE> ============================
-}

-- Convert string (0*x^2 + 2*y + 5*z + y + 7*y^2) to Polinomio type
-- [((0,2), "x"),((2,1), "y") , ((5,1), "z"), ((1,1), "y"), ((7,2), "y")]
parser :: String -> Polinomio
parser str = [((0,2), "x")] -- TO-DO

{-
============================ </PARSE> ============================
-}













{-
============================ </NORMALIZE> ============================
-}


normalize :: Polinomio -> Polinomio 
normalize poli = noZeroCoef poli


{-
============================ </NORMALIZE> ============================
-}














{-
============================ </SUM> ============================
-}

-- Retorna 2 monomios se incompativeis ou 1 monimio resultado
sumMono :: Monomio -> Monomio -> Polinomio 
sumMono m1 m2
    | eqVar && eqExp = [((sumCoef , monoExp m1), monoVar m1)]
    | otherwise = [m1,m2]
    where 
        eqVar = ((monoVar m1) == (monoVar m2))
        eqExp = ((monoExp m1) == (monoExp m2))
        sumCoef = (monoCoef m1) + (monoCoef m2) 
        
     


{-
============================ </SUM> ============================
-}


















{-
============================ <DERIVE> ============================
-}

-- Derivar monomios simples
deriveMono ::  Monomio -> Monomio
deriveMono m = ((newCoef, exp - 1), monoVar m)
    where
        exp = (monoExp m) 
        newCoef = (monoCoef m) *exp

-- Derivar polinomios simples
derivePoli :: Polinomio -> Polinomio  
derivePoli poli = noZeroCoef $ map deriveMono poli       

{-
============================ </DERIVE> ============================
-}









   

-- main = print $ sumMono ((4,5), "x") ((9,5), "x")



-- main = print $ derivePoli $ [((0,2), "x"),((2,1), "y") , ((5,1), "z"), ((1,1), "y"), ((7,2), "y")]

--main = print $ normalize $ [((0,2), "x"),((2,1), "y") , ((5,1), "z"), ((1,1), "y"), ((7,2), "y")]







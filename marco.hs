-- Trabalho 1 PFL

{- OBJETIVOS
		        a) normalizar polinómios
		        b) adicionar polinómios
		        c) multiplicar polinómios
		        d) calcular a derivada de um polinómio	X
		        e) parsing string <-> polinomios
-}

import Data.List
-- replace item at pos N with nem ITEM in list LS

replaceAtIndex n item ls
    | length ls <= n = error "Out of bounds replaceAtIndex"
    | otherwise = a ++ (item:b) where (a, (_:b)) = splitAt n ls


-- ======================================================FUNCOES BASICAS AUX===============================================================

-- Types definition
type Monomio = ((Int, [Int]), String) -- 3yx^2 = ([3,], 2, "xy")
type Polinomio = [Monomio]


-- Get exponent in monomio
monoExp ::  Monomio -> [Int]
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

-- =======================================================PARSING DE POLI -> STR=============================================================

poliParseToStr ::  Polinomio -> String 
poliParseToStr poli =  if result == "" then "0" else result
    where result = concat $  intersperse " + " $ map monoParseToStr $ noZeroCoef poli 
    
-- isto poe '+' entre cada monomio formatado mas se for negativo temos de ver dps...

monoParseToStr::  Monomio -> String 
monoParseToStr m =  (if (monoCoef m) /= 1 then show (monoCoef m) else "")  ++ together
    where
        aux = zip (monoExp m) (monoVar m)
        together =  concat $ intersperse "*" [var : (if exp /= 1 then "^" ++ show exp else "") | (exp, var) <- aux ]


-- ======================================================DERIVE POLINOMIALS========================================================

monoContainsVar :: Monomio -> Char -> Bool
monoContainsVar mono var = elem var $ monoVar mono


-- Derivar polinomio dado em ordem a char dado
derivePoli :: Polinomio -> Char ->  Polinomio
derivePoli poli dx = noZeroCoef $ map (\mono -> deriver mono dx) poli 


deriver :: Monomio -> Char -> Monomio
deriver m dx = if monoContainsVar m dx then deriveMono m dx else ((0,[1]),"x")


deriveMono :: Monomio -> Char -> Monomio
deriveMono m dx = (((monoCoef m) * exp, exponents), (monoVar m))
    where
        aux = zip (monoVar m) (monoExp m) -- (var, exp)
        aux2 =  (zip [0..] aux) -- (index, (var, exp))
        auxElem = foldr auxFunc (-1, (' ',-1)) aux2
        (index, exp) = (fst auxElem, snd (snd auxElem)) -- index e expoente atual da derivada em questao
        exponents = replaceAtIndex index (exp - 1) (monoExp m) -- reduz expoente da derivada em questao
        auxFunc e acc = if (fst (snd e)) == dx then e else acc -- procura o elemento com as infos uteis


-- ====================================================================================================================================



main = putStr $ poliParseToStr [((1,[3,4]),"yx")] 















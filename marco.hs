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

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls
    | length ls <= n = error "Out of bounds replaceAtIndex"
    | otherwise = a ++ (item:b) where (a, (_:b)) = splitAt n ls


-- ======================================================FUNCOES BASICAS AUX===============================================================

-- Types definition
type Monomio = ((Int, [Int]), String) -- 3yx^2 = ([3,], 2, "xy")
type Polinomio = [Monomio]
-- constantes sao representadas ((3 ,[0]),"ç") onde 3 é a constante!


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

-- testa se monomio possui variavel dada
monoContainsVar :: Monomio -> Char -> Bool
monoContainsVar mono var = elem var $ monoVar mono

-- =======================================================PARSING DE POLI -> STR=============================================================

-- Passa polinomio para string
poliParseToStr ::  Polinomio -> String 
poliParseToStr poli =  if result == "" then "0" else result
    where result = concat $  intersperse " + " $ map monoParseToStr $ noZeroCoef poli 
    
-- isto poe '+' entre cada monomio formatado mas se for negativo temos de ver dps...

-- passa monomio para string
monoParseToStr::  Monomio -> String 
monoParseToStr m =  coefShow  ++ (if together /= "ç^0" then together else "")
    where
        aux = zip (monoExp m) (monoVar m)
        together =  concat $ intersperse "*" [[var] ++ expShow exp | (exp, var) <- aux , exp /= 0 ]
        expShow exp = (if exp /= 1 then "^" ++ show exp else "")
        coefShow = (if (monoCoef m) /= 1 then show (monoCoef m) else "")


-- ======================================================DERIVE POLINOMIALS========================================================

-- Derivar polinomio dado em ordem a char dado
derivePoli :: Polinomio -> Char ->  Polinomio
derivePoli poli dx = noZeroCoef $ map (\mono -> deriver mono dx) poli 


deriver :: Monomio -> Char -> Monomio
deriver m dx = if monoContainsVar m dx then deriveMono m dx else ((0,[1]),"ç") -- ç representa uma constante

-- isto ja assume q monomio esta normalizado ent pode falhar se n estiver na forma C * X^a * Y^b ...
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


main :: IO ()
main = do
    putStrLn $ poliParseToStr [((1,[3,4]),"ax"),((1,[3,4]),"xy"),((2,[0]),"ç")]
    putStrLn $ poliParseToStr [((1,[3,4]),"ax"),((1,[3,4]),"xy"),((2,[0]),"ç")]















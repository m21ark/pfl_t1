-- Trabalho 1 PFL

{- OBJETIVOS
		        a) normalizar polinómios
		        b) adicionar polinómios
		        c) multiplicar polinómios
		        d) calcular a derivada de um polinómio	X
		        e) parsing string <-> polinomios
-}

{-  Ideias extras: (nao sei se valorizam...) --> perguntar ao stor cm funciona o metodo de avaliacao

Adicionar exponencialização de polinomio
permitir escolher derivada de n-esimo grau

-}

import Data.List -- to use splitAt
import Data.Char -- to use ord

-- replace item at pos N with nem ITEM in list LS
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls
    | length ls <= n = error "Out of bounds replaceAtIndex"
    | otherwise = a ++ (item:b) where (a, (_:b)) = splitAt n ls


-- Basicamente transforma lista num set sorted e volta a lista --> talvez\ podiamos usar Data.Set se n fizer mal
rmvdups :: (Ord a) => [a] -> [a]
rmvdups = map head . group . sort

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


-- ======================================================= PARSING DE STR -> POLI =======================================================





-- ======================================================= PARSING DE POLI -> STR =======================================================

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


-- ====================================================== SUM POLIS ============================================================

-- TODO Falta agr apenas aplicar esta soma de monomios pares a pares em todo o polinomio...

sumMonoCompatible :: Monomio -> Monomio -> Bool
sumMonoCompatible m1 m2 = (monoVar m1 == monoVar m2) || (monoCoef m1 == monoCoef m2)

-- Testar primeiro se dá para somar com sumMonoCompatible para evitar erro
sumMono :: Monomio -> Monomio -> Monomio
sumMono m1 m2
    | not $ sumMonoCompatible m1 m2 = error "incompatible sum!"
    | otherwise = (( (monoCoef m1) + (monoCoef m2) , monoExp m1), monoVar m1)


-- ====================================================== MULTIPLY POLIS ============================================================


-- Aplica a distributiva entre cada par de monomios dos polinomios: (m1 + m2 + m3 ...) * (m'1 + m'2 + m'3 ...)
multPoli :: Polinomio -> Polinomio -> Polinomio
multPoli p1 p2 = [multMono (p1!!x) (p2!!y) | (x,y)<-genPairs]
    where
        genPairs = [(x,y) | x<-[0..(length p1)-1], y<-[0..(length p2)-1]] 
        
-- (2a^3*x^4 + 5x^3*y^4) * (2a^3*x^4 + 5x^3*y^4) = 4a^6*x^8 + 10a^3*x^7*y^4 + 10a^3*x^7*y^4 + 25x^6*y^8


-- (5 x^2 y^3)  *  (7 y^2 z^5) = 35 x^2 y^5 z^5
multMono :: Monomio -> Monomio -> Monomio
multMono m1 m2 =  ((coef, exp), vars)
    where
        coef = (monoCoef m1) * (monoCoef m2)
        vars = rmvdups $ (monoVar m1) ++ (monoVar m2)
        exp = [(findVarExp c m1) + (findVarExp c m2) | c <- vars ] -- ((5,[2,3]),"xy") ((7,[2,5]),"yz") 
        findVarExp c m = sum [e | (v,e) <- aux , v == c] 
            where aux = zip (monoVar m) (monoExp m)
            
-- ====================================================== DERIVE POLINOMIALS ============================================================

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
        auxFunc e acc = if (fst (snd e)) == dx then e else acc -- procura o elemento com as infos uteis
        (index, exp) = (fst auxElem, snd (snd auxElem)) -- index e expoente atual da derivada em questao
        exponents = replaceAtIndex index (exp - 1) (monoExp m) -- reduz expoente da derivada em questao


-- ====================================================================================================================================


main :: IO ()
main = do
    putStrLn $ poliParseToStr $ derivePoli  (multPoli [((2,[3,4]),"ax"),((5,[3,4]),"xy")] [((2,[3,4]),"ax"),((5,[3,4]),"xy")]) 'x' 









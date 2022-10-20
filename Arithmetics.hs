module Arithmetics where -- Este ficheiro contem as funções de Normalizar, Somar, Multiplicar, Derivar polinomios

import Data.List -- to use splitAt, interspace

-- Types definition
type Monomio = ((Int, [Int]), String) -- 3yx^2 = ([3,], 2, "xy")
type Polinomio = [Monomio]

-- replace item at pos N with nem ITEM in list LS
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls
    | length ls <= n = error "Out of bounds replaceAtIndex"
    | otherwise = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- Basicamente transforma lista num set sorted e volta a lista --> talvez podiamos usar Data.Set se n fizer mal
rmvdups :: (Ord a) => [a] -> [a]
rmvdups = map head . group . sort

-- Get exponent in monomio
monoExp ::  Monomio -> [Int]
monoExp m = snd $ fst m

-- Get coeficient in monomio
monoCoef ::  Monomio -> Int
monoCoef m = fst $ fst m

-- Get variable in monomio
monoVar ::  Monomio -> String
monoVar m = snd m

-- ====================================================== NORMALIZE ============================================================

-- Filtra monimios nulos de polinomio -->
noZeroCoef :: Polinomio -> Polinomio 
noZeroCoef poli = filter (\mono -> 0 /= monoCoef mono) poli 

-- testa se monomio possui variavel dada
monoContainsVar :: Monomio -> Char -> Bool
monoContainsVar mono var = elem var $ monoVar mono

cleanZeroExp :: [Int] -> String -> ([Int], String) 
cleanZeroExp a b = ([z | z <- a, z /= 0], [snd x | x <- zip a b, fst x /= 0]) 

-- Remove monomios que tenham coeficiente nulo
noZeroExp :: Polinomio -> Polinomio
noZeroExp p = [((fst (fst x), fst y), snd y) | x <- p, let y = cleanZeroExp (snd (fst x)) (snd x) ]
                
-- Verifica o tamanho dos expoentes nos monomios
checkGreaterExp :: [Int] -> [Int] -> Bool
checkGreaterExp [] [] = False
checkGreaterExp (x:xs) (y:ys) | x <  y = False
                              | x >  y = True
                              | otherwise = checkGreaterExp xs ys

-- Ordena monomios por grau
monoSort :: Monomio -> Monomio -> Ordering
monoSort a b | snd a > snd b = LT 
             | snd a < snd b = GT
             | checkGreaterExp  (monoExp a) (monoExp b) = GT
             | otherwise = LT

-- Verifica ordem dos monomios por variaveis             
monoSortVar :: Ord x => (a, x) -> (a, x) -> Ordering
monoSortVar x b | snd x < snd b = LT
                | otherwise = GT

-- Ordena monomios pelas variaveis que têm
monoSortVars :: Monomio -> Monomio
monoSortVars x = ((monoCoef x, a), b)
                    where (a, b) = unzip [s | s <- sortBy (monoSortVar) (zip (monoExp x) (monoVar x))]  

-- Normaliza monomios com base nos graus e variáveis que possuem
normaliseVars :: Monomio -> Monomio
normaliseVars p =  ((monoCoef p, exps), vars)
                    where z = groupBy (\a b -> snd a == snd b)  [x | x <- zip (monoExp p) (monoVar p)] 
                          exps = [sum [fst e | e <- ex] | ex <- z] 
                          vars = [ snd (var !! 0) | var <- z]
                          
-- Normaliza polinomios verificando coeficientes nulos, ordenando os monomios por grau e variaveis que possuem
normPoli :: Polinomio -> Polinomio
normPoli p =   sumPoli_ . (sortBy monoSort) . noZeroExp . noZeroCoef $ [normaliseVars . monoSortVars $ x | x <- p]

-- Passa polinomio normalizado para string
poliParseToStr ::  Polinomio -> String 
poliParseToStr poli =  if result == "" then "0" else result
    where result = concat $  intersperse " + " $ map monoParseToStr $ noZeroCoef poli 
    
-- isto poe '+' entre cada monomio formatado mas se for negativo temos de ver dps cm fazer...

-- passa monomio para string
monoParseToStr::  Monomio -> String 
monoParseToStr m =  coefShow  ++ (if together /= "ç^0" then together else "")
    where
        aux = zip (monoExp m) (monoVar m)
        together =  concat $ intersperse "*" [[var] ++ expShow exp | (exp, var) <- aux , exp /= 0 ]
        expShow exp = (if exp /= 1 then "^" ++ show exp else "")
        coefShow = (if ((monoCoef m) /= 1) || ((null (monoVar m))) then show (monoCoef m) else "")


-- ====================================================== SUM ============================================================

-- Verifica se a soma entre 2 monomios é possivel com base nas variaveis e respetivos expoentes
sumMonoCompatible :: Monomio -> Monomio -> Bool
sumMonoCompatible m1 m2 = (monoVar m1 == monoVar m2) && (monoExp m1 == monoExp m2)

-- Testar primeiro se dá para somar com sumMonoCompatible para evitar chamar o erro de prevencao
sumMono :: Monomio -> Monomio -> Monomio
sumMono m1 m2
    | not $ sumMonoCompatible m1 m2 = error "incompatible sum!"
    | otherwise = (( (monoCoef m1) + (monoCoef m2) , monoExp m1), monoVar m1)
    
-- Pega num polinomio normaliado q é a mistura/soma de dois e agora junta as parcelas possiveis de juntar
sumPoli_ :: Polinomio -> Polinomio 
sumPoli_ [] = []
sumPoli_  (m1:[]) = [m1]
sumPoli_  (m1:ps)
    | sumMonoCompatible m1 m2 = sumPoli_ ( res : tail ps) 
    | otherwise = m1 : sumPoli_ ps
    where 
        m2 = head ps
        res = (sumMono m1 m2)
    
-- Isto junta os dois polinomios num, mas dps isto precisa levar um parse para dps sim chamar sumPoli_ q necessita de receber input ja normalizado
sumPoli :: Polinomio -> Polinomio -> Polinomio
sumPoli p1 p2 = {- sumPoli $ parse $ -} p1 ++ p2


-- ====================================================== MULTIPLY ============================================================

-- Aplica a distributiva entre cada par de monomios dos polinomios: (m1 + m2 + m3 ...) * (m'1 + m'2 + m'3 ...)
multPoli :: Polinomio -> Polinomio -> Polinomio
multPoli p1 p2 = [multMono (p1!!x) (p2!!y) | (x,y)<-genPairs]
    where
        genPairs = [(x,y) | x<-[0..(length p1)-1], y<-[0..(length p2)-1]]  -- acho q da para tirar a var genPairs e por td numa linha

-- Multiplica dois monomios
multMono :: Monomio -> Monomio -> Monomio
multMono m1 m2 =  ((coef, exp), vars)
    where
        coef = (monoCoef m1) * (monoCoef m2)
        vars = rmvdups $ (monoVar m1) ++ (monoVar m2)
        exp = [(findVarExp c m1) + (findVarExp c m2) | c <- vars ] -- ((5,[2,3]),"xy") ((7,[2,5]),"yz") 
        findVarExp c m = sum [e | (v,e) <- aux , v == c] 
            where aux = zip (monoVar m) (monoExp m)


-- ====================================================== DERIVE ============================================================

-- Derivar polinomio dado em ordem a char dado. Vai derivar monomio a monomio
derivePoli :: Polinomio -> Char ->  Polinomio
derivePoli poli dx = noZeroCoef $ map (\mono -> deriver mono dx) poli 

-- Função auxiliar para poupar tempo. Se aparecer um monomio sem a variavel a qual estamos a derivar, poe logo a constante zero
deriver :: Monomio -> Char -> Monomio
deriver m dx = if monoContainsVar m dx then deriveMono m dx else ((0,[1]),"ç") -- ç representa uma constante


-- isto ja assume q monomio esta normalizado ent pode falhar se n estiver na forma C * X^a * Y^b ...
-- Deriva monomio --> esta funcao esta um pouco confusa mas da para simplificar consideravelmente dps
deriveMono :: Monomio -> Char -> Monomio
deriveMono m dx = (((monoCoef m) * exp, exponents), (monoVar m))
    where
        aux = zip (monoVar m) (monoExp m) -- (var, exp)
        aux2 =  (zip [0..] aux) -- (index, (var, exp))
        auxElem = foldr auxFunc (-1, (' ',-1)) aux2
        auxFunc e acc = if (fst (snd e)) == dx then e else acc -- procura o elemento com as infos uteis
        (index, exp) = (fst auxElem, snd (snd auxElem)) -- index e expoente atual da derivada em questao
        exponents = replaceAtIndex index (exp - 1) (monoExp m) -- reduz expoente da derivada em questao

{-# LANGUAGE Safe #-}
module Arithmetics where -- Este ficheiro contem as funções de Normalizar, Somar, Multiplicar, Derivar polinomios

import Data.List -- to use splitAt, interspace

-- | Types definition
type Monomio = ((Int, [Int]), String) 
type Polinomio = [Monomio]

-- | replace item at pos N with nem ITEM in list LS
replaceAtIndex :: Int -- ^ index of
               -> a   -- ^ item to replace with
               -> [a] -- ^ list of items to replace at
               -> [a] -- ^ list of items according to the new state
replaceAtIndex n item ls
    | length ls <= n = error "Out of bounds replaceAtIndex"
    | otherwise = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- | Basicamente transforma lista num set sorted e volta a lista --> talvez podiamos usar Data.Set se n fizer mal
rmvdups :: (Ord a)
        => [a]     -- ^ the list of items to be sorted
        -> [a]     -- ^ list of items sorted
rmvdups = map head . group . sort

-- | Get exponent in monomio
monoExp ::  Monomio -- ^ Monomio 
        -> [Int]    -- ^ list of exponents
monoExp m = snd $ fst m

-- | Get coeficient in monomio
monoCoef ::  Monomio -- ^ Monomio
         -> Int      -- ^ Coeficient of the monomio  
monoCoef m = fst $ fst m

-- | Get variable in monomio
monoVar ::  Monomio -- ^ Monomio
        -> String   -- ^ The vars of the monomio
monoVar m = snd m

-- ====================================================== NORMALIZE ============================================================

-- | Filtra monimios nulos de polinomio -->
noZeroCoef :: Polinomio -- ^ Polinomio
           -> Polinomio -- ^ Polinomio with no zero coef
noZeroCoef poli = filter (\mono -> 0 /= monoCoef mono) poli 

-- | testa se monomio possui variavel dada
monoContainsVar :: Monomio -- ^ Monomio
                -> Char    -- ^ Var
                -> Bool    -- ^ Has var then true else false
monoContainsVar mono var = elem var $ monoVar mono

-- | Removes monomios with zero exponents
cleanZeroExp :: [Int]           -- ^ List of exponents
             -> String          -- ^ Variables            
             -> ([Int], String) -- ^ resulting (exponents, variables)
cleanZeroExp a b = ([z | z <- a, z /= 0], [snd x | x <- zip a b, fst x /= 0]) 

-- | Remove monomios que tenham coeficiente nulo
noZeroExp :: Polinomio -- ^ list of monomios
          -> Polinomio -- ^ List resulted from removing null exponents
noZeroExp p = [((fst (fst x), fst y), snd y) | x <- p, let y = cleanZeroExp (snd (fst x)) (snd x) ]
                
-- | Verifica o tamanho dos expoentes nos monomios
checkGreaterExp :: [Int]    -- ^ list of exponents
                   -> [Int] -- ^ list of exponents
                   -> Bool  -- ^ True if left is greater than right in exponents
checkGreaterExp [] [] = False
checkGreaterExp _ [] = True
checkGreaterExp [] _ = False
checkGreaterExp (x:xs) (y:ys) | x <  y = False
                              | x >  y = True
                              | otherwise = checkGreaterExp xs ys

-- | Ordena monomios por grau
monoSort :: Monomio  -- ^ Monomio
         -> Monomio  -- ^ Monomio
         -> Ordering -- ^ True if left is greater than the right monomios exponents
monoSort a b | snd a > snd b = LT 
             | snd a < snd b = GT
             | checkGreaterExp  (monoExp a) (monoExp b) = GT
             | otherwise = LT

-- | Verifica ordem dos monomios por variaveis             
monoSortVar :: Ord x => 
            (a, x)      -- ^ (exponents, variables)
            -> (a, x)   -- ^ (exponents, variables)
            -> Ordering -- ^ True if left is greater than the right monomios variables
monoSortVar x b | snd x < snd b = LT
                | otherwise = GT

-- | Ordena monomios pelas variaveis que têm
monoSortVars :: Monomio -- ^ Monomio
             -> Monomio -- ^ Monomio sortBy its variables
monoSortVars x = ((monoCoef x, a), b)
                    where (a, b) = unzip [s | s <- sortBy (monoSortVar) (zip (monoExp x) (monoVar x))]  

-- | Normaliza monomios com base nos graus e variáveis que possuem
normaliseVars :: Monomio -- ^ Monomio
              -> Monomio -- ^ Monomio normalised by Vars coef etc...
normaliseVars p =  ((monoCoef p, exps), vars)
                    where z = groupBy (\a b -> snd a == snd b)  [x | x <- zip (monoExp p) (monoVar p)] 
                          exps = [sum [fst e | e <- ex] | ex <- z] 
                          vars = [ snd (var !! 0) | var <- z]
                          
-- | Normaliza polinomios verificando coeficientes nulos, ordenando os monomios por grau e variaveis que possuem
normPoli :: Polinomio -- ^ list of monomios
         -> Polinomio -- ^ normalised list of monomios
normPoli p =   sumPoli_ . (sortBy monoSort) . noZeroExp . noZeroCoef $ [normaliseVars . monoSortVars $ x | x <- p]

-- | Passa polinomio normalizado para string
poliParseToStr ::  Polinomio -- ^ polinomio
               -> String     -- ^ representation of a polinomio in string
poliParseToStr poli =  if result == "" then "0" else result
    where result = concat $  intersperse " + " $ map monoParseToStr $ noZeroCoef poli 
    
-- isto poe '+' entre cada monomio formatado mas se for negativo temos de ver dps cm fazer...

-- | passa monomio para string
monoParseToStr::  Monomio -- ^ Monomio
              -> String   -- ^ The representation of a monomio in string
monoParseToStr m =  coefShow  ++ (if together /= "ç^0" then together else "")
    where
        aux = zip (monoExp m) (monoVar m)
        together =  concat $ intersperse "*" [[var] ++ expShow exp_ | (exp_, var) <- aux , exp_ /= 0 ]
        expShow exp_ = (if exp_ /= 1 then "^" ++ show exp_ else "")
        coefShow = (if ((monoCoef m) /= 1) || ((null (monoVar m))) then show (monoCoef m) else "")


-- ====================================================== SUM ============================================================

-- | Verifica se a soma entre 2 monomios é possivel com base nas variaveis e respetivos expoentes
sumMonoCompatible :: Monomio -- ^ Monomio
                  -> Monomio -- ^ Monomio
                  -> Bool    -- ^ True if its possible to add
sumMonoCompatible m1 m2 = (monoVar m1 == monoVar m2) && (monoExp m1 == monoExp m2)

-- | Testar primeiro se dá para somar com sumMonoCompatible para evitar chamar o erro de prevencao
sumMono :: Monomio -- ^ Monomio
        -> Monomio -- ^ Monomio
        -> Monomio -- ^ Resulting of adding the 2 monomios
sumMono m1 m2
    | not $ sumMonoCompatible m1 m2 = error "incompatible sum!"
    | otherwise = (( (monoCoef m1) + (monoCoef m2) , monoExp m1), monoVar m1)
    
-- | Pega num polinomio normaliado q é a mistura/soma de dois e agora junta as parcelas possiveis de juntar
sumPoli_ :: Polinomio -- ^ Polinomio
         -> Polinomio -- ^ Resulting of adding the monomios of the polinomio
sumPoli_ [] = []
sumPoli_  (m1:[]) = [m1]
sumPoli_  (m1:ps)
    | sumMonoCompatible m1 m2 = sumPoli_ ( res : tail ps) 
    | otherwise = m1 : sumPoli_ ps
    where 
        m2 = head ps
        res = (sumMono m1 m2)
    
-- | Isto junta os dois polinomios num, mas dps isto precisa levar um parse para dps sim chamar sumPoli_ q necessita de receber input ja normalizado
sumPoli :: Polinomio -- ^ Polinomio
        -> Polinomio -- ^ Polinomio
        -> Polinomio -- ^ Resulting of the addition of the polinomios
sumPoli p1 p2 = {- sumPoli $ parse $ -} p1 ++ p2

subPoli :: Polinomio -- ^ Polinomio
        -> Polinomio -- ^ Polinomio
        -> Polinomio -- ^ Resulting of the subtraction of the polinomios
subPoli p1 p2 = sumPoli_ (p1 ++ (multPoli [((-1,[]),[])] p2))
-- ====================================================== MULTIPLY ============================================================

-- | Aplica a distributiva entre cada par de monomios dos polinomios: (m1 + m2 + m3 ...) * (m'1 + m'2 + m'3 ...)
multPoli :: Polinomio -- ^ Polinomio
         -> Polinomio -- ^ Polinomio
         -> Polinomio -- ^ Resulting of the multiplication of the polinomios
multPoli p1 p2 = [multMono (p1!!x) (p2!!y) | (x,y)<-genPairs]
    where
        genPairs = [(x,y) | x<-[0..(length p1)-1], y<-[0..(length p2)-1]]  -- acho q da para tirar a var genPairs e por td numa linha

-- | Multiplica dois monomios
multMono :: Monomio -- ^ Monomio
         -> Monomio -- ^ Monomio
         -> Monomio -- ^ Resulting of multiplying the 2 monomios
multMono m1 m2 =  ((coef, exp_), vars)
    where
        coef = (monoCoef m1) * (monoCoef m2)
        vars = rmvdups $ (monoVar m1) ++ (monoVar m2)
        exp_ = [(findVarExp c m1) + (findVarExp c m2) | c <- vars ] -- ((5,[2,3]),"xy") ((7,[2,5]),"yz") 
        findVarExp c m = sum [e | (v,e) <- aux , v == c] 
            where aux = zip (monoVar m) (monoExp m)


-- ====================================================== DERIVE ============================================================

-- | Derivar polinomio dado em ordem a char dado. Vai derivar monomio a monomio
derivePoli :: Polinomio  -- ^ Polinomio
           -> Char       -- ^ deriving variable
           ->  Polinomio -- ^ Resulting of deriving the polinomio by the variable char
derivePoli poli dx = noZeroCoef $ map (\mono -> deriver mono dx) poli 

-- | Função auxiliar para poupar tempo. Se aparecer um monomio sem a variavel a qual estamos a derivar, poe logo a constante zero
deriver :: Monomio -- ^ Monomio
        -> Char    -- ^ deriving variable
        -> Monomio -- ^ 0 if it doesn't exist the variable in the monomio coef else returns the deriveMono result
deriver m dx = if monoContainsVar m dx then deriveMono m dx else ((0,[1]),"ç") -- ç representa uma constante


-- isto ja assume q monomio esta normalizado ent pode falhar se n estiver na forma C * X^a * Y^b ...
-- | Deriva monomio --> esta funcao esta um pouco confusa mas da para simplificar consideravelmente dps
deriveMono :: Monomio -- ^ Monomio
           -> Char    -- ^ deriving variable
           -> Monomio -- ^ Resulting of deriving the Monomio by the variable char
deriveMono m dx = (((monoCoef m) * exp_, exponents), (monoVar m))
    where
        aux = zip (monoVar m) (monoExp m) -- (var, exp)
        aux2 =  (zip [0..] aux) -- (index, (var, exp))
        auxElem = foldr auxFunc (-1, (' ',-1)) aux2
        auxFunc e acc = if (fst (snd e)) == dx then e else acc -- procura o elemento com as infos uteis
        (index, exp_) = (fst auxElem, snd (snd auxElem)) -- index e expoente atual da derivada em questao
        exponents = replaceAtIndex index (exp_ - 1) (monoExp m) -- reduz expoente da derivada em questao

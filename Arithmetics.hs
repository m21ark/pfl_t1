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

-- | Removes duplicates from a list
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

-- | Removes monomios with 0 coeficient
noZeroCoef :: Polinomio -- ^ Polinomio
           -> Polinomio -- ^ Polinomio with no zero coef
noZeroCoef poli = filter (\mono -> 0 /= monoCoef mono) poli 

-- | Checks if the monomio contains vars
monoContainsVar :: Monomio -- ^ Monomio
                -> Char    -- ^ Var
                -> Bool    -- ^ Has var then true else false
monoContainsVar mono var = elem var $ monoVar mono

-- | Cleans monomios with zero exponents
cleanZeroExp :: [Int]           -- ^ List of exponents
             -> String          -- ^ Variables            
             -> ([Int], String) -- ^ resulting (exponents, variables)
cleanZeroExp a b = ([z | z <- a, z /= 0], [snd x | x <- zip a b, fst x /= 0]) 

-- | Cleans monomios with zero exponents belonging to a polinomio
noZeroExp :: Polinomio -- ^ list of monomios
          -> Polinomio -- ^ List resulted from removing null exponents
noZeroExp p = [((fst (fst x), fst y), snd y) | x <- p, let y = cleanZeroExp (snd (fst x)) (snd x) ]
                
-- | Checks who gots the greater exponent from the two list
checkGreaterExp :: [Int]    -- ^ list of exponents
                   -> [Int] -- ^ list of exponents
                   -> Bool  -- ^ True if left is greater than right in exponents
checkGreaterExp [] [] = False
checkGreaterExp _ [] = True
checkGreaterExp [] _ = False
checkGreaterExp (x:xs) (y:ys) | x <  y = False
                              | x >  y = True
                              | otherwise = checkGreaterExp xs ys

-- | Checks the monomio greater. First check the variables and then the exponent
monoSort :: Monomio  -- ^ Monomio
         -> Monomio  -- ^ Monomio
         -> Ordering -- ^ True if left is greater than the right monomios exponents
monoSort a b | snd a > snd b = LT 
             | snd a < snd b = GT
             | checkGreaterExp (monoExp b) (monoExp a) = GT -- If the variables are equal then we want sorted with descending order
             | otherwise = LT

-- | Checks the monomio greater. According to the variables     
monoSortVar :: Ord x => 
            (a, x)      -- ^ (exponents, variables)
            -> (a, x)   -- ^ (exponents, variables)
            -> Ordering -- ^ True if left is greater than the right monomios variables
monoSortVar x b | snd x < snd b = LT
                | otherwise = GT

-- | Sorts the monomios variables
monoSortVars :: Monomio -- ^ Monomio
             -> Monomio -- ^ Monomio sortBy its variables
monoSortVars x = ((monoCoef x, a), b)
                    where (a, b) = unzip [s | s <- sortBy (monoSortVar) (zip (monoExp x) (monoVar x))]  

-- | Normalizes the monomio variables
normaliseVars :: Monomio -- ^ Monomio
              -> Monomio -- ^ Monomio normalised by Vars coef etc...
normaliseVars p =  ((monoCoef p, exps), vars)
                    where z = groupBy (\a b -> snd a == snd b)  [x | x <- zip (monoExp p) (monoVar p)] 
                          exps = [sum [fst e | e <- ex] | ex <- z] 
                          vars = [ snd (var !! 0) | var <- z]
                          
-- | Normalizes polinomios taking into account the variables, zero coef and exponents, and making appropriate sums.
normPoli :: Polinomio -- ^ list of monomios
         -> Polinomio -- ^ normalised list of monomios
normPoli p =   sumPoli_ . (sortBy monoSort) . noZeroExp . noZeroCoef $ [normaliseVars . monoSortVars $ x | x <- p]

-- =============================================== PARSING TO STRING =====================================================

-- | Parses to string a polinomio representation
poliParseToStr ::  Polinomio -- ^ polinomio
                -> String     -- ^ representation of a polinomio in string
poliParseToStr poli =  if result == "" then "0" else result
    where 
        result = firstElem ++ (auxF $ tail $ strL)
        strL = map monoParseToStr $ noZeroCoef poli 
        firstElem = if (head (head strL)) == '-' then "-" ++ (tail (head strL))  else (head strL)
        auxF [] = ""
        auxF (x:xs) =  (if (head x) /='-' then " + " else " ") ++ x ++ (auxF xs)


-- | Parses monomio to string 
monoParseToStr::  Monomio -- ^ Monomio
              -> String   -- ^ The representation of a monomio in string
monoParseToStr m =  coefShow  ++ (if together /= "ç^0" then together else "")
    where
        aux = zip (monoExp m) (monoVar m)
        together =  concat $ intersperse "*" [[var] ++ expShow exp_ | (exp_, var) <- aux , exp_ /= 0 ]
        expShow exp_ = if exp_ /= 1 then "^" ++ show exp_ else ""

        coefShow = if (monoCoef m) == -1 then "- " ++ vars else negativeCoef
        
        vars = if (monoVar m) == "" then "1" else ""
        
        negativeCoef = if (monoCoef m) < -1 then "- " ++ show (-(monoCoef m))  else positiveCoef
        positiveCoef = if ((monoCoef m) /= 1) || ((null (monoVar m))) then show (monoCoef m) else ""
        
-- ====================================================== SUM ============================================================

-- | Verifies if a sum is possible between two monomios
sumMonoCompatible :: Monomio -- ^ Monomio
                  -> Monomio -- ^ Monomio
                  -> Bool    -- ^ True if its possible to add
sumMonoCompatible m1 m2 = (monoVar m1 == monoVar m2) && (monoExp m1 == monoExp m2)

-- | Sums monomios if it's possible
sumMono :: Monomio -- ^ Monomio
        -> Monomio -- ^ Monomio
        -> Monomio -- ^ Resulting of adding the 2 monomios
sumMono m1 m2
    | not $ sumMonoCompatible m1 m2 = error "incompatible sum!"
    | otherwise = (( (monoCoef m1) + (monoCoef m2) , monoExp m1), monoVar m1)
    
-- | Makes the appropriate sums, if any, within a polinomio
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
    
-- | Sums to polinomios, returning the resulted one.
sumPoli :: Polinomio -- ^ Polinomio
        -> Polinomio -- ^ Polinomio
        -> Polinomio -- ^ Resulting of the addition of the polinomios
sumPoli p1 p2 = {- sumPoli $ parse $ -} p1 ++ p2

-- | substracts two polinomios
subPoli :: Polinomio -- ^ Polinomio
        -> Polinomio -- ^ Polinomio
        -> Polinomio -- ^ Resulting of the subtraction of the polinomios
subPoli p1 p2 = sumPoli_ (p1 ++ (multPoli [((-1,[]),[])] p2))

-- ====================================================== MULTIPLY ============================================================

-- | Applies the distribuitive between two polinomios : (m1 + m2 + m3 ...) * (m'1 + m'2 + m'3 ...)
multPoli :: Polinomio -- ^ Polinomio
         -> Polinomio -- ^ Polinomio
         -> Polinomio -- ^ Resulting of the multiplication of the polinomios
multPoli p1 p2 = [multMono (p1!!x) (p2!!y) | (x,y)<-genPairs]
    where
        genPairs = [(x,y) | x<-[0..(length p1)-1], y<-[0..(length p2)-1]]  -- acho q da para tirar a var genPairs e por td numa linha

-- | Muliplies two monomios
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

-- | Derives the polinomio for the given char var
derivePoli :: Polinomio  -- ^ Polinomio
           -> Char       -- ^ deriving variable
           ->  Polinomio -- ^ Resulting of deriving the polinomio by the variable char
derivePoli poli dx = noZeroCoef $ map (\mono -> deriver mono dx) poli 

-- | Helper Function that automatically put a zero coef if the monomio doesn't have the char for wich we derive
deriver :: Monomio -- ^ Monomio
        -> Char    -- ^ deriving variable
        -> Monomio -- ^ 0 if it doesn't exist the variable in the monomio coef else returns the deriveMono result
deriver m dx = if monoContainsVar m dx then deriveMono m dx else ((0,[1]),"ç") -- ç representa uma constante


-- | Derives a monomio for the variable given 
deriveMono :: Monomio -- ^ Monomio
           -> Char    -- ^ deriving variable
           -> Monomio -- ^ Resulting of deriving the Monomio by the variable char
deriveMono m dx = if exp_ == -1 then ((0,[]),"") else (((monoCoef m) * exp_, exponents), (monoVar m))
    where
        exp_aux = zip (monoVar m) (monoExp m) -- [(var, exp)]
        index_aux = zip (monoVar m) [0..] -- [(var, exp)]
        exp_ = unwrapper $ lookup dx exp_aux
        index = unwrapper $ lookup dx index_aux
        exponents = replaceAtIndex index (exp_ - 1) (monoExp m)   -- reduz expoente da derivada em questao
        unwrapper x = case x of 
                        Just n  -> n
                        Nothing -> -1
                        
-- | Makes a pow of the given polinomio for the given integer
powPoli :: Polinomio -- ^ polinomio 
        -> Int       -- ^ the power
        -> Polinomio -- ^ the polinomio resulting from the pow
powPoli p x | x == 0 =  [((1, []),"")]
            | x > 0  = normPoli  $ (foldr  (multPoli) p [p | i <- [2..x], i > 0]) 
            | otherwise = error "Not valid in the context of a polinomio"

{-# LANGUAGE TemplateHaskell #-}

module Prop_tests where

import Test.QuickCheck
import Arithmetics

-- Auxiliar funcs for testing
valid_mono :: Monomio -> Bool
valid_mono m = (length (monoVar m)) == (length (monoExp m))  && (length (monoExp m)) > 0

valid_poli :: Polinomio -> Bool
valid_poli p = and $ map valid_mono p

-- ================================================== SUM ==================================================

prop_associativity_sum :: Polinomio -> Polinomio -> Bool
prop_associativity_sum p1 p2 = if (not (valid_poli p1)) || (not (valid_poli p2)) then True else sumPoli p1 p2 == sumPoli p2 p1

prop_null_element_sum :: Polinomio -> Bool
prop_null_element_sum p1 = normPoli (sumPoli p1 [((0,[1]),"ç")]) == normPoli p1

-- ================================================== MULT ==================================================

prop_coef_mult :: Monomio -> Monomio -> Bool
prop_coef_mult m1 m2 = (monoCoef m1)*(monoCoef m2) == (monoCoef (multMono m1 m2))

prop_associativity_mult :: Polinomio -> Polinomio -> Bool
prop_associativity_mult p1 p2 = if (not (valid_poli p1)) || (not (valid_poli p2)) then True else multPoli p1 p2 == multPoli p2 p1

prop_null_element_mult :: Polinomio -> Bool
prop_null_element_mult p1 = if (not (valid_poli p1)) then True else multPoli p1 [((1,[]),"")] == p1

-- ================================================== DIFF ==================================================

prop_const_deriv :: Polinomio -> Char -> Bool
prop_const_deriv p1 c =  if (not (valid_poli p1)) || (not (elem c (concat (map monoVar p1)))) then True 
   else normPoli ( derivePoli p1 c ) == normPoli [((0,[1]),"ç")]


-- End of testing file
return []
check = $quickCheckAll


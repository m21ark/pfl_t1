-- Trabalho 1 PFL

{- OBJETIVOS
		        a) normalizar polinómios                        TO-DO
		        b) adicionar polinómios                         DONE?
		        c) multiplicar polinómios                       DONE?
		        d) calcular a derivada de um polinómio	        DONE?
		        e) parsing string <-> polinomios                HALF DONE -> FALTA STR -> POLI
-}

{-  Ideias extras: (nao sei se valorizam...) --> perguntar ao stor cm funciona o metodo de avaliacao

Adicionar exponencialização de polinomio
permitir escolher derivada de n-esimo grau
adicionar subtracoes


-}

import Data.List -- to use splitAt
import Data.Char -- to use ord
import qualified Data.Map as Map
import System.IO

-- replace item at pos N with nem ITEM in list LS
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls
    | length ls <= n = error "Out of bounds replaceAtIndex"
    | otherwise = a ++ (item:b) where (a, (_:b)) = splitAt n ls


-- Basicamente transforma lista num set sorted e volta a lista --> talvez podiamos usar Data.Set se n fizer mal
rmvdups :: (Ord a) => [a] -> [a]
rmvdups = map head . group . sort

-- ======================================================FUNCOES BASICAS AUX===============================================================

-- Types definition
type Monomio = ((Int, [Int]), String) -- 3yx^2 = ([3,], 2, "xy")
type Polinomio = [Monomio]

-- constantes sao representadas ((3 ,[0]),"ç") onde 3 é a constante!
-- mas pelos meus testes tmb n faz mal usar como constante qualquer variavel em vez de 'ç'

-- Get exponent in monomio
monoExp ::  Monomio -> [Int]
monoExp m = snd $ fst m

-- Get coeficient in monomio
monoCoef ::  Monomio -> Int
monoCoef m = fst $ fst m

-- Get variable in monomio
monoVar ::  Monomio -> String
monoVar m = snd m


-- Filtra monimios nulos de polinomio -->
noZeroCoef :: Polinomio -> Polinomio 
noZeroCoef poli = filter (\mono -> 0 /= monoCoef mono) poli 

-- testa se monomio possui variavel dada
monoContainsVar :: Monomio -> Char -> Bool
monoContainsVar mono var = elem var $ monoVar mono


-- ======================================================= PARSING DE STR -> POLI =======================================================


-- TODO A funcao noZeroCoef ja faz um inicio de parsing basico de remover monomios com coef nulo e q é usado em varias funcoes ...


-- ======================================================= NORMALIZAR =======================================================

-- TODO Normalizar inclui por por ordem decrescente de expoentes e ordem alfabetica de variaveis 
-- Expoentes de 0 transforma em 1 -- nozerocoef

-- normMono :: Monomio -> Monomio
-- normMono m =   noZeroCoef 

-- (sortby ()) . (sumPoli_) . (groupby (monoCoef)) . noZeroCoef 

cleanZeroExp :: [Int] -> String -> ([Int], String) 
cleanZeroExp a b = ([z | z <- a, z /= 0], [snd x | x <- zip a b, fst x /= 0]) 

-- main = print (cleanZero [4,0,1] "xyz") - a dar 
noZeroExp :: Polinomio -> Polinomio
noZeroExp p = [((fst (fst x), fst y), snd y) | x <- p, let y = cleanZeroExp (snd (fst x)) (snd x) ]
                
-- main = print (noZeroExp [((0,[0]),"x")]) -- WORKS ::: SEE CASE

-- main = print (noZeroExp ([((0,[3]),"x"),((2,[1]),"y"), ((5,[1]),"z"), ((1,[1]),"y"), ((7,[2,0]),"zy")  ]))

checkGreaterExp :: [Int] -> [Int] -> Bool
checkGreaterExp [] [] = False
checkGreaterExp (x:xs) (y:ys) | x <  y = False
                              | x >  y = True
                              | otherwise = checkGreaterExp xs ys


monoSort :: Monomio -> Monomio -> Ordering
monoSort a b | snd a > snd b = LT 
             | snd a < snd b = GT
             | checkGreaterExp  (monoExp a) (monoExp b) = GT
             | otherwise = LT

monoSortVar :: Ord x => (a, x) -> (a, x) -> Ordering
monoSortVar x b | snd x < snd b = LT
                | otherwise = GT

monoSortVars :: Monomio -> Monomio
monoSortVars x = ((monoCoef x, a), b)
                    where (a, b) = unzip [s | s <- sortBy (monoSortVar) (zip (monoExp x) (monoVar x))]  

normaliseVars :: Monomio -> Monomio
normaliseVars p =  ((monoCoef p, exps), vars)
                    where z = groupBy (\a b -> snd a == snd b)  [x | x <- zip (monoExp p) (monoVar p)] 
                          exps = [sum [fst e | e <- ex] | ex <- z] 
                          vars = [ snd (var !! 0) | var <- z]
                          
--groupBy (\a b -> MonoVar a == MonoVar b)  
normPoli :: Polinomio -> Polinomio
normPoli p =   sumPoli_ . (sortBy monoSort) . noZeroExp . noZeroCoef $ [monoSortVars . normaliseVars $ x | x <- p]


-- main = print (normaliseVars ((0,[3,1,3,4]),"xxyy"))
-- main = print(poliParseToStr . normPoli $ ([((0,[3]),"x"),((2,[1, 1,1]),"zyx"), ((5,[1,2,4,3]),"zzyx"), ((1,[1,1,1]),"zxy"), ((7,[0]) ,"z")  ]))


--vars = rmvdups $ (monoVar m1) ++ (monoVar m2)
--exp = [(findVarExp c m1) + (findVarExp c m2) | c <- vars ]

            -- TODO :: decompor com mais funções

-- (map sumPoli (groupBy (\a b-> snd a == snd b)))
--- (groupBy (\a b -> monoCoef b == monoCoef a)) 


-- ======================================================= PARSING DE POLI -> STR =======================================================

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
        coefShow = (if (monoCoef m) /= 1 then show (monoCoef m) else "")


-- ====================================================== SUM POLIS ============================================================

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
    

-- 2x^3 + 3x^3 + 4x^2 + 8x + 5x + 17 + 2 = 5x^3 + 4x^2 + 13x+ 19 
-- [((2,[3]),"x"),((3,[3]),"x"),((4,[2]),"x"),((8,[1]),"x"),((5,[1]),"x"),((17,[0]),"x"),((2,[0]),"x")]

    
-- Isto junta os dois polinomios num, mas dps isto precisa levar um parse para dps sim chamar sumPoli_ q necessita de receber input ja normalizado
sumPoli :: Polinomio -> Polinomio -> Polinomio
sumPoli p1 p2 = {- sumPoli $ parse $ -} p1 ++ p2


-- ====================================================== MULTIPLY POLIS ============================================================


-- Aplica a distributiva entre cada par de monomios dos polinomios: (m1 + m2 + m3 ...) * (m'1 + m'2 + m'3 ...)
multPoli :: Polinomio -> Polinomio -> Polinomio
multPoli p1 p2 = [multMono (p1!!x) (p2!!y) | (x,y)<-genPairs]
    where
        genPairs = [(x,y) | x<-[0..(length p1)-1], y<-[0..(length p2)-1]]  -- acho q da para tirar a var genPairs e por td numa linha
        
-- (2a^3*x^4 + 5x^3*y^4) * (2a^3*x^4 + 5x^3*y^4) = 4a^6*x^8 + 10a^3*x^7*y^4 + 10a^3*x^7*y^4 + 25x^6*y^8
-- (multPoli [((2,[3,4]),"ax"),((5,[3,4]),"xy")] [((2,[3,4]),"ax"),((5,[3,4]),"xy")])


-- (5 x^2 y^3)  *  (7 y^2 z^5) = 35 x^2 y^5 z^5

-- Multiplica dois monomios
multMono :: Monomio -> Monomio -> Monomio
multMono m1 m2 =  ((coef, exp), vars)
    where
        coef = (monoCoef m1) * (monoCoef m2)
        vars = rmvdups $ (monoVar m1) ++ (monoVar m2)
        exp = [(findVarExp c m1) + (findVarExp c m2) | c <- vars ] -- ((5,[2,3]),"xy") ((7,[2,5]),"yz") 
        findVarExp c m = sum [e | (v,e) <- aux , v == c] 
            where aux = zip (monoVar m) (monoExp m)
            
-- ====================================================== DERIVE POLINOMIALS ============================================================

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


-- ====================================================================================================================================

-- 32a^6*x^7 + 70a^3*x^6*y^4 + 70a^3*x^6*y^4 + 150x^5*y^8
-- aux = derivePoli  (multPoli [((2,[3,4]),"ax"),((5,[3,4]),"xy")] [((2,[3,4]),"ax"),((5,[3,4]),"xy")]) 'x' 


-- 2x^3 + 3x^3 + 4x^2 + 8x + 5x + 17 + 2
-- aux3 = [((2,[3]),"x"),((3,[3]),"x"),((4,[2]),"x"),((8,[1]),"x"),((5,[1]),"x"),((17,[0]),"x"),((2,[0]),"x")]



-- usar putStrLn para melhor visualização nos poliParse, mas tmb da para usar print
--main :: IO ()
--main = do
  --  putStrLn $ poliParseToStr $ sumPoli_ [((2,[3]),"x"),((2,[3]),"x"),((2,[3]),"x"),((2,[3]),"x"),((3,[3]),"x"),((3,[3]),"x"),((3,[3]),"x"),((3,[3]),"x"),((4,[2]),"x"),((8,[1]),"x"),((5,[1]),"x"),((17,[0]),"x"),((2,[0]),"x"),((2,[0]),"x"),((17,[0]),"x"),((17,[0]),"x"),((17,[0]),"x"),((2,[0]),"x")]

data Expr = Add Expr Expr
          | Mult Expr Expr
          | Poli Polinomio
          deriving (Eq, Show)

eval :: Expr -> Polinomio 
eval (Poli n) = n
eval (Add e1 e2) = sumPoli_ ((eval e1) ++ (eval e2))
eval (Mult e1 e2) = multPoli (eval e1) (eval e2)

expression = "f"

-- isMono = isDigit -- TODO
-- 
-- --newtype Parser a = Parser { parse :: String -> [(a, String)] }
-- 
-- parse :: String -> (String, Expr)
-- parse [] = ("", Poli [((0, [1]), "x")])
-- parse (' ':more) = parse more
-- --parse ('-':more) = let (remainder, e) = parse more
-- --                   in (remainder, Sub e)
-- parse ('+':more) = let (remainder, e) = parse more
--                    in (remainder, Poli [((2, [1]), "x")])
-- parse ('*':more) = undefined -- TODO
-- parse s@(num:more) | isMono num = ("", ((snd . parse $ fs )) )
--                                         where x = parseMono $ s
--                                               fs = fst $ x 
--                                               sn = snd $ x
-- 
-- parse s = error ("unexpected input: " ++ s)

-- Se for monomio chamo o parse de monomio 

findValue :: (String, Int) -> (String, Int) 
findValue ([], exp) = ("", exp)
findValue (x:xs, exp) | isDigit x = findValue (xs, exp * 10 + digitToInt x)
                      | otherwise = (x:xs, exp)

parseMono :: (String, Monomio) -> (String, Monomio)
parseMono ([], monomio) = ("", monomio)
parseMono (s:m, mono) | isDigit s = parseMono (left , ((monoCoef mono * coef, monoExp mono), monoVar mono))
                        where r = findValue (m, digitToInt s)
                              coef = snd r
                              left = fst r 
parseMono (s:o:m, mono) | isLetter s && o == '^' = parseMono (left, ((monoCoef mono, monoExp mono ++ [exps]), monoVar mono ++ [s]))
                        | isLetter s = parseMono (o:m, ((monoCoef mono, monoExp mono ++ [1]), monoVar mono ++ [s]))
                        where r = findValue (m, 0)
                              exps = snd r
                              left = fst r
parseMono (s:m, mono) | isLetter s = parseMono (m, ((monoCoef mono, monoExp mono ++ [1]), monoVar mono ++ [s])) 
parseMono ('*':m, a) = parseMono (m, a)
parseMono (' ':m, a) = parseMono (m, a)
parseMono ('+':m, a) = (m , a)
parseMono ('-':m, a) = ('-':m , a)
--parseMono (_, a) = (_ , a)



parsePolo :: String -> Polinomio 
parsePolo "" = []
parsePolo (s:s') | s == '-' = [((- (monoCoef mono), monoExp mono), monoVar mono)] ++ parsePolo toParseStr
                where (toParseStr, mono) = parseMono (s', ((1, []), "")) -- é preciso meter o numero depois do -
parsePolo s =  [mono] ++ parsePolo toParseStr
                where (toParseStr, mono) = parseMono (s, ((1, []), ""))




--parseMono (s:m) | isVar s = (m , Poli [((digitToInt $ s, []), "")])


--parseStr :: String -> Polinomio
--parseStr str = runParser  (expression) str
--https://oliverbalfour.github.io/haskell/2020/08/09/parsing-arithmetic-with-monads.html

main :: IO ()
main   = do putStrLn "---------------------------WELCOME---------------------------"
            putStrLn "-------------------------------------------------------------"
            print . eval $ (Mult (Add (Poli [((1, [0]), "x")]) (Poli [((1, [0]), "x")])) (Poli [((2, [0]), "x")]))
           -- print . poliParseToStr . parseStr $ "fllf"
            print . snd . parseMono $ ("x^2 * 30x^2y^444z^2929y2", ((1, []), ""))
            print . isDigit $ '\n'
            print . parsePolo $ ("-2x^2 * 30x^2y^444z^2929y2 + x^2")
            
            



-- Nota para ricardo: as funcoes a receber polinomios com coeficientes negativos estao a dar mas tem alguns bugs menores  
-- Tmb n estou a fazer mtas verificacoes de input pq acho q isso pode ficar na funcao de normalização e assumir apenas q 
-- input das funcoes ja vem normalizado (caso da soma é o mais critico estar td ordenadinho)
-- dps para fazer output fazemos outra normalizacao para garantir ficar td direito
-- input com parse de STR para Polinomio --> normalizar / verificar --> operacoes (precisam input normalizado) --> normalizar output again --> output com parse para STR




-- https://hackage.haskell.org/package/computational-algebra-0.0.1.1/docs/Algebra-Ring-Polynomial-Parser.html
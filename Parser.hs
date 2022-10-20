module Parser where -- Ficheiro responsável pelo parsing do input para estrutura interna: String -> Polinomio

import Data.Char (isDigit, isSpace, isLetter)
import Control.Applicative -- to use "instance Alternative Parser"
import Arithmetics -- Program module
import Data.Char

data Expr = Add Expr Expr
          | Mult Expr Expr
          | Poli Polinomio
          | Pow Expr Char -- NOT POW CHANGE LATER 
          deriving (Eq, Show)

eval :: Expr -> Polinomio 
eval (Poli n) = n
eval (Add e1 e2) = sumPoli_ ((eval e1) ++ (eval e2))
eval (Mult e1 e2) = multPoli (eval e1) (eval e2)
eval (Pow e1 e2) = derivePoli (eval e1) e2

findValue :: (String, Int) -> (String, Int) 
findValue ([], exp_) = ("", exp_)
findValue (x:xs, exp_) | isDigit x = findValue (xs, exp_ * 10 + digitToInt x)
                      | otherwise = (x:xs, exp_)

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
parseMono (')':m, a) = (')':m, a)


parsePolo :: String -> Polinomio
parsePolo "" = []
parsePolo (s:_) | s == ')' = []
parsePolo (s:s') | s == '-' = [((- (monoCoef mono), monoExp mono), monoVar mono)] ++ parsePolo toParseStr
                where (toParseStr, mono) = parseMono (s', ((1, []), "")) -- é preciso meter o numero depois do -
parsePolo s =  [mono] ++ parsePolo toParseStr
                where (toParseStr, mono) = parseMono (s, ((1, []), ""))


--parseMono (s:m) | isVar s = (m , Poli [((digitToInt $ s, []), "")])

findMatchingParethesis :: String -> String
findMatchingParethesis (')':m) = m
findMatchingParethesis s = findMatchingParethesis $ tail s

parseStr :: String -> Expr
parseStr [] = Poli [((0, [0]), "")]
parseStr ('d':var:'(':xs) | isLetter var && (if left /=[] then head left else '0') == '*' = Mult (Pow (Poli . parsePolo $ xs) var ) (parseStr $ left)
                          | isLetter var && (if left /=[] then head left else '0') == '+' = Add (Pow (Poli . parsePolo $ xs) var ) (parseStr $ left)
                          | otherwise = Pow (Poli . parsePolo $ xs) var
                where left = findMatchingParethesis xs
parseStr (x:xs) | x == '(' && (if left /=[] then head left else '0') == '*' = Mult (Poli . parsePolo $ xs) (parseStr $ left)
                | x == '(' && (if left /=[] then head left else '0') == '+' = Add (Poli . parsePolo $ xs) (parseStr $ left)
                | x == '(' = Add (Poli . parsePolo $ xs) (parseStr left)
                | otherwise = parseStr xs
                where left = findMatchingParethesis xs

newtype Parser a = Parser { parse :: String -> [(a, String)] }

item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c,cs)])

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred_ = item >>= (\c -> if pred_ c then pure c else empty)

char :: Char -> Parser Char
char c = satisfy (== c)

instance Functor Parser where
  fmap f (Parser p) = Parser (\cs ->
    map (\(x, cs') -> (f x, cs')) (p cs))

isMono :: Char -> Bool
isMono x = isDigit x || isLetter x || '^' == x || '(' == x || '-' == x || '*'  == x 

digit :: Parser Int
digit = fmap (read . (:[])) (satisfy isDigit)

instance Applicative Parser where
  pure x = Parser (\cs -> [(x, cs)])
  f <*> a = Parser (\cs ->
    concat [parse (fmap fn a) cs' | (fn, cs') <- parse f cs])

instance Monad Parser where
  return = pure
  p >>= f = Parser (\cs ->
    concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance Alternative Parser where
  empty = Parser (\_ -> [])
  p <|> q = Parser (\cs ->
    let (p', q') = (parse p cs, parse q cs) in
    if length p' > 0 then p' else q')

space :: Parser String
space = many (satisfy isSpace)

string :: String -> Parser String
string "" = return ""
string (c:cs) = (:) <$> char c <*> string cs

token :: String -> Parser String
token symb = space *> string symb

mul :: Parser (Expr -> Expr -> Expr)
mul = token "*" *> pure Mult

add :: Parser (Expr -> Expr -> Expr)
add = token "+" *> pure Add 

--derive :: Parser (Expr -> Char -> Expr)
--derive = (token "d")  *> pure Pow

-- integer :: Parser Int
-- integer = let positive = fmap read (some (satisfy isDigit))
--           in space *> unary_minus positive

--neg :: Parser Expr -> Parser Expr
--neg = Expr []
--
--unary_minus :: Parser Expr -> Parser Expr
--unary_minus p = char '-' *> fmap neg p<|> p

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest a = ((op <*> pure a <*> p) >>= rest)  <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = p >>= rest
  where rest a =  (op <*> pure a <*> (p >>= rest)) <|> return a

----------------------------
-- Este código está um pouco sujo. Talvez tentar usar mais monads, parece ser mais simples de se ler.  
----------------------------
parsePolo2 :: String -> Expr 
parsePolo2 "" = Poli []
parsePolo2 (s:_) | s == ')' = Poli [] --- ver isto em mais detalhe para este caso do parser 
parsePolo2 ('d':var:'(':xs) | isLetter var = Pow (Poli . parsePolo $ xs) var  -- ISTO N DEVIA BEM ESTAR AQUI
parsePolo2 (s:s') | s == '-' = Poli ([((- (monoCoef mono), monoExp mono), monoVar mono)] ++ parsePolo toParseStr)
                where (toParseStr, mono) = parseMono (s', ((1, []), "")) -- é preciso meter o numero depois do - VER A NOTA ACIMA. O PARSING PODE SER UTILIZADO AQUI 
parsePolo2 s =  Poli ([mono] ++ parsePolo toParseStr)
                where (toParseStr, mono) = parseMono (s, ((1, []), ""))

polinomio :: Parser Expr
polinomio = space *> fmap parsePolo2 (some (satisfy isMono))
--polinomio = let positive = fmap read (some (satisfy isMono))
--          in space *> unary_minus positive

expr :: Parser Expr
expr = subexpr  `chainl1` mul `chainl1` add --`chainr1` derive 

subexpr :: Parser Expr
subexpr = token "(" *> expr <* token ")" <|> polinomio

repl :: String -> String
repl cs = let results = parse expr cs in
  case results of
    [] -> "Invalid expression"
    num -> (poliParseToStr . normPoli . eval  $ (fst (num !! 0)))
{-# LANGUAGE Safe #-}

module Parser where -- Ficheiro responsável pelo parsing do input para estrutura interna: String -> Polinomio

import Data.Char()
import Control.Applicative -- to use "instance Alternative Parser"
import Arithmetics -- Program module
import Data.Char

data Expr = Add Expr Expr
          | Mult Expr Expr
          | Poli Polinomio
          | Derive Expr Expr-- NOT POW CHANGE LATER 
          | Sub Expr Expr
          deriving (Eq)

-- | Show a representation of an evaluated expression in the form of a normalised Polinomial
instance Show Expr where
  show x = poliParseToStr . normPoli . eval $ x

newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- | Defines a functor for the Parser. This applies the funcion f to the first argument of (a, String), a, for each (a, String)
instance Functor Parser where
  fmap f (Parser p) = Parser (\cs ->
    map (\(x, cs') -> (f x, cs')) (p cs))

-- | Defines an Applicative for the Parser. It gives a list of (function, string) pairs, using fmap to apply the function and calling the modified parser with the remaining string 
instance Applicative Parser where
  pure x = Parser (\cs -> [(x, cs)])
  f <*> a = Parser (\cs ->
    concat [parse (fmap fn a) cs' | (fn, cs') <- parse f cs])

-- | Defines a Monad for the Parser. It applies the funcion f to the each unwrapped argument produced by the previous parser.
instance Monad Parser where
  return = pure
  p >>= f = Parser (\cs ->
    concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance Alternative Parser where
  empty = Parser (\_ -> [])
  p <|> q = Parser (\cs ->
    let (p', q') = (parse p cs, parse q cs) in
    if length p' > 0 then p' else q')

eval :: Expr -> Polinomio 
eval (Poli n) = n
eval (Add e1 e2) = sumPoli_ ((eval e1) ++ (eval e2))
eval (Mult e1 e2) = multPoli (eval e1) (eval e2)
eval (Derive e1 e2) = derivePoli (eval e1) (monoVar ((eval e2) !! 0) !! 0)
eval (Sub e1 e2) = subPoli (eval e1) (eval e2)

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
parseMono (_, _) = error "Invalid Char present in input"


parsePolo :: String -> Polinomio
parsePolo "" = [] 
parsePolo (s:s') | s == ')' = parsePolo s'
parsePolo s =  normPoli ([mono] ++ parsePolo toParseStr)
                where (toParseStr, mono) = parseMono (s, ((1, []), ""))

item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:c') -> [(c,c')])

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred_ = item >>= (\c -> if pred_ c then pure c else empty)

char :: Char -> Parser Char
char c = satisfy (== c)

isPoli :: Char -> Bool
isPoli x = isDigit x || isLetter x || '^' == x 

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
add = token "+" *> pure Add <|> token "-" *> pure Sub

derive :: Parser (Expr -> Expr -> Expr)
derive = token "'" *> pure Derive

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest a = ((op <*> pure a <*> p) >>= rest)  <|> return a

parseExpr :: String -> Expr 
parseExpr "" = Poli []
parseExpr (s:s') | s == ')' = Poli $ normPoli $ parsePolo $ s'
parseExpr s =  Poli $ normPoli ([mono] ++ parsePolo toParseStr)
                where (toParseStr, mono) = parseMono (s, ((1, []), ""))

polinomio :: Parser Expr
polinomio = space *> fmap parseExpr (some (satisfy isPoli))
 
expr :: Parser Expr
expr = subexpr `chainl1` derive `chainl1` mul `chainl1` add   

subexpr :: Parser Expr
subexpr = token "(" *> expr <* token ")" <|> polinomio

repl :: String -> String
repl cs = let results = parse expr cs in
  case results of
    [] -> "Invalid expression"
    num -> (show (fst (num !! 0)))
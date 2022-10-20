--module Arithmetic where

import Data.Char (isDigit, isSpace, isLetter)
import Control.Applicative
import Marco
import System.IO

newtype Parser a = Parser { parse :: String -> [(a, String)] }

item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c,cs)])

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = item >>= (\c -> if pred c then pure c else empty)

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
parsePolo2 (s:s') | s == ')' = Poli [] --- ver isto em mais detalhe para este caso do parser 
parsePolo2 ('d':var:'(':xs) | isLetter var = Pow (Poli . parsePolo $ xs) var  -- ISTO N DEVIA BEM ESTAR AQUI
parsePolo2 (s:s') | s == '-' = Poli ([((- (monoCoef mono), monoExp mono), monoVar mono)] ++ parsePolo toParseStr)
                where (toParseStr, mono) = parseMono (s', ((1, []), "")) -- é preciso meter o numero depois do - VER A NOTA ACIMA. O PARSING PODE SER UTILIZADO AQUI 
parsePolo2 s =  Poli ([mono] ++ parsePolo toParseStr)
                where (toParseStr, mono) = parseMono (s, ((1, []), ""))



polinomio :: Parser Expr
polinomio = space *> fmap parsePolo2 (some (satisfy isMono))
--polinomio = let positive = fmap read (some (satisfy isMono))
--          in space *> unary_minus positive

expr = subexpr  `chainl1` mul `chainl1` add --`chainr1` derive 
subexpr = token "(" *> expr <* token ")" <|> polinomio

repl :: String -> String
repl cs = let results = parse expr cs in
  case results of
    [] -> "Invalid expression"
    num -> (poliParseToStr . normPoli . eval  $ (fst (num !! 0)))


find_prev :: String -> String -> String
find_prev "" _ = ""
find_prev (x:xs) prev =  if x == '!' then ("(" ++ prev ++")") ++ find_prev xs prev  else [x] ++ find_prev xs prev

main_ :: String -> IO ()
main_ prev = do  
    putStr "> "
    hFlush stdout
    line <- getLine 
    -- previous_input <- line
    if line /= "exit"  then 
            putStrLn (unlines (map repl (lines (find_prev line prev)))) >> (main_ line)
    else return ()


main :: IO ()
main = main_ "0"
       
      --- TEM AQUI UM BUG ___ NO PoliPARSETOSTRG


  -- x^2 +2 
  -- x^2 + 1 ????
 --print (poliParseToStr . eval $ (Pow (Poli [((1, [1]),"x"), ((1, []),"")]) 'x')) 
 
 
 
 
 

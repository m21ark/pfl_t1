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

-- | Defines a Alternative in which the second choice is chosen over the first if the first fails.
instance Alternative Parser where
  empty = Parser (\_ -> [])
  p <|> q = Parser (\cs ->
    let (p', q') = (parse p cs, parse q cs) in
    if length p' > 0 then p' else q')

-- | Evaluates the expression in order to determine the result in a polinomio
eval :: Expr      -- ^ The expression to evaluate
     -> Polinomio -- ^ The resulted polinomio
eval (Poli n) = n
eval (Add e1 e2) = sumPoli_ ((eval e1) ++ (eval e2))
eval (Mult e1 e2) = multPoli (eval e1) (eval e2)
eval (Derive e1 e2) = derivePoli (eval e1) (monoVar ((eval e2) !! 0) !! 0)
eval (Sub e1 e2) = subPoli (eval e1) (eval e2)

-- | Finds the coef of the monomial 
findValue :: (String, Int) -- ^ The string to evaluate/parse and the number already seen
          -> (String, Int) -- ^ The result of evaluating the next argument
findValue ([], exp_) = ("", exp_)
findValue (x:xs, exp_) | isDigit x = findValue (xs, exp_ * 10 + digitToInt x)
                      | otherwise = (x:xs, exp_)

-- | Parses a String representation of a monomial and returns the curresponding polinomio. This is bit of a naive implementation as it comes from our first not so great parser
parseMono :: (String, Monomio) -- ^ The string yet to be parsed and a monomial
          -> (String, Monomio) -- ^ The result of evaluating the next argument
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


-- | Parses a Polynomial. This is a bit inrelevant because of the fact that the input string will be a monomial but notheless we need to parse it to a polinomio.
parsePolo :: String     -- ^ String representation
          -> Polinomio  -- ^ Polynomial resulted from parsing
parsePolo "" = [] 
parsePolo (s:s') | s == ')' = parsePolo s'
parsePolo s =  normPoli ([mono] ++ parsePolo toParseStr)
                where (toParseStr, mono) = parseMono (s, ((1, []), ""))

-- | Consumes the next char.
item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:c') -> [(c,c')])

-- | See if char matches the pattern defined in (char -> Bool)
satisfy :: (Char -> Bool) -- ^ Function that determines the satisfaction of the next char
        -> Parser Char    -- ^ The resulting parser, if it's true, else empty 
satisfy pred_ = item >>= (\c -> if pred_ c then pure c else empty)

-- | Matches a char and checks satisfaction
char :: Char        -- ^ The character
     -> Parser Char -- ^ The resulting parser, if it's true, else empty
char c = satisfy (== c)

-- | Defines the satisfy function that tells the parser that this is a polinomio. It's actually a polinomio with only one argument, a monomio, but has it returns a polinomio we opt to keep the name
isPoli :: Char -- ^ The character
       -> Bool -- ^ True if it is a character from a polinomio else False
isPoli x = isDigit x || isLetter x || '^' == x 

-- | Parses whitespaces
space :: Parser String
space = many (satisfy isSpace)

-- | Applies the Functor of : with the char and then uses the result as the applicative of the rest of the string. This allows us to check if a string occurs.
string :: String        -- ^ The string to check against
       -> Parser String -- ^ The resulting parser 
string "" = return ""
string (c:cs) = (:) <$> char c <*> string cs

-- | Parses empty Spaces and check if symb is satisfied
token :: String        -- ^ The token
      -> Parser String -- ^ The resulting parser
token symb = space *> string symb

-- | Checks if the next token is a multiplication
mul :: Parser (Expr -> Expr -> Expr)
mul = token "*" *> pure Mult

-- | Checks if the next token is an addition or subtraction
add :: Parser (Expr -> Expr -> Expr)
add = token "+" *> pure Add <|> token "-" *> pure Sub

-- | Checks if the next token is a derivative 
derive :: Parser (Expr -> Expr -> Expr)
derive = token "'" *> pure Derive

-- | Constructs a Operation with left association. In our case, the a means Expr.  
chainl1 :: Parser a ->          -- ^ Represents the Factor between the separators
           Parser (a -> a -> a) -- ^ Used for the separators
           -> Parser a          -- ^ The resulting parser
p `chainl1` op = p >>= rest
  where rest a = ((op <*> pure a <*> p) >>= rest)  <|> return a

-- | Parses the string returning the corresponding Expr
parseExpr :: String -- ^ String to parse
          -> Expr   -- ^ Resulting expression
parseExpr "" = Poli []
parseExpr (s:s') | s == ')' = Poli $ normPoli $ parsePolo $ s'
parseExpr s =  Poli $ normPoli ([mono] ++ parsePolo toParseStr)
                where (toParseStr, mono) = parseMono (s, ((1, []), ""))

-- | "Defines" a polinomio
polinomio :: Parser Expr
polinomio = space *> fmap parseExpr (some (satisfy isPoli))
 
-- | Defines an expression
expr :: Parser Expr
expr = subexpr `chainl1` derive `chainl1` mul `chainl1` add   

-- | Defines a subexpression
subexpr :: Parser Expr
subexpr = token "(" *> expr <* token ")" <|> polinomio

-- | Replies to the input String of the user
repl :: String -- ^ String reprenting a polinomio
     -> String -- ^ The result of parsing and evaluating the given String
repl cs = let results = parse expr cs in
  case results of
    [] -> "Invalid expression"
    num -> (show (fst (num !! 0)))
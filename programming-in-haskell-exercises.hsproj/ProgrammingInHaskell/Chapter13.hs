module Chapter13 where

---------------------------------------
-- Monadic parsing
  
import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x : xs) -> [(x, xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v, out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v, inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v, out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v, out)] -> [(v, out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x : xs) = do  char x
                      string xs
                      return (x : xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x : xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

---------------------------------------
-- Exercise 1

comment :: Parser ()
comment = do  string "--"
              many (sat (/= '\n'))
              return ()

---------------------------------------
-- Exercises: 5, 6, 7

data Expr
  = Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  deriving Show
  
 
expr :: Parser Expr
expr = do   t <- term
            do  symbol "+"
                e <- expr
                return (Add t e)
             <|> 
                do  symbol "-"
                    e <- expr
                    return (Sub t e)
             <|> return t


term :: Parser Expr
term = do   e <- exponential
            do  symbol "*"
                t <- term
                return (Mul e t)
             <|>
                do  symbol "/"
                    t <- term
                    return (Div e t)                    
             <|> return e
             

exponential :: Parser Expr
exponential = do   f <- factor
                   do   symbol "^"
                        e <- exponential
                        return (Exp f e)
                    <|> return f

factor :: Parser Expr
factor = do   symbol "("
              e <- expr
              symbol ")"
              return e
          <|> fmap Num integer

---------------------------------------
-- Exercise 8

-- expr ::= nat | expr - nat
-- nat  ::= 0 | 1 | 2 | ...

nats :: Parser [Int]
nats = do   n <- natural
            ns <- many (do  symbol "-" 
                            n <- natural
                            return (-n))
            return (n : ns)


expr' :: Parser Int
expr' = do  xs <- nats
            return (foldl (+) 0 xs)

---------------------------------------
-- Exercise 9 TODO

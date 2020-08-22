module Chapter09 where
  
---------------------------------------
-- The countdown problem

-- Arithmetic operators

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Numeric expressions

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- Combinatorial functions

-- Returns all subsequences of a list
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

-- Returns all possible ways of inserting 
-- a new element into a list
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- Returns all permutations of a list
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- Returns all possible ways of selecting zero 
-- or more elements in any order
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- Formalising the problem

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- Brute force solution

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l       <- exprs ls,
                 r       <- exprs rs,
                 e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
  
---------------------------------------
-- Exercise 1

choices' :: [a] -> [[a]]
choices' xs =  [zs | ys <- subs xs, zs <- perms ys]

---------------------------------------
-- Exercise 2

removeone x [] = []
removeone x (y:ys)
  | x == y = ys
  | otherwise = y : removeone x ys

isChoise :: Eq a => [a] -> [a] -> Bool
isChoise [] _ = True
isChoise (x:xs) ys = elem x ys && isChoise xs (removeone x ys) 

---------------------------------------
-- Exercise 4

count ns = (possibleExprs, successfullyExprs)
  where
    expressions = [e | ns' <- choices ns, e <- exprs ns']
    possibleExprs = length expressions
    reduce = foldl (\s xs -> if xs == [] then s else s + 1) 0
    successfullyExprs = reduce (map eval expressions)
    
-- Compile with ghc
--main :: IO()
--main = print (count [1, 3, 7, 10, 25, 50])

---------------------------------------
-- Exercise 5

--valid :: Op -> Int -> Int -> Bool
--valid Add _ _ = True
--valid Sub x y = True
--valid Mul _ _ = True
--valid Div x y = y /= 0 && x `mod` y == 0

-- Compile with ghc
--main :: IO()
--main = print (count [1, 3, 7, 10, 25, 50])

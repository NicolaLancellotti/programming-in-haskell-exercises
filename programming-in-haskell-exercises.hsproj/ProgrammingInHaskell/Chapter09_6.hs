module Chapter09_6 where
  
---------------------------------------
-- Exercise 6

-- Arithmetic operators

data Op = Add | Sub | Mul | Div | Exp
  deriving(Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0
valid' Exp x y = x /= 1 && y /= 1 && x ^ y > 0 -- overflows are more frequent with exponentiation

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y 

-- Numeric expressions

data Expr = Val Int | App Op Expr Expr
 
instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e       = "(" ++ show e ++ ")"

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid' o x y]

-- Combinatorial functions

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- Combining generation and evaluation

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

type Result = (Expr,Int)

results' :: [Int] -> [Result]
results' []  = []
results' [n] = [(Val n,n) | n > 0]
results' ns  = [res | (ls,rs) <- split ns,
                       lx     <- results' ls,
                       ry     <- results' rs,
                       res    <- combine'' lx ry]

combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns, (e,m) <- results' ns', m == n]

---------------------------------------
-- Exercise 6b
  
nearestSolution :: [Int] -> Int -> [Result]
nearestSolution ns n = if null exact then nearest else exact
  where 
    exact = fst solutions
    nearest = snd solutions
    results = [r | ns' <- choices ns, r <- results' ns']
    solutions = findSol results ([], [])
    
    findSol :: [Result] -> ([Result], [Result]) -> ([Result], [Result])
    findSol [] (exact, nearest) = (exact, nearest)
    findSol ((e, m) : rs) (exact, nearest) 
      | m == n      = findSol rs (exact ++ [(e, m)], []) 
      | null exact  = findSol rs ([], newNearest (e, m) nearest)
      | otherwise   = findSol rs (exact, [])

    newNearest :: Result -> [Result] -> [Result]
    newNearest r []                    = [r]
    newNearest (e1, m1) ((e2, m2):_)
      | abs (m1 - n) < abs (m2 - n)    = [(e1, m1)]
      | otherwise                      = [(e2, m2)]


---------------------------------------
-- Exercise 6c

instance Eq Expr where
  Val x == Val y = x == y
  App op1 e1_lhs e1_rhs == App op2 e2_lhs e2_rhs = 
      op1 == op2 && e1_lhs == e2_lhs && e1_rhs == e2_rhs
  _ == _ = False
  
instance Ord Expr where
  (lhs) <= (rhs) = expensiveOps lhs <= expensiveOps rhs
    where
      expensiveOps e = numOps Mul e + numOps Div e + numOps Exp e
    
numOps :: Op -> Expr -> Int
numOps _ (Val _)      = 0
numOps op (App o l r) = numOps op l + numOps op r + if op == o then 1 else 0

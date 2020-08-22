module Chapter08 where
  
---------------------------------------
-- Exercise 1

data Nat = Zero | Succ Nat
  deriving(Show, Eq)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n) 

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

---------------------------------------
-- Exercise 2

data Tree2 a = Leaf2 a | Node2 (Tree2 a) a (Tree2 a)

occurs :: Ord a => a -> Tree2 a -> Bool
occurs x (Leaf2 y) = x == y
occurs x (Node2 l y r) = case compare x y of
  EQ -> True
  LT -> occurs x l
  GT -> occurs x r
    
---------------------------------------
-- Exercise 3

data Tree3 a = Leaf3 a | Node3 (Tree3 a) (Tree3 a)
  deriving(Show)

leaves :: Tree3 a -> Int
leaves (Leaf3 _) = 1
leaves (Node3 l r) = leaves l + leaves r

balanced :: Tree3 a -> Bool
balanced (Leaf3 _) = True
balanced (Node3 l r) = balanced l && balanced r && 
  abs(leaves l - leaves r) <= 1

---------------------------------------
-- Exercise 4

balance :: [a] -> Tree3 a
balance [x] = Leaf3 x
balance xs = Node3 (balance l) (balance r)
  where (l, r) = splitAt (length xs `div` 2) xs
  
---------------------------------------
-- Exercise 5

data Expr5 = Val5 Int | Add5 Expr5 Expr5

folde :: (Int -> a) -> (a -> a -> a) -> Expr5 -> a
folde f g (Val5 x) = f x
folde f g (Add5 x y) = g (folde f g x) (folde f g y)

---------------------------------------
-- Exercise 6

eval :: Expr5 -> Int
eval e = folde id (+) e

size :: Expr5 -> Int
size e = folde (\_ -> 1) (+) e

---------------------------------------
-- Exercise 7

data Maybe6 a = Nothing6 | Just6 a

instance Eq a => Eq (Maybe6 a) where
  Nothing6 == Nothing6 = True
  Just6 x == Just6 b = x == b
  _ == _ = False
  
--instance Eq a => Eq [a] where
--    [] == [] = True
--    (x : xs) == (y : ys) = x == y && xs == ys
--    _ == _ = False

---------------------------------------
-- Exercise 8

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Eq Prop Prop
          
type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t =  head [v | (k',v) <- t, k == k']

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
  where 
    eval :: Subst -> Prop -> Bool
    eval _ (Const b)   = b
    eval s (Var x)     = find x s
    eval s (Not p)     = not (eval s p)
    eval s (And p q)   = eval s p && eval s q
    eval s (Imply p q) = eval s p <= eval s q
    eval s (Or p q)    = eval s p || eval s q
    eval s (Eq p q)    = eval s p == eval s q
    
    substs :: Prop -> [Subst]
    substs p = map (zip vs) (bools (length vs)) 
      where 
        bools :: Int -> [[Bool]]
        bools 0 = [[]]
        bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)
            
        vs = rmdups (vars p)
          where 
            vars :: Prop -> [Char]
            vars (Const _)   = []
            vars (Var x)     = [x]
            vars (Not p)     = vars p
            vars (And p q)   = vars p ++ vars q
            vars (Imply p q) = vars p ++ vars q
            vars (Or p q)    = vars p ++ vars q
            vars (Eq p q)    = vars p ++ vars q
               
            rmdups :: Eq a => [a] -> [a]
            rmdups []     = []
            rmdups (x:xs) = x : rmdups (filter (/= x) xs)
         
---------------------------------------
-- Exercise 9

data Expr = Val Int | Add Expr Expr 
  | Mul Expr Expr | Div Expr Expr

type Cont = [Op]

data Op = EVALADD Expr | ADD Int 
  | EVALMUL Expr | MUL Int
  | EVALDIV Expr | DIV Int

value :: Expr -> Int
value e = eval e []
  where  
    eval :: Expr -> Cont -> Int
    eval (Val n)   c = exec c n
    eval (Add x y) c = eval x (EVALADD y : c)
    eval (Mul x y) c = eval x (EVALMUL y : c)
    eval (Div x y) c = eval x (EVALDIV y : c)

    exec :: Cont -> Int -> Int
    exec [] n              = n
    exec (EVALADD y : c) n = eval y (ADD n : c)
    exec (ADD n : c) m     = exec c (n + m)
    exec (EVALMUL y : c) n = eval y (MUL n : c)
    exec (MUL n : c) m     = exec c (n * m)
    exec (EVALDIV y : c) n = eval y (DIV n : c)
    exec (DIV n : c) m     = exec c (n `div` m)

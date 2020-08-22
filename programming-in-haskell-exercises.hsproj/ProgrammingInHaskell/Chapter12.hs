module Chapter12 where
  
---------------------------------------
-- Exercise 1

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show
  
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)
  
---------------------------------------
-- Exercise 2

{-
instance Functor ((->) r) where
-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
   fmap = (.)
-}

---------------------------------------
-- Exercise 3

{-
instance Applicative ((->) r) where
-- pure :: a -> f a
-- pure :: a -> ((->) r a)
-- pure :: a -> (r -> a)
   pure = const

-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: ((->) r (a -> b)) -> ((->) r a) -> ((->) r b)
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
   g <*> h = \x -> g x (h x)
-}

---------------------------------------
-- Exercise 4

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = pure g <*> Z xs
  
instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)
  
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

---------------------------------------
-- Exercise 5

{-
----------------------
law: pure id <*> x = x

left:
id :: a -> a
pure :: a -> f a
pure id :: f (a -> a)
x :: f a
pure id <*> x :: f a

right 
x :: f a
-----------------------------------
law: pure (g x) = pure g <*> pure x

left:
pure :: a -> f a
g :: a -> b
x :: a
g x :: b
pure (g x) :: f b

right: 
g :: a -> b
pure g :: f (a -> b)
x :: a
pure x :: f a
pure g <*> pure x :: f b
-----------------------------------------
law: x <*> pure y = pure (\g ->g y) <*> x

left: 
x :: f (a -> b)
y :: a
pure y :: f a
x <*> pure y :: f b

right:
(\g ->g y) :: (a -> b) -> b
pure (\g ->g y) :: f ((a -> b) -> b)
x :: f (a -> b)
pure (\g ->g y) <*> x :: f b
---------------------------------------------------
law: x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

left:
x ::  f (a -> b)
y ::  f (c -> a)
z  :: f c
y <*> z :: f a
x <*> (y <*> z) :: f b

right:
pure (.) :: f ((a -> b) -> (c -> a -> c -> b))
x ::  f (a -> b)
pure (.) <*> x :: f (c -> a -> c -> b)
y ::  f (c -> a)
(pure (.) <*> x <*> y) :: f (c -> b)
z :: f c
(pure (.) <*> x <*> y) <*> z :: f b
-}

---------------------------------------
-- Exercise 6

{-
instance Monad ((->) r) where
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: ((->) r a) -> (a -> ((->) r b)) -> ((->) r b)
-- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
   f >>= g = \x -> g (f x) x
-}

---------------------------------------
-- Exercise 7

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show


instance Functor Expr where
--  fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x) = Var (f x)
    fmap _ (Val x) = Val x
    fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)
    
instance Applicative Expr where
--  pure :: a -> Expr a
    pure x = Var x

--  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    (Var f) <*> e = fmap f e
    (Val x) <*> _ = Val x
    (Add f1 f2) <*> e = Add (f1 <*> e) (f2 <*> e)
    
instance Monad Expr where
--  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Var x) >>= f = f x
    (Val x) >>= f = Val x
    (Add e1 e2) >>= f = Add (e1 >>= f) (e2 >>= f)

---------------------------------------
-- Exercise 8

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
-- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = do  x <- st
                   return (g x)
   
instance Applicative ST where
-- pure :: a -> ST a
   pure x = S (\s -> (x, s))

-- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = do  f <- stf
                     x <- stx
                     return (f x)

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s ->
    let (x,s') = app st s in app (f x) s')

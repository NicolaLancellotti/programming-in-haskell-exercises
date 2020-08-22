module Chapter16 where
  
---------------------------------------
-- Exercise 1

{-
Show:
add n (Succ m) == Succ (add n m)

Base case:

add Zero (Succ m)
= { applying add }
Succ m
= { unapplying add }
Succ (add Zero m)

Inductive case:

add (Succ n) (Succ m)
= { applying add }
Succ (add n (Succ m))
= { induction hypothesis }
Succ (Succ (add n m))
= { unapplying add }
Succ (add (Succ n) m)
-}

---------------------------------------
-- Exercise 2

{-
Given:
P1: add n (Succ m) == Succ (add n m)
P2: add n Zero = n

Show:
add n m = add m n

Base case:

add Zero m
= { applying add }
m
= { unapplying P2}
add m Zero

Inductive case:

add (Succ n) m
= { applying add }
Succ (add n m)
= { induction hypothesis }
Succ (add m n)
= { unapplying P1 }
add m (Succ n)
-}

---------------------------------------
-- Exercise 3

{-
Show:
all (== x) (replicate n x)

Base case:

all (== x) (replicate 0 x)
= { applying replicate }
all (== x) []
= { applying all}
True

Inductive case:

all (== x) (replicate (n + 1) x)
= { applying replicate }
all (== x) (x : replicate (n))
= { applying all }
(== x) x && all p replicate (n)
= { applying == x }
True && all (== x) replicate (n)
= { applying && }
all (== x) replicate (n)
= { induction hypothesis }
True
-}

---------------------------------------
-- Exercise 4

{-
Show:
xs ++ [] == xs

Base case:
[] ++ []
= { applying ++ }
[]

Inductive case:
(x : xs) ++ []
= { applying ++ }
x : (xs ++ [])
= { induction hypothesis }
x : xs

Show:
xs ++ (ys ++ zs) == (xs ++ ys) ++ zs

Base case:
[] ++ (ys ++ zs)
= { applying ++ }
ys ++ zs
= { unapplying ++ }
([] ++ ys) ++ zs

Inductive case:
(x : xs) ++ (ys ++ zs)
= { applying ++ }
x : (xs ++ (ys ++ zs))
= { induction hypothesis }
x : ((xs ++ ys) ++ zs)
= { unapplying ++ }
(x : (xs ++ ys)) ++ zs
= { unapplying ++ }
((x : xs) ++ ys)) ++ zs
-}

---------------------------------------
-- Exercise 5

{-
Show:
take n xs ++ drop n xs = xs

Base case (n = 0,  xs = xs):
take 0 xs ++ drop 0 xs
= { applying take }
[] ++ drop 0 xs
= { applying ++ }
drop 0 xs
= { applying drop }
xs

Base case (n = n + 1, xs = []):
take (n + 1) [] ++ drop (n + 1) []
= { applying take }
[] ++ drop (n + 1) []
= { applying ++ }
drop (n + 1) []
= { applying drop }
[]

Inductive case: (n = n + 1, xs = (x : xs))
take (n + 1) (x : xs) ++ drop (n + 1) (x : xs)
= { applying take }
(x : (take n xs)) ++ drop (n + 1) (x : xs)
= { applying ++ }
x : ((take n xs) ++ drop (n + 1) (x : xs))
= { applying drop }
x : ((take n xs) ++ drop n xs)
= { induction hxpothesis }
x : xs
-}
---------------------------------------
-- Exercise 6

{-
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

nodes (Leaf _) = 0
nodes (Node l r) = nodes l + nodes r + 1

Show
leaves tree = 1 + nodes tree

Base case:
leaves (Leaf _)
= { applying leaves }
1
= { identity element }
1 + 0
= { unapplying nodes }
1 + nodes (Leaf _)

Inductive case:
leaves (Node l r)
= { applying leaves }
leaves l + leaves r
= { induction hypothesis }
1 + nodes l + 1 + nodes r
= { associative property }
1 + (nodes l + 1 + nodes r)
= { unapplying nodes }
1 + nodes (Node l r)
-}

---------------------------------------
-- Exercise 7

{-
Functor law:
fmap id = id

fmap id Nothing 
= { applying fmap }
Nothing 
= { unapplying id }
id Nothing

fmap id (Just x) 
= { applying fmap }
Just (id x) 
= { applying id }
Just x 
= { unapplying id }
id (Just x)
-}

{-
Functor law:
fmap (g . h) = fmap g . fmap h

fmap (g . h) Nothing
= { applying fmap }
Nothing
= { unapplying fmap }
fmap g Nothing
= { unapplying fmap }
fmap g (fmap g Nothing)
= { unapplying . }
(fmap g . fmap h) Nothing

fmap (g . h) (Just x)
= { applying fmap }
Just ((g . h) x)
= { applying . }
Just (g (h x))
= { unapplying fmap }
fmap g (Just (h x)) 
= { unapplying fmap }
fmap g (fmap h (Just x)) 
= { unapplying . }
(fmap g . fmap h) (Just x)
-}

---------------------------------------
-- Exercise 8

{-
Functor law:
fmap id = id

Base case:
fmap id (Leaf x)
= { applying fmap }
Leaf (id x)
= { applying id }
Leaf x
= { unapplying id }
id (Leaf x)

Inductive case:
fmap id (Node l r)
= { applying fmap }
Node (fmap id l) (fmap id r)
= { induction hypothesis }
Node l r
= { applying id }
id (Node l r)
-}

{-
Functor law:
fmap (g . h) = fmap g . fmap h

Base case:
fmap (g . h) (Leaf x)
= { applying fmap }
Leaf ((g . h) x)
= { applying . }
Leaf (g (h x))
= { unapplying fmap }
fmap g (Leaf (h x))
= { unapplying fmap }
fmap g (fmap h (Leaf x))
= { unapplying . }
(fmap g . fmap h) (Leaf x)

Inductive case:
fmap (g . h) (Node l r)
= { applying fmap }
Node (fmap (g . h) l) (fmap (g . h) r)
= { induction hypothesis }
Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
= { applying . }
Node (fmap g (fmap h l)) (fmap g (fmap h r))
= { unapplying fmap }
fmap g (Node (fmap h l) (fmap h r))
= { unapplying fmap }
fmap g (fmap h (Node l r))
= { unapplying . }
(fmap g . fmap h) (Node l r)
-}

---------------------------------------
-- Exercise 9

{-
Applicative law:
pure id <*> x = x

pure id <*> Nothing 
= { applying pure }
(Just id) <*> Nothing
= { applying <*> }
fmap id Nothing 
= { applying fmap }
Nothing

pure id <*> (Just x)
= { applying pure }
(Just id) <*> (Just x)
= { applying <*> }
fmap id (Just x)
= { applying fmap }
Just x
-}

{-
Applicative law:
pure (g x) = pure g <*> pure x

pure (g Nothing)
= { applying pure }
Just (g Nothing)
= { unapplying fmap }
fmap g (Just Nothing)
= { unapplying pure }
fmap g (pure Nothing)
= { unapplying <*> }
(Just g) <*> (pure Nothing)
= { unapplying pure }
pure g <*> (pure Nothing)

pure (g (Just x))
= { applying pure }
Just (g (Just x))
= { unapplying fmap }
fmap g (Just (Just x))
= { unapplying pure }
fmap g (pure (Just x))
= { unapplying <*> }
(Just g) <*> (pure (Just x))
= { unapplying pure }
pure g <*> (pure (Just x))
-}

{-
Applicative law:
x <*> pure y = pure (\g ->g y) <*> x

Nothing <*> pure y
= { applying <*> }
Nothing
= { unapplying fmap }
fmap (\g ->g y) Nothing
= { unapplying <*> }
(Just (\g ->g y)) <*> Nothing
= { unapplying pure }
pure (\g ->g y) <*> Nothing

(Just f) <*> pure y
= { applying <*> }
fmap f (pure y)
= { applying pure }
fmap f (Just y)
= { applying fmap }
Just (f y)
= { f y = (\g -> g y) f }
Just ((\g -> g y) f)
= { unapplying fmap }
fmap (\g -> g y) (Just f) 
= { unapplying <*> }
(Just (\g ->g y)) <*> (Just f)
= { unapplying pure }
pure (\g ->g y) <*> (Just f)
-}

{-
Applicative law:
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

Case x = Nothing
Nothing <*> (y <*> z)
= { applying <*> }
Nothing
= { unapplying <*> }
Nothing <*> z
= { unapplying <*> }
Nothing <*> y <*> z
= { unapplying fmap }
fmap (.) Nothing <*> y <*> z
= { unapplying <*> }
Just (.) <*> Nothing <*> y <*> z
= { unapplying pure }
pure (.) <*> Nothing <*> y <*> z
= { <*> associate to the left }
(pure (.) <*> Nothing <*> y) <*> z

Case x = (Just x), y = Nothing
(Just x) <*> (Nothing <*> z)
= { applying <*> }
fmap (Just g) (Nothing <*> z)
= { applying <*> }
fmap (Just g) (Nothing)
= { applying fmap }
Nothing
= { unapplying <*> }
Nothing <*> z
= { unapplying fmap }
fmap (.) Nothing <*> z
= { unapplying <*> }
(Just (.)) <*> Nothing <*> z
= { unapplying pure }
pure (.) <*> Nothing <*> z
= { unapplying fmap }
pure (.) <*> (fmap x Nothing) <*> z
= { unapplying <*> }
pure (.) <*> (Just x) <*> Nothing <*> z
= { <*> associate to the left }
(pure (.) <*> (Just x) <*> Nothing) <*> z

Case x = (Just x), y = (Just y), z = Nothing
(Just x) <*> ((Just y) <*> Nothing)
= { applying <*> }
fmap (Just g) ((Just y) <*> Nothing)
= { applying <*> }
fmap (Just g) (fmap y Nothing)
= { applying fmap }
fmap (Just g) Nothing
= { applying fmap }
Nothing
= { unapplying fmap }
fmap (.) Nothing
= { unapplying <*> }
(Just (.)) <*> Nothing
= { unapplying pure }
pure (.) <*> Nothing
= { unapplying fmap }
pure (.) <*> (fmap x Nothing)
= { unapplying <*> }
pure (.) <*> (Just x) <*> Nothing
= { unapplying fmap }
pure (.) <*> (Just x) <*> (fmap y Nothing)
= { unapplying <*> }
pure (.) <*> (Just x) <*> (Just y) <*> Nothing
= { <*> associate to the left }
(pure (.) <*> (Just x) <*> (Just y) <*>) Nothing

Case x = (Just x), y = (Just y), z = (Just z)
(Just x) <*> ((Just y) <*> (Just z))
= { applying <*> }
fmap x ((Just y) <*> (Just z))
= { applying <*> }
fmap x (fmap y (Just z))
= { applying fmap }
fmap x (Just (y z))
= { applying fmap }
Just (x (y z))
= { unapplying (.) }
Just ((x . y) z)
= { unapplying fmap }
fmap (x . y) (Just z)
= { unapplying <*> }
(Just (x . y)) <*> (Just z)
= { applying currying for (.) }
(Just (x .) y) <*> (Just z)
= { unapplying fmap }
fmap (x .) (Just y) <*> (Just z)
= { unapplying <*> }
(Just (x .)) <*> (Just y) <*> (Just z)
= { unapplying currying for (.) }
(Just ((.) x)) <*> (Just y) <*> (Just z)
= { unapplying fmap }
fmap (.) (Just x) <*> (Just y) <*> (Just z)
= { unapplying <*> }
(Just (.)) <*> (Just x) <*> (Just y) <*> (Just z)
= { unapplying pure }
pure(.) <*> (Just x) <*> (Just y) <*> (Just z)
= { <*> associate to the left }
(pure(.) <*> (Just x) <*> (Just y)) <*> (Just z)
-}

---------------------------------------
-- Exercise 10

{-
Monad law:
return x >>= f = f x 

return x >>= f
= { applying return }
[x] >>= f
= { applying >>= }
[y | x1 <- [x], y <- f x1]
= { applying list comprehension }
f x
-}

{-
Monad law:
mx >>= return = mx

mx >>= return
= { applying >>= }
[y | x <- mx, y <- return x]
= { applying list comprehension }
mx
-}

{-
Monad law:
(mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

(mx >>= f) >>= g
= { applying >>= }
[y1 | x1 <- mx, y1 <- f x1] >>= g
= { applying >>= }
[y2 | x2 <- [y1 | x1 <- mx, y1 <- f x1], y2 <- g x2]
= { flatting list comprehensions }
[y2 | x1 <- mx, y1 <- f x1, y2 <- g y1]
= { nesting list comprehension }
[y2 | x1 <- mx, y2 <- [y2 | y1 <- f x1, y2 <- g y1]]
= { extracting a lambda }
[y2 | x1 <- mx, y2 <- (\x -> [y2 | y1 <- f x, y2 <- g y1]) x1]
= { unapplying >>= }
[y2 | x1 <- mx, y2 <- (\x -> (f x >>= g) ) x1]
= { unapplying >>= }
mx >>= (\x -> (f x >>= g))
-}

---------------------------------------
-- Exercise 11

{-
Specification:
comp' e c == comp e ++ c

Show how to construct the recursive definition for comp':

comp'(Val n) c 
= { applying specification }
comp (Val n) ++ c
= { applying comp }
[PUSH n] ++ c
= { applying ++ }
PUSH n : c

comp' (Add y) c
= { applying specification }
comp (Add y) ++ c
= { applying comp }
comp x ++ comp y ++ [ADD] ++ c
= { applying ++ }
comp x ++ comp y ++ (ADD : c)
= { unapplying specification }
comp x ++ (comp' y (ADD : c))
= { unapplying specification }
comp' x (comp' y (ADD : c))
-}

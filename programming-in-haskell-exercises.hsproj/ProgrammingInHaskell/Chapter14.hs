module Chapter14 where

import Data.Foldable
  
---------------------------------------
-- Exercise 1

--instance (Monoid a, Monoid b) => Monoid (a, b) where
--  -- mempty :: (a, b)
--  mempty = (mempty, mempty)
--  
--  --mappend :: (a, b) -> (a, b) -> (a, b)
--  (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)
  
---------------------------------------
-- Exercise 2

--instance Monoid b => Monoid (a -> b) where
--  -- mempty :: (a -> b)
--  mempty = \_ -> mempty
--  
--  --mappend :: (a -> b) -> (a -> b) -> (a -> b)
--  f `mappend` g = \x -> f x `mappend` g x

---------------------------------------
-- Exercise 3

data Maybe' a = Just' a | Nothing'
  deriving (Eq, Ord, Show)
  
instance Foldable Maybe' where
  --  fold    :: Monoid a => Maybe' a -> a
  fold Nothing' = mempty
  fold (Just' x) = x

  -- foldMap :: Monoid b => (a -> b) -> Maybe' a -> b
  foldMap _ Nothing' = mempty
  foldMap f (Just' x) = f x
  
  --  foldr   :: (a -> b -> b) -> b -> Maybe' a -> b 
  foldr _ v Nothing' = v
  foldr f v (Just' x) = f x v
  
  -- foldl   :: (a -> b -> a) -> a -> Maybe' b -> a
  foldl _ v Nothing' = v
  foldl f v (Just' x) = f v x  

instance Functor Maybe' where
  -- fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap g (Just' x) = Just' (g x)
  
instance Traversable Maybe' where
  --  traverse :: Applicative f => (a -> f b) -> Maybe' a -> f (Maybe' b)
  traverse _ Nothing' = pure Nothing'
  traverse g (Just' x) = pure Just' <*> g x
  
---------------------------------------
-- Exercise 4

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show
  
instance Foldable Tree where
  -- fold    :: Monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l `mappend` x `mappend` fold r
  
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

  --  foldr   :: (a -> b -> b) -> b -> Tree a -> b 
  foldr _ v Leaf = v
  foldr f v (Node l x r) = left
    where 
      right = foldr f v r
      middle = f x right
      left = foldr f middle l

  -- foldl   :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ v Leaf = v
  foldl f v (Node l x r) = right
    where 
      left  = foldl f v l
      middle = f left x
      right = foldl f middle r
      
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Traversable Tree where
  --  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r

---------------------------------------
-- Exercise 5

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f x = filter f (foldMap (\x -> [x]) x)

module Chapter15 where
  
---------------------------------------
-- Exercise 4

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

---------------------------------------
-- Exercise 5

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

repeat' :: a -> Tree a
repeat' x = Node (repeat' x) x (repeat' x)

take' :: Int -> Tree a -> Tree a
take' 0 _ = Leaf
take' _ Leaf = Leaf
take' n (Node l x r) = 
  (Node (take' (n - 1) l) x (take' (n - 1) r))

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

---------------------------------------
-- Exercise 6

sqroot :: Double -> Double
sqroot n = (snd . head) results
  where
    distance = 0.1
    initial = 1
    
    next :: Double -> Double
    next a = (a + n / a) / 2    
    
    approx :: [Double]
    approx = iterate next initial
        
    z = zip approx (tail approx)

    results = filter (\(x, y) -> abs(x - y) < distance) z
  
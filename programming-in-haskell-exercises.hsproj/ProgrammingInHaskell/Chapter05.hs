module Chapter05 where

import Data.Char
  
---------------------------------------
-- Exercise 2

grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

---------------------------------------
-- Exercise 3

square n = [(x, y) | (x, y) <- grid n n, x /= y]

---------------------------------------
-- Exercise 4

replicate' n x = [x | _ <- [1..n]]

---------------------------------------
-- Exercise 5

pyths n = [(x, y, z) | x <- [1 .. n], 
                       y <- [1 .. n], 
                       z <- [1 .. n], 
                       x ^ 2 + y ^ 2 == z ^ 2]

---------------------------------------                       
-- Exercise 6
factors n = [x | x <- [1 .. n], n `mod` x == 0]
  
perfects n = [x | x <- [1 .. n], sum(init(factors x)) == x]

---------------------------------------
-- Exercise 7

exercise7 xs ys = concat [[(x, y) | y <- ys] | x <- xs ]

---------------------------------------
-- Exercise 8

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions x xs = find x (zip xs [0 ..])

---------------------------------------
-- Exercise 9

scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

---------------------------------------  
-- Exercise 10

lower2int c = ord c  - ord 'a'
upper2int c = ord c  - ord 'A'

int2lower n = chr(ord 'a' + n)
int2upper n = chr(ord 'A' + n)

shift n c
  | isLower c = int2lower((lower2int c + n) `mod` 26)
  | isUpper c = int2upper((upper2int c + n) `mod` 26)
  | otherwise = c

encode n xs = [shift n x | x <- xs]

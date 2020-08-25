module Chapter01 where
 
---------------------------------------
-- Exercise 3

product' [] = 1
product' (n : ns) = n * product' ns

---------------------------------------
-- Exercise 4

qsort4 [] = []
qsort4 (x:xs) = qsort4 larger ++ [x] ++ qsort4 smaller
  where 
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]

---------------------------------------    
-- Exercise 5

qsort5 [] = []
qsort5 (x:xs) = qsort5 smaller ++ [x] ++ qsort5 larger
  where 
    smaller = [a | a <- xs, a < x]
    larger = [a | a <- xs, a > x]
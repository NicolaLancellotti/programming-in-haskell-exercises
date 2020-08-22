module Chapter01 where
 
---------------------------------------
-- Exercise 3

product' [] = 1
product' (n : ns) = n * product' ns

---------------------------------------
-- Exercise 4

qsort' [] = []
qsort' (x : xs) = qsort' larger ++ [x] ++ qsort' smaller
  where 
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]

---------------------------------------    
-- Exercise 5

qsort'' [] = []
qsort'' (x : xs) = qsort'' smaller ++ [x] ++ qsort'' larger
  where 
    smaller = [a | a <- xs, a < x]
    larger = [a | a <- xs, a > x]

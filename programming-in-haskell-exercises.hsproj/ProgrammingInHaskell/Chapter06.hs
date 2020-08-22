module Chapter06 where
  
---------------------------------------
-- Exercise 1

fac 0 = 1
fac n | n > 0 = n * fac (n - 1)

---------------------------------------
-- Exercise 2

sumdown n 
  | n > 0 = n + sumdown (n - 1)
  | n == 0 = 0

---------------------------------------  
-- Exercise 3

m ^^^ 0 = 1
m ^^^ n | n > 0 = m  * (m ^^^ (n - 1))

---------------------------------------
-- Exercise 4

euclid x y 
  | x == y = x
  | x < y = euclid (y - x) x
  | x > y = euclid (x - y) y
           
euclid' x y 
  | x == y = x
  | otherwise = euclid (max - min) min 
      where (max, min) = if x > y then (x, y) else (y, x)

---------------------------------------
-- Exercise 6

and' [] = True
and' (False : _) = False
and' (True : xs) = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs: xxs)  =  xs ++ concat' xxs

replicate' n x 
  | n == 0 = []
  | n > 0 = [x] ++ replicate' (n - 1) x 

nth (x : xs) n 
 | n == 0 = x
 | n > 0 = nth xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x : xs)
  | x == n = True
  | otherwise = elem' n xs
  
---------------------------------------
-- Exercise 7

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x : xs) (y : ys) 
  | x < y = [x] ++ merge xs ([y] ++ ys)
  | x > y = [y] ++ merge ys ([x] ++ xs)

---------------------------------------
-- Exercise 8

halve xs = (take k xs, drop k xs) 
  where k = length xs `div` 2 
         
msort [] = []
msort [x] = [x]
msort xs = merge (msort x) (msort y) 
  where (x, y) = halve xs

---------------------------------------
-- Exercise 9

sum' [] = 0
sum' (x : xs) = x + sum' xs

take' _ [] = []
take' n (x : xs) 
  | n <= 0 = []
  | otherwise = [x] ++ take' (n - 1) xs
  
last' (x : xs)
  | null xs = x
  | otherwise = last' xs

module Chapter04 where
  
---------------------------------------
-- Exercise 1

halve :: [a] -> ([a], [a])
halve xs = (take k xs, drop k xs)
  where k = length xs `div` 2
  
halve' xs = splitAt (length xs `div` 2) xs

---------------------------------------
-- Exercise 2

third :: [a] -> a
third xs = head (tail (tail xs))

third' xs = xs !! 2

third'' (_ : _ : x : _) = x

---------------------------------------
-- Exercise 3

safetail xs = if null xs then [] else tail xs
              
safetail' xs 
  | null xs = []
  | otherwise = tail xs

  
safetail'' [] = []
safetail'' (_ : xs) = xs

---------------------------------------
-- Exercise 4

or True True = True
or True False = True
or False True = True
or False False = False

or' False False = False
or' _ _ = True

or'' True _ = True
or'' False a = a

or''' b c
  | b == c = b
  | otherwise = True
  
---------------------------------------
-- Exercise 5

and' x y = 
  if x then 
    if y then True else False
  else False

---------------------------------------  
-- Exercise 6

and'' x y = if x then y else False

---------------------------------------
-- Exercise 7

mult' :: Int -> Int -> Int -> Int
mult' = \x -> (\y -> \z -> (x * y * z))

---------------------------------------
-- Exercise 8
luhnDouble :: Int -> Int
luhnDouble x = if k > 9 then k - 9 else k
  where k =  x * 2


luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z k = total `mod` 10 == 0
  where total = luhnDouble x + y + luhnDouble z + k

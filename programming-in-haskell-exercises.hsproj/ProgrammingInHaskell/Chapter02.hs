module Chapter02 where
 
---------------------------------------
-- Exercise 3

n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

---------------------------------------      
-- Exercise 4

last' xs = head (reverse xs)

last'' xs = head (drop (length xs - 1) xs)

last''' xs = xs !! (length xs - 1)

---------------------------------------
-- Exercise 5

init'' xs = take (length xs - 1) xs

init''' xs = reverse (tail (reverse xs))

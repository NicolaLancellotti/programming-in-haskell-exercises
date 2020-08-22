module Chapter07 where
  
import Data.Char
  
---------------------------------------
-- Exercise 2

all' p = and . map p

any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs) 
  | p x = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x : xs) 
  | p x = dropWhile p xs
  | otherwise = xs
                      
---------------------------------------
-- Exercise 3

map' f = foldr (\x xs -> f x : xs) []

filter' p = foldr (\x xs -> if p x then x : xs else xs ) []

---------------------------------------
-- Exercise 4

dec2int = foldl (\v x -> v * 10 + x) 0

---------------------------------------
-- Exercise 5

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y 

---------------------------------------
-- Exercise 6

unfold p h t x 
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop8 = unfold (null) (take 8) (drop 8)

map'' f = unfold (null) (f . head) (drop 1)

interate' f = unfold (\_ -> False) id f

---------------------------------------
-- Exercise 7

type Bit = Int
type Channel = [Bit] -> [Bit]

-- Parity
checkParity = even . length . filter (==1)

addParity xs = if checkParity xs then 0 : xs else 1 : xs

removeParity xs = if checkParity xs then drop 1 xs else error "parity error"
  
-- Encode
int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

-- Decode
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

bin2int :: [Int] -> Int 
bin2int bits = sum [w * b | (w, b) <- zip wights bits]
  where wights = iterate (*2) 1
  
decode :: [Bit] -> String
decode = map (chr . bin2int . removeParity) . chop9

-- transmit
transmit :: Channel -> String -> String
transmit channel = decode . channel . encode

channel :: Channel
channel = id

---------------------------------------
-- Exercise 8

faultyChannel :: Channel
faultyChannel = tail

---------------------------------------
-- Exercise 9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : (altMap g f xs)

---------------------------------------
-- Exercise 10

luhnDouble :: Int -> Int
luhnDouble x = if k > 9 then k - 9 else k
  where k =  x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z k = total `mod` 10 == 0
  where total = luhnDouble x + y + luhnDouble z + k 
  

luhn' :: [Int] -> Bool
luhn' xs = total `mod` 10 == 0
  where total = (sum . altMap luhnDouble id) xs

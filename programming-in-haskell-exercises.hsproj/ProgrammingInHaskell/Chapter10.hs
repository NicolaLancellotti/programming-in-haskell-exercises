module Chapter10 where
  
import System.IO

---------------------------------------
-- Exercise 1

putStr' :: String -> IO ()
putStr' s = sequence_ [putChar c | c <- s]

---------------------------------------
-- Exercise 2

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))


putBoard :: Board -> IO ()
putBoard = putBoardRec 1 
  where 
    putBoardRec _ [] = return()
    putBoardRec n (x : xs) = do putRow n x
                                putBoardRec (n + 1) xs 
                       
---------------------------------------
-- Exercise 3

putBoard' :: Board -> IO ()
putBoard' xs = sequence_ [putRow n x | (n, x) <- zip [1..] xs]

---------------------------------------
-- Exercise 4

readInt :: IO Int
readInt = do line <- getLine
             return (read line ::Int)
            
readAndSum :: Int -> Int -> IO (Int)
readAndSum total 0 = return total
readAndSum total n = do value <- readInt
                        readAndSum (total + value) (n - 1)

adder :: IO ()
adder = do  putStr "How many numbers? "
            hFlush stdout
            count <- readInt
            sum <- readAndSum 0 count
            putStrLn ("The total is " ++ (show sum))
    
---------------------------------------
-- Exercise 5

adder' :: IO ()
adder' = do putStr "How many numbers? "
            hFlush stdout
            count <- readInt
            list <- sequence [readInt | _ <- [1..count]]
            putStrLn ("The total is " ++ (show (sum list)))
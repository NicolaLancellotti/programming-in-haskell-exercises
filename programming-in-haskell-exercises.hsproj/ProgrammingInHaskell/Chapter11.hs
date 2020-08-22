module Chapter11 where
  
-- Basic declarations

import Data.Char
import Data.List
import System.IO
import System.Random hiding (next) -- Exercise 2
import qualified Data.Map as Map -- Exercise 4C

data Exercise = EX_None | EX_2 | EX_3 -- Exercise 2 & 3
ex :: Exercise
ex = EX_None

ex4A :: Bool -- Exercise 4A
ex4A = False

lengthWinningLine :: Int -- Exercise 4B
lengthWinningLine = 3

ex4C :: Bool -- Exercise 4C
ex4C = False

ex4D :: Bool -- Exercise 4D
ex4D = False

size :: Int
size = 3

type Grid = [[Player]]

data Player = AlphaInit | X | B | O | BetaInit
  deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

-- Grid utilities

empty :: Grid 
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    -- Exercise 4B
    line = isSublistOf (take lengthWinningLine (repeat p))
    -- line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- Displaying a grid

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

-- Making a move

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move:: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Reading a natural number

getNat :: String -> IO Int
getNat prompt = do 
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs then
    return (read xs)
  else do 
    putStrLn "ERROR: Invalid number"
    getNat prompt

-- Human vs human

tictactoe :: IO ()
tictactoe = run empty O 

run :: Grid -> Player -> IO ()
run g p = do 
  cls
  goto (1,1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p 
  | wins O g  = putStrLn "Player O wins!\n"
  | wins X g  = putStrLn "Player X wins!\n"
  | full g    = putStrLn "It's a draw!\n"
  | otherwise = do 
    i <- getNat (prompt p)
    case move g i p of
      []   -> do 
            putStrLn "ERROR: Invalid move"
            run' g p
      [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Game trees

data Tree a = Node a [Tree a]
  deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p 
  | won g     = []
  | full g    = []
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

-- Minimax

minimax :: Tree Grid -> Tree (Grid, Player) -- Exercise 4D
minimax = if ex4D then minimax_ex4d else minimax'

minimax' :: Tree Grid -> Tree (Grid, Player)
minimax' (Node g [])
  | wins O g  = Node (g, O) []
  | wins X g  = Node (g, X) []
  | otherwise = Node (g, B) []
minimax' (Node g ts) 
  | turn g == O = Node (g, maximum ps) ts'
  | turn g == X = Node (g, minimum ps) ts'
    where
      ts' :: [Tree (Grid, Player)]
      ts' = map minimax' ts
      ps :: [Player]
      ps  = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where 
    tree = if ex4C then do
      getGameTree g
    else
      prune depth (gametree g p)
    Node (_, best) ts = minimax tree

-- Human vs computer

play :: Grid -> Player -> Player -> IO () -- Exercise 4A
play g p human = do  
  cls
  goto (1,1)
  putGrid g
  play' g p human
             
play' :: Grid -> Player -> Player -> IO () -- Exercise 4A
play' g p human
  | wins O g  = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == human = do 
    i <- getNat (prompt p)
    case move g i p of
      []   -> do 
        putStrLn "ERROR: Invalid move"
        play' g p human
      [g'] -> play g' (next p) human
  | otherwise   = do  
    putStr "Player "
    putStr (show p) -- Exercise 4A
    putStr " is thinking... "
    case ex of
      EX_None -> do
        let bestMove = bestmove g p
        (play $! bestMove) (next p) human
      -- Exercise 2
      EX_2 -> do
        bestMove <- bestmove_ex2 g p
        (play $! bestMove) (next p) human
      -- Exercise 3
      EX_3 -> do
        let bestMove = bestmove_ex3 g p
        (play $! bestMove) (next p) human

main :: IO () -- Exercise 4A
main = do
  hSetBuffering stdout NoBuffering
  if ex4A then do
    human <- getHuman
    play empty O human
  else
    play empty O O

---------------------------------------
-- Exercise 1

nodes :: Tree a -> Int
nodes (Node _ ts) = 1 + sum (map nodes ts)

mydepth :: Tree a -> Int
mydepth (Node _ []) = 0
mydepth (Node _ ts) = 1 + maximum (map mydepth ts)

---------------------------------------
-- Exercise 2

bestmove_ex2 :: Grid -> Player -> IO Grid
bestmove_ex2 g p = do
  let gs = bestmoves g p
  n <- randomRIO(0, length gs - 1)
  return (gs !! n)
    where
      bestmoves :: Grid -> Player -> [Grid]
      bestmoves g p = [g' | Node (g',p') _ <- ts, p' == best]
        where
          tree = if ex4C then do
            getGameTree g
          else
            prune depth (gametree g p)
          Node (_, best) ts = minimax tree

---------------------------------------
-- Exercise 3

bestmove_ex3 :: Grid -> Player -> Grid
bestmove_ex3 g p =  bestGrid
  where
  tree = if ex4C then do
    getGameTree g
  else
    prune depth (gametree g p)
  Node (_, best) ts = minimax tree

  isBest :: Tree (Grid, Player) -> Bool
  isBest (Node (g, p) _) = p == best

  removePlayer :: Tree (Grid, Player) -> Tree (Grid)
  removePlayer (Node (g, p) ts) = Node g (map removePlayer ts)

  bests :: [Tree Grid]
  bests = map removePlayer (filter isBest ts)

  bestsAndDepths :: [(Tree Grid, Int)]
  bestsAndDepths = map (\x -> (x, mydepth x)) bests

  findBest :: ((Tree Grid, Int) -> (Tree Grid, Int) -> (Tree Grid, Int))
  findBest (g1, d1) (g2, d2) = if d1 < d2 then (g1, d1) else (g2, d2)

  (Node bestGrid _, _) = foldr1 findBest bestsAndDepths

---------------------------------------
-- Exercise 4A

getHuman :: IO Player
getHuman = do
  putStr "Do you want to play first? (y/n) "
  xs <- getLine
  case xs of
    "y" -> return O
    "n" -> return X
    _ -> do 
      putStrLn "ERROR"
      getHuman

---------------------------------------
-- Exercise 4B

isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf [] list = True
isSublistOf _ [] = False
isSublistOf (x:xs) (y:ys)
  | x == y = isPrefixOf xs ys || isSublistOf (x:xs) ys
  | otherwise = isSublistOf (x:xs) ys

---------------------------------------
-- Exercise 4C

mapGameTree :: Map.Map Grid (Tree Grid) 
mapGameTree = Map.fromList listTuple
  where
    reduceToList :: [Tree Grid] -> [Tree Grid]
    reduceToList [] = [] 
    reduceToList (Node g ts:tts) = [Node g ts] ++ reduceToList ts ++ reduceToList tts

    tree = gametree empty O
    list = reduceToList [tree]
    listTuple = map (\(Node g ts) -> (g, Node g ts)) list

getGameTree:: Grid -> Tree Grid
getGameTree g = case Map.lookup g mapGameTree of
  Just tree -> tree

---------------------------------------
-- Exercise 4D

minimax_ex4d :: Tree Grid -> Tree (Grid, Player)
minimax_ex4d t = alphaBetaSearch t AlphaInit BetaInit where
  alphaBetaSearch (Node g []) _ _ 
    | wins O g  = Node (g, O) []
    | wins X g  = Node (g, X) []
    | otherwise = Node (g, B) []
  alphaBetaSearch (Node g ts) alpha beta = Node (g, newValue) newTreeGP
    where
      isMaxTurn = turn g == O
      valueInit = if isMaxTurn then AlphaInit else BetaInit

      foldlStop f v [] = v
      foldlStop f v (x:xs) = case v of
        (True, t) -> v
        (False, t) -> foldlStop f (f v x) xs
        
      (_, (newValue, _, _, newTreeGP)) = 
        foldlStop f (False, (valueInit, alpha, beta, [])) ts
          where
            f (False, (value, alpha, beta, treeGP)) tree
              | isMaxTurn && newValue >= beta =      
                (True, (newValue, alpha, beta,newTreeGP))
              | isMaxTurn =                          
                (False, (newValue, max alpha newValue, beta, newTreeGP))
              | not isMaxTurn && newValue <= alpha = 
                (True, (newValue, alpha, beta, newTreeGP))
              | not isMaxTurn =                      
                (False, (newValue, alpha, min beta newValue, newTreeGP))
              where
                Node (g, best) ts =  alphaBetaSearch tree alpha beta
                newValue = (if isMaxTurn then max else min) value best
                newTreeGP = treeGP ++ [(Node (g, best) ts)]

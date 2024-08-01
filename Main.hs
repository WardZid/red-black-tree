{-# LANGUAGE FlexibleContexts #-}

import System.IO (readFile)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Control.Exception (evaluate, catch, SomeException)

type Element = Float

data Color = R | B deriving (Show, Eq)

data Tree a = E | T Color (Tree a) a (Tree a) deriving (Show, Eq)

type RBTree = Tree Element

-- Insert a value into the Red-Black Tree
insert :: Ord a => a -> Tree a -> Tree a
insert x s = T B a z b
  where
    T _ a z b = ins s
    ins E = T R E x E
    ins s@(T B a y b)
      | x < y = balance (ins a) y b
      | x > y = balance a y (ins b)
      | otherwise = s
    ins s@(T R a y b)
      | x < y = T R (ins a) y b
      | x > y = T R a y (ins b)
      | otherwise = s

-- Membership test in the Red-Black Tree
member :: Ord a => a -> Tree a -> Bool
member x E = False
member x (T _ a y b)
  | x < y = member x a
  | x > y = member x b
  | otherwise = True

-- Balance the Red-Black Tree
balance :: Tree a -> a -> Tree a -> Tree a
balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance a x b = T B a x b

-- Delete a value from the Red-Black Tree
delete :: Ord a => a -> Tree a -> Tree a
delete x t =
  case del t of
    T _ a y b -> T B a y b
    _ -> E
  where
    del E = E
    del (T _ a y b)
      | x < y = delformLeft a y b
      | x > y = delformRight a y b
      | otherwise = app a b
    delformLeft a@(T B _ _ _) y b = balleft (del a) y b
    delformLeft a y b = T R (del a) y b
    delformRight a y b@(T B _ _ _) = balright a y (del b)
    delformRight a y b = T R a y (del b)

-- Balancing functions
balleft :: Tree a -> a -> Tree a -> Tree a
balleft (T R a x b) y c = T R (T B a x b) y c
balleft bl x (T B a y b) = balance bl x (T R a y b)
balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance b z c)

balright :: Tree a -> a -> Tree a -> Tree a
balright a x (T R b y c) = T R a x (T B b y c)
balright (T B a x b) y bl = balance (T R a x b) y bl
balright (T R a x (T B b y c)) z bl = T R (balance (sub1 a) x b) y (T B c z bl)

sub1 :: Tree a -> Tree a
sub1 (T B a x b) = T R a x b
sub1 _ = error "invariance violation"

-- Append two Red-Black Trees
app :: Tree a -> Tree a -> Tree a
app E x = x
app x E = x
app (T R a x b) (T R c y d) =
  case app b c of
    T R b' z c' -> T R (T R a x b') z (T R c' y d)
    bc -> T R a x (T R bc y d)
app (T B a x b) (T B c y d) =
  case app b c of
    T R b' z c' -> T R (T B a x b') z (T B c' y d)
    bc -> balleft a x (T B bc y d)
app a (T R b x c) = T R (app a b) x c
app (T R a x b) c = T R a x (app b c)

-- Optimized union of two Red-Black Trees
optimizedUnion :: Ord a => Tree a -> Tree a -> Tree a
optimizedUnion E t = t
optimizedUnion t E = t
optimizedUnion t1@(T c1 l1 x r1) t2@(T c2 l2 y r2)
  | x < y = balanceL c1 l1 x (optimizedUnion r1 t2)
  | x > y = balanceR (optimizedUnion t1 l2) y c2 r2
  | otherwise = balanceB (optimizedUnion l1 l2) x (optimizedUnion r1 r2)

-- Balancing functions adapted for union
balanceL :: Color -> Tree a -> a -> Tree a -> Tree a
balanceL B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balanceL B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balanceL c l x r = T c l x r

balanceR :: Tree a -> a -> Color -> Tree a -> Tree a
balanceR a x B (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balanceR a x B (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balanceR l x c r = T c l x r

balanceB :: Tree a -> a -> Tree a -> Tree a
balanceB = T B

-- Intersection of two Red-Black Trees
intersection :: Ord a => Tree a -> Tree a -> Tree a
intersection E _ = E
intersection _ E = E
intersection t1@(T c1 l1 x r1) t2
  | member x t2 = balance (intersection l1 t2) x (intersection r1 t2)
  | otherwise = app (intersection l1 t2) (intersection r1 t2)

-- Difference of two Red-Black Trees
difference :: Ord a => Tree a -> Tree a -> Tree a
difference t E = t
difference E _ = E
difference t1@(T c1 l1 x r1) t2@(T c2 l2 y r2)
  | x < y = balanceL c1 (difference l1 t2) x r1
  | x > y = balanceR l1 x c1 (difference r1 t2)
  | otherwise = merge (difference l1 l2) (difference r1 r2)

-- Merge two Red-Black Trees
merge :: Ord a => Tree a -> Tree a -> Tree a
merge E t = t
merge t E = t
merge t1@(T c1 l1 x r1) t2@(T c2 l2 y r2)
  | x < y = balanceL c1 l1 x (merge r1 t2)
  | x > y = balanceR (merge t1 l2) y c2 r2
  | otherwise = balanceB (merge l1 l2) x (merge r1 r2)

-- Convert a list of floats to a Red-Black Tree
listToRBTree :: [Element] -> RBTree
listToRBTree = foldl' (flip insert) E

-- Read matrix from file and convert to a list of floats
readMatrixFromFile :: FilePath -> IO [[Element]]
readMatrixFromFile filePath = do
  content <- readFile filePath
  let rows = map (mapMaybe readMaybe . wordsWhen (== ',')) (lines content)
  return rows

-- Utility function to split a string by a delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

flatten :: Tree a -> [a]
flatten E = []
flatten (T _ l x r) = flatten l ++ [x] ++ flatten r

-- Helper function to get the size of a tree
treeSize :: Tree a -> Int
treeSize E = 0
treeSize (T _ l _ r) = 1 + treeSize l + treeSize r

main :: IO ()
main = do
  let filePath = "data_struct(1000x1000).txt"
  
  -- Read the file and parse the matrix
  putStrLn "Reading and parsing the file..."
  rows <- readMatrixFromFile filePath
  putStrLn $ "Parsed " ++ show (length rows) ++ " rows."

  -- Ensure rows are valid
  putStrLn "Validating parsed rows..."
  evaluate (length rows) >>= print

  -- Create trees from each row
  putStrLn "Creating trees from each row..."
  let trees = map listToRBTree rows
  putStrLn $ "Created " ++ show (length trees) ++ " trees."

  -- Perform optimized union of all trees
  putStrLn "Performing optimized union of all trees..."
  let unionWithDebug acc tree =
        let result = optimizedUnion acc tree
        in result
      allUnion = foldl' unionWithDebug E trees
  putStrLn "Optimized union of all trees completed."

  -- Perform intersection of all trees
  putStrLn "Performing intersection of all trees..."
  let intersectionWithDebug acc tree =
        let result = intersection acc tree
        in result
      allIntersection = foldl' intersectionWithDebug (head trees) (tail trees)
  putStrLn "Intersection of all trees completed."

  -- Perform difference of all trees
  putStrLn "Performing difference of all trees..."
  let differenceWithDebug acc tree =
        let result = difference acc tree
        in result
      allDifference = foldl' differenceWithDebug (head trees) (tail trees)
  putStrLn "Difference of all trees completed."

  -- Flatten the union tree
  let unionList = flatten allUnion
  putStrLn $ "Flattened union tree has " ++ show (length unionList) ++ " elements."

  -- Print the first 10 elements of the union tree
  putStrLn "First 10 elements of the union tree:"
  let first10Elements = take 10 unionList
  mapM_ print first10Elements

  -- Ensure the union tree is flattened correctly
  evaluate (length first10Elements) >>= print

  -- Delete all elements from the unioned tree
  putStrLn "Deleting all elements from the unioned tree..."
  let _ = foldl' (flip delete) allUnion unionList
  putStrLn "All elements deleted."

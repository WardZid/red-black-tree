module Main where

import System.IO (readFile)
import Data.Char (isDigit, isSpace)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import System.CPUTime
import Text.Printf

-- Type alias for elements in the Red-Black Tree
type Element = Float

-- Define Color type for Red-Black Tree nodes
data Color = R | B deriving (Show, Eq)

-- Define Red-Black Tree data structure
data Tree a = E | T Color (Tree a) a (Tree a) deriving (Show, Eq)

-- Type alias for the Red-Black Tree
type RBTree = Tree Element

-- Insert a value into the Red-Black Tree
-- Time Complexity: O(log n)
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
-- Time Complexity: O(log n)
member :: Ord a => a -> Tree a -> Bool
member x E = False
member x (T _ a y b)
  | x < y = member x a
  | x > y = member x b
  | otherwise = True

-- Balance the Red-Black Tree
-- Time Complexity: O(1)
balance :: Tree a -> a -> Tree a -> Tree a
balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance a x b = T B a x b

-- Delete a value from the Red-Black Tree
-- Time Complexity: O(log n)
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
-- Time Complexity: O(1) for each function
balleft :: Tree a -> a -> Tree a -> Tree a
balleft (T R a x b) y c = T R (T B a x b) y c
balleft bl x (T B a y b) = balance bl x (T R a y b)
balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance b z c)

balright :: Tree a -> a -> Tree a -> Tree a
balright a x (T R b y c) = T R a x (T B b y c)
balright (T B a x b) y bl = balance (T R a x b) y bl
balright (T R a x (T B b y c)) z bl = T R (balance (sub1 a) x b) y (T B c z bl)

-- Convert a black tree to a red tree
-- Time Complexity: O(1)
sub1 :: Tree a -> Tree a
sub1 (T B a x b) = T R a x b
sub1 _ = error "invariance violation"

-- Append two Red-Black Trees
-- Time Complexity: O(log n)
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

-- Union of two Red-Black Trees
-- Time Complexity: O(m log(n/m + 1))
union :: Ord a => Tree a -> Tree a -> Tree a
union E t = t
union (T _ a x b) t = union b (a `union` insert x t)

-- Intersection of two Red-Black Trees
-- Time Complexity: O(m log(n/m + 1))
intersection :: Ord a => Tree a -> Tree a -> Tree a
intersection E _ = E
intersection _ E = E
intersection t1@(T _ a1 x1 b1) t2
  | member x1 t2 = insert x1 (a1 `intersection` t2 `union` b1 `intersection` t2)
  | otherwise = a1 `intersection` t2 `union` b1 `intersection` t2

-- Difference of two Red-Black Trees
-- Time Complexity: O(m log(n/m + 1))
difference :: Ord a => Tree a -> Tree a -> Tree a
difference E _ = E
difference t E = t
difference t1@(T _ a1 x1 b1) t2
  | member x1 t2 = (a1 `union` b1) `difference` t2
  | otherwise = insert x1 (a1 `difference` t2 `union` b1 `difference` t2)

-- Convert a list of floats to a Red-Black Tree
-- Time Complexity: O(n log n) for inserting n elements
listToRBTree :: [Element] -> RBTree
listToRBTree = foldl' (flip insert) E

-- Read matrix from file and convert to a list of floats
-- Time Complexity: O(n) for reading n elements from file and parsing
readMatrixFromFile :: FilePath -> IO [Element]
readMatrixFromFile filePath = do
  content <- readFile filePath
  let cleaned = filter (\c -> isDigit c || c == '.' || c == ',' || isSpace c) content
      numbers = mapMaybe (readMaybe . filter (`elem` ".0123456789")) (words cleaned) :: [Float]
  return numbers

-- Time measurement utility
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main :: IO ()
main = do
  let filePath = "data_struct(1000x1000).txt"
  
  -- Read the file and parse the matrix
  numbers <- readMatrixFromFile filePath
  putStrLn "Building the tree..."
  tree <- time $ return $ listToRBTree numbers
  
  -- Perform and print various operations
  putStrLn "Red-Black Tree:"
  print tree
  
  putStrLn "Member check for 5.0:"
  time $ print $ member 5.0 tree
  
  putStrLn "Deleting 5.0:"
  tree' <- time $ return $ delete 5.0 tree
  print tree'
  
  let tree1 = listToRBTree [1.0, 2.0, 3.0]
  let tree2 = listToRBTree [3.0, 4.0, 5.0]
  putStrLn "Union of tree1 and tree2:"
  time $ print $ tree1 `union` tree2
  
  putStrLn "Intersection of tree1 and tree2:"
  time $ print $ tree1 `intersection` tree2
  
  putStrLn "Difference of tree1 and tree2:"
  time $ print $ tree1 `difference` tree2

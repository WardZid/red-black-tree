module Main where

import System.IO (readFile)
import Data.Char (isDigit, isSpace)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Color = R | B deriving (Show, Eq)
data RB a = E | T Color (RB a) a (RB a) deriving (Show, Eq)

-- Insert a value into the Red-Black Tree
insert :: Ord a => a -> RB a -> RB a
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
member :: Ord a => a -> RB a -> Bool
member x E = False
member x (T _ a y b)
  | x < y = member x a
  | x > y = member x b
  | otherwise = True

-- Balance the Red-Black Tree
balance :: RB a -> a -> RB a -> RB a
balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance a x b = T B a x b

-- Delete a value from the Red-Black Tree
delete :: Ord a => a -> RB a -> RB a
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
balleft :: RB a -> a -> RB a -> RB a
balleft (T R a x b) y c = T R (T B a x b) y c
balleft bl x (T B a y b) = balance bl x (T R a y b)
balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance b z c)

balright :: RB a -> a -> RB a -> RB a
balright a x (T R b y c) = T R a x (T B b y c)
balright (T B a x b) y bl = balance (T R a x b) y bl
balright (T R a x (T B b y c)) z bl = T R (balance a x b) y (T B c z bl)

sub1 :: RB a -> RB a
sub1 (T B a x b) = T R a x b
sub1 _ = error "invariance violation"

app :: RB a -> RB a -> RB a
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
union :: Ord a => RB a -> RB a -> RB a
union E t = t
union (T _ a x b) t = union b (a `union` insert x t)

-- Intersection of two Red-Black Trees
intersection :: Ord a => RB a -> RB a -> RB a
intersection E _ = E
intersection _ E = E
intersection t1@(T _ a1 x1 b1) t2
  | member x1 t2 = insert x1 (a1 `intersection` t2 `union` b1 `intersection` t2)
  | otherwise = a1 `intersection` t2 `union` b1 `intersection` t2

-- Difference of two Red-Black Trees
difference :: Ord a => RB a -> RB a -> RB a
difference E _ = E
difference t E = t
difference t1@(T _ a1 x1 b1) t2
  | member x1 t2 = (a1 `union` b1) `difference` t2
  | otherwise = insert x1 (a1 `difference` t2 `union` b1 `difference` t2)

-- Convert a list of floats to a Red-Black Tree
listToRBTree :: [Float] -> RB Float
listToRBTree = foldl' (flip insert) E

-- Read matrix from file and convert to a list of floats
readMatrixFromFile :: FilePath -> IO [Float]
readMatrixFromFile filePath = do
  content <- readFile filePath
  let cleaned = filter (\c -> isDigit c || c == '.' || c == ',' || isSpace c) content
      numbers = mapMaybe (readMaybe . filter (`elem` ".0123456789")) (words cleaned) :: [Float]
  return numbers

main :: IO ()
main = do
  let filePath = "data_struct(1000x1000).txt"
  
  -- Read the file and parse the matrix
  numbers <- readMatrixFromFile filePath
  let tree = listToRBTree numbers
  
  -- Perform and print various operations
  putStrLn "Red-Black Tree:"
  print tree
  
  putStrLn "Member check for 5.0:"
  print $ member 5.0 tree
  
  putStrLn "Deleting 5.0:"
  let tree' = delete 5.0 tree
  print tree'
  
  let tree1 = listToRBTree [1.0, 2.0, 3.0]
  let tree2 = listToRBTree [3.0, 4.0, 5.0]
  putStrLn "Union of tree1 and tree2:"
  print $ tree1 `union` tree2
  
  putStrLn "Intersection of tree1 and tree2:"
  print $ tree1 `intersection` tree2
  
  putStrLn "Difference of tree1 and tree2:"
  print $ tree1 `difference` tree2

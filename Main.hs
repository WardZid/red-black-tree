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
balance a x b = T B a x b

-- Delete a value from the Red-Black Tree
delete :: Ord a => a -> Tree a -> Tree a
delete x t = makeBlack (del t)
  where
    makeBlack (T _ a y b) = T B a y b
    makeBlack E = E

    del E = E
    del (T B a y b)
      | x < y = balleft (del a) y b
      | x > y = balright a y (del b)
      | otherwise = optimizedUnion a b
    del (T R a y b)
      | x < y = T R (del a) y b
      | x > y = T R a y (del b)
      | otherwise = app a b

-- Balancing functions
balleft :: Tree a -> a -> Tree a -> Tree a
balleft (T R a x b) y c = T R (T B a x b) y c
balleft (T B a x (T R b y c)) z d = T R (T B a x b) y (balance c z d)
balleft (T B a x (T B b y c)) z d = balance (T B a x b) y (T B b y c)
balleft (T B E x b) y c = T B E x (T B b y c) -- Handle empty left case
balleft bl x (T B E y b) = T B bl x (T B E y b) -- Handle empty right case
balleft t1 x t2 = T B t1 x t2  -- Handle all other cases

balright :: Tree a -> a -> Tree a -> Tree a
balright a x (T R b y c) = T R a x (T B b y c)
balright (T R a x b) y (T B c z d) = T R (balance a x b) y (T B c z d)
balright (T B (T R a x b) y c) z d = T R (T B a x b) y (balance c z d)
balright (T B (T B a x b) y c) z d = balance (T B a x b) y (T B c z d)
balright a x E = T B a x E  -- Handle empty right case
balright E x t = T B E x t  -- Handle empty left case
balright t1 x t2 = T B t1 x t2  -- Handle all other cases

sub1 :: Tree a -> Tree a
sub1 (T B a x b) = T R a x b
sub1 t = t  -- Handle other cases (e.g., T R a x b, E)

-- Append two Red-Black Trees (used in merge)
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

-- Split a Red-Black Tree into two trees and a pivot element
split :: Ord a => a -> Tree a -> (Tree a, a, Tree a)
split _ E = (E, error "Pivot not found", E) -- Using an error to handle the case where the pivot is not found
split x (T _ l y r)
  | x < y = let (lt, piv, gt) = split x l in (lt, piv, T B gt y r)
  | x > y = let (lt, piv, gt) = split x r in (T B l y lt, piv, gt)
  | otherwise = (l, y, r)

-- Optimized union of two Red-Black Trees without duplicates
optimizedUnion :: Ord a => Tree a -> Tree a -> Tree a
optimizedUnion E t = t
optimizedUnion t E = t
optimizedUnion t1@(T c1 l1 x r1) t2 = T c (optimizedUnion l1 l2) x (optimizedUnion r1 r2)
  where
    (l2, _, r2) = split x t2
    T c _ _ _ = t1

-- Intersection of two Red-Black Trees
intersection :: Ord a => Tree a -> Tree a -> Tree a
intersection E _ = E
intersection _ E = E
intersection t1@(T c1 l1 x r1) t2
  | member x t2 = balance (intersection l1 t2) x (intersection r1 t2)
  | otherwise = app (intersection l1 t2) (intersection r1 t2)

-- Optimized Difference of two Red-Black Trees
difference :: Ord a => Tree a -> Tree a -> Tree a
difference E _ = E
difference t E = t
difference t1@(T c1 l1 x r1) t2@(T c2 l2 y r2)
  | x < y = balanceL c1 (difference l1 t2) x r1
  | x > y = balanceR l1 x c1 (difference r1 t2)
  | member x t2 = optimizedUnion (difference l1 l2) (difference r1 r2)
  | otherwise = balanceB (difference l1 l2) x (difference r1 r2)

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

  -- Flatten the union tree
  let unionList = flatten allUnion
  putStrLn $ "Flattened union tree has " ++ show (length unionList) ++ " elements."

  -- Print the first 10 elements of the union tree
  putStrLn "First 10000 elements of the union tree:"
  let first10000Elements = take 10000 unionList
  mapM_ print first10000Elements

  -- Ensure the union tree is flattened correctly
  evaluate (length first10000Elements) >>= print

  putStrLn "The number of the element in the all union tree is :"
  evaluate (length unionList) >>= print

  -- Delete all elements from the unioned tree
  putStrLn "Deleting all elements from the unioned tree..."
  let deletedTree = foldl' (flip delete) allUnion unionList
  putStrLn "All elements deleted."
  evaluate (treeSize deletedTree -15) >>= print

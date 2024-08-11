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

delete :: Ord a => a -> Tree a -> Tree a
delete k t@(T c l x r) = join l r
  where
    (l, _, r) = split k t

-- Append two Red-Black Trees
join :: Tree a -> Tree a -> Tree a
join E x = x
join x E = x
join (T R a x b) (T R c y d) =
  case join b c of
    T R b' z c' -> T R (T R a x b') z (T R c' y d)
    bc -> T R a x (T R bc y d)
join (T B a x b) (T B c y d) =
  case join b c of
    T R b' z c' -> T R (T B a x b') z (T B c' y d)
    bc -> balance a x (T B bc y d)
join a (T R b x c) = T R (join a b) x c
join (T R a x b) c = T R a x (join b c)

-- Split a Red-Black Tree into two trees and a pivot element
split :: Ord a => a -> Tree a -> (Tree a, a, Tree a)
split _ E = (E, error "Pivot not found", E) -- Using an error to handle the case where the pivot is not found
split x (T _ l y r)
  | x < y = let (lt, piv, gt) = split x l in (lt, piv, T B gt y r)
  | x > y = let (lt, piv, gt) = split x r in (T B l y lt, piv, gt)
  | otherwise = (l, y, r)

-- Optimized union of two Red-Black Trees without duplicates
optimizedUnion :: Ord a => Tree a -> Tree a -> Tree a
optimizedUnion t1 t2
  | treeSize t1 > treeSize t2 = unionHelper t1 t2
  | otherwise = unionHelper t2 t1

-- Helper function where t1 is the larger tree and t2 is the smaller tree
unionHelper :: Ord a => Tree a -> Tree a -> Tree a
unionHelper E t = t
unionHelper t E = t
unionHelper t1@(T c1 l1 x r1) t2 =
  let (l2, _, r2) = split x t2
  in balance (unionHelper l1 l2) x (unionHelper r1 r2)

-- Intersection of two Red-Black Trees
intersection :: Ord a => Tree a -> Tree a -> Tree a
intersection E _ = E
intersection _ E = E
intersection t1@(T c1 l1 x r1) t2
  | member x t2 = balance (intersection l1 t2) x (intersection r1 t2)
  | otherwise = join (intersection l1 t2) (intersection r1 t2)

-- Optimized Difference of two Red-Black Trees
difference :: Ord a => Tree a -> Tree a -> Tree a
difference E _ = E
difference t E = t
difference t1@(T c1 l1 x r1) t2
  | member x t2 = join (difference l1 t2) (difference r1 t2)
  | otherwise = balance (difference l1 t2) x (difference r1 t2)


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
  evaluate (treeSize deletedTree) >>= print

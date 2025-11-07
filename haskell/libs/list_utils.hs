-- List utility functions library for Haskell
module ListUtils
  ( chunk
  , splitAt'
  , takeWhile'
  , dropWhile'
  , groupBy'
  , unique
  , frequencies
  , rotate
  , interleave
  , flatten
  , cartesianProduct
  , permutations'
  , combinations
  , partition'
  , span'
  , break'
  , stripPrefix'
  , stripSuffix'
  , isPrefixOf'
  , isSuffixOf'
  ) where

import Data.List (group, sort)
import qualified Data.List as L

-- | Split list into chunks of size n
-- >>> chunk 3 [1,2,3,4,5,6,7,8]
-- [[1,2,3],[4,5,6],[7,8]]
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- | Split list at index
-- >>> splitAt' 3 [1,2,3,4,5]
-- ([1,2,3],[4,5])
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' = splitAt

-- | Take elements while predicate is true
-- >>> takeWhile' (< 5) [1,2,3,4,5,6,7]
-- [1,2,3,4]
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = takeWhile

-- | Drop elements while predicate is true
-- >>> dropWhile' (< 5) [1,2,3,4,5,6,7]
-- [5,6,7]
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' = dropWhile

-- | Group consecutive elements by predicate
-- >>> groupBy' (\a b -> a == b) [1,1,2,2,3,3,3]
-- [[1,1],[2,2],[3,3,3]]
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' eq (x:xs) = (x : ys) : groupBy' eq zs
  where
    (ys, zs) = span (eq x) xs

-- | Remove duplicate elements
-- >>> unique [1,2,2,3,3,3,4]
-- [1,2,3,4]
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

-- | Count frequency of each element
-- >>> frequencies [1,1,2,2,2,3]
-- [(1,2),(2,3),(3,1)]
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies xs = map (\g -> (head g, length g)) . group . sort $ xs

-- | Rotate list left by n positions
-- >>> rotate 2 [1,2,3,4,5]
-- [3,4,5,1,2]
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = drop n' xs ++ take n' xs
  where n' = n `mod` length xs

-- | Interleave two lists
-- >>> interleave [1,2,3] [4,5,6]
-- [1,4,2,5,3,6]
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- | Flatten nested list
-- >>> flatten [[1,2],[3,4],[5]]
-- [1,2,3,4,5]
flatten :: [[a]] -> [a]
flatten = concat

-- | Cartesian product of two lists
-- >>> cartesianProduct [1,2] ['a','b']
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

-- | Generate all permutations of a list
-- >>> permutations' [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [y:ys | y <- xs, ys <- permutations' (delete' y xs)]
  where
    delete' _ [] = []
    delete' y (x:xs')
      | x == y = xs'
      | otherwise = x : delete' y xs'

-- | Generate all combinations of size k
-- >>> combinations 2 [1,2,3,4]
-- [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- | Partition list by predicate
-- >>> partition' even [1,2,3,4,5,6]
-- ([2,4,6],[1,3,5])
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' p (x:xs)
  | p x = (x:ys, ns)
  | otherwise = (ys, x:ns)
  where (ys, ns) = partition' p xs

-- | Split list into prefix where predicate is true and remainder
-- >>> span' (< 5) [1,2,3,4,5,6,7]
-- ([1,2,3,4],[5,6,7])
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' = span

-- | Split list at first element where predicate is true
-- >>> break' (> 3) [1,2,3,4,5]
-- ([1,2,3],[4,5])
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' = break

-- | Remove prefix from list if it matches
-- >>> stripPrefix' [1,2] [1,2,3,4,5]
-- Just [3,4,5]
stripPrefix' :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix' [] ys = Just ys
stripPrefix' _ [] = Nothing
stripPrefix' (x:xs) (y:ys)
  | x == y = stripPrefix' xs ys
  | otherwise = Nothing

-- | Remove suffix from list if it matches
-- >>> stripSuffix' [4,5] [1,2,3,4,5]
-- Just [1,2,3]
stripSuffix' :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix' xs ys = fmap reverse $ stripPrefix' (reverse xs) (reverse ys)

-- | Check if first list is prefix of second
-- >>> isPrefixOf' [1,2] [1,2,3,4,5]
-- True
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

-- | Check if first list is suffix of second
-- >>> isSuffixOf' [4,5] [1,2,3,4,5]
-- True
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' xs ys = isPrefixOf' (reverse xs) (reverse ys)

-- Demo module
demo :: IO ()
demo = do
  putStrLn "=== List Utils Library Demo ==="

  putStrLn "\nChunk:"
  print $ chunk 3 [1..10]

  putStrLn "\nSplit at 4:"
  print $ splitAt' 4 [1..10]

  putStrLn "\nTake while < 5:"
  print $ takeWhile' (< 5) [1..10]

  putStrLn "\nUnique:"
  print $ unique [1,1,2,2,3,3,3,4,4]

  putStrLn "\nFrequencies:"
  print $ frequencies [1,1,2,2,2,3,3,3,3]

  putStrLn "\nRotate 3:"
  print $ rotate 3 [1..7]

  putStrLn "\nInterleave:"
  print $ interleave [1,2,3] [10,20,30]

  putStrLn "\nCartesian product:"
  print $ cartesianProduct [1,2] ['a','b']

  putStrLn "\nPermutations [1,2,3]:"
  print $ permutations' [1,2,3]

  putStrLn "\nCombinations 2 [1,2,3,4]:"
  print $ combinations 2 [1,2,3,4]

  putStrLn "\nPartition even:"
  print $ partition' even [1..10]

  putStrLn "\nSpan < 5:"
  print $ span' (< 5) [1..10]

  putStrLn "\nStrip prefix [1,2]:"
  print $ stripPrefix' [1,2] [1,2,3,4,5]

  putStrLn "\nIs prefix [1,2,3]:"
  print $ isPrefixOf' [1,2,3] [1,2,3,4,5]

-- Main for running demo
main :: IO ()
main = demo

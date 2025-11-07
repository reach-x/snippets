-- List operations in Haskell demonstrating functional programming
module ListOperations where

import Data.List (sort, reverse, nub, intercalate)

-- Basic list operations
listLength :: [a] -> Int
listLength = length

-- Get first element (head)
firstElement :: [a] -> Maybe a
firstElement [] = Nothing
firstElement (x:_) = Just x

-- Get all but first element (tail)
restElements :: [a] -> [a]
restElements [] = []
restElements (_:xs) = xs

-- Prepend element
prepend :: a -> [a] -> [a]
prepend x xs = x : xs

-- Append element
appendElement :: [a] -> a -> [a]
appendElement xs x = xs ++ [x]

-- Concatenate two lists
concatenateLists :: [a] -> [a] -> [a]
concatenateLists = (++)

-- Map - transform each element
mapList :: (a -> b) -> [a] -> [b]
mapList = map

-- Filter - keep only matching elements
filterList :: (a -> Bool) -> [a] -> [a]
filterList = filter

-- Fold left (reduce)
foldList :: (b -> a -> b) -> b -> [a] -> b
foldList = foldl

-- Fold right
foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight = foldr

-- Find first element matching predicate
findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst p (x:xs)
  | p x = Just x
  | otherwise = findFirst p xs

-- Check if any element matches
anyMatch :: (a -> Bool) -> [a] -> Bool
anyMatch = any

-- Check if all elements match
allMatch :: (a -> Bool) -> [a] -> Bool
allMatch = all

-- Sort list
sortList :: Ord a => [a] -> [a]
sortList = sort

-- Reverse list
reverseList :: [a] -> [a]
reverseList = reverse

-- Remove duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

-- Take first n elements
takeFirst :: Int -> [a] -> [a]
takeFirst = take

-- Drop first n elements
dropFirst :: Int -> [a] -> [a]
dropFirst = drop

-- Zip two lists together
zipLists :: [a] -> [b] -> [(a, b)]
zipLists = zip

-- Sum of numbers
sumList :: Num a => [a] -> a
sumList = sum

-- Product of numbers
productList :: Num a => [a] -> a
productList = product

-- Maximum element
maxElement :: Ord a => [a] -> Maybe a
maxElement [] = Nothing
maxElement xs = Just $ maximum xs

-- Minimum element
minElement :: Ord a => [a] -> Maybe a
minElement [] = Nothing
minElement xs = Just $ minimum xs

-- Range of numbers
range :: Int -> Int -> [Int]
range start end = [start..end]

-- List comprehension examples
evens :: [Int] -> [Int]
evens xs = [x | x <- xs, even x]

squares :: [Int] -> [Int]
squares xs = [x * x | x <- xs]

-- Demo function
demo :: IO ()
demo = do
  putStrLn "=== List Operations in Haskell ==="

  let numbers = [1, 2, 3, 4, 5]
  putStrLn $ "Numbers: " ++ show numbers
  putStrLn $ "Length: " ++ show (listLength numbers)

  putStrLn $ "First element: " ++ show (firstElement numbers)
  putStrLn $ "Rest elements: " ++ show (restElements numbers)

  putStrLn $ "Prepend 0: " ++ show (prepend 0 numbers)
  putStrLn $ "Append 6: " ++ show (appendElement numbers 6)

  let squared = mapList (\x -> x * x) numbers
  putStrLn $ "Squared: " ++ show squared

  let evensOnly = filterList even numbers
  putStrLn $ "Even numbers: " ++ show evensOnly

  let total = foldList (+) 0 numbers
  putStrLn $ "Sum: " ++ show total

  putStrLn $ "First > 3: " ++ show (findFirst (> 3) numbers)

  putStrLn $ "Any > 4: " ++ show (anyMatch (> 4) numbers)
  putStrLn $ "All positive: " ++ show (allMatch (> 0) numbers)

  let unsorted = [3, 1, 4, 1, 5, 9, 2, 6]
  putStrLn $ "Sorted: " ++ show (sortList unsorted)
  putStrLn $ "Reversed: " ++ show (reverseList numbers)

  let duplicates = [1, 2, 2, 3, 3, 3, 4]
  putStrLn $ "Remove duplicates: " ++ show (removeDuplicates duplicates)

  putStrLn $ "First 3: " ++ show (takeFirst 3 numbers)
  putStrLn $ "Drop 2: " ++ show (dropFirst 2 numbers)

  let letters = ['a', 'b', 'c', 'd', 'e']
  putStrLn $ "Zipped: " ++ show (zipLists numbers letters)

  putStrLn $ "Sum: " ++ show (sumList numbers)
  putStrLn $ "Product: " ++ show (productList numbers)
  putStrLn $ "Max: " ++ show (maxElement numbers)
  putStrLn $ "Min: " ++ show (minElement numbers)

  putStrLn $ "Range 1-10: " ++ show (range 1 10)

  putStrLn $ "Evens: " ++ show (evens [1..10])
  putStrLn $ "Squares: " ++ show (squares [1..5])

  -- Infinite lists (lazy evaluation)
  putStrLn $ "First 10 of infinite list: " ++ show (take 10 [1..])

-- Run demo
main :: IO ()
main = demo

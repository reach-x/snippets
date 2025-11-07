#!/usr/bin/env runhaskell

-- File statistics script in Haskell
-- Reads a file and provides various statistics

import System.IO
import System.Environment (getArgs)
import Data.List (sort, group)
import Data.Char (isSpace)

-- Count lines in content
countLines :: String -> Int
countLines = length . lines

-- Count words in content
countWords :: String -> Int
countWords = length . words

-- Count characters
countChars :: String -> Int
countChars = length

-- Count non-whitespace characters
countNonWhitespace :: String -> Int
countNonWhitespace = length . filter (not . isSpace)

-- Find longest line
longestLine :: String -> Int
longestLine content =
  if null (lines content)
    then 0
    else maximum . map length $ lines content

-- Count occurrences of each word
wordFrequency :: String -> [(String, Int)]
wordFrequency content =
  map (\ws -> (head ws, length ws)) .
  group .
  sort .
  words $ content

-- Get top N most frequent words
topWords :: Int -> String -> [(String, Int)]
topWords n content =
  take n .
  reverse .
  sort .
  map (\(w, c) -> (c, w)) .
  wordFrequency $ content

-- Calculate average word length
avgWordLength :: String -> Double
avgWordLength content =
  if null (words content)
    then 0
    else fromIntegral totalChars / fromIntegral totalWords
  where
    ws = words content
    totalWords = length ws
    totalChars = sum $ map length ws

-- Print statistics
printStats :: String -> String -> IO ()
printStats filename content = do
  putStrLn $ "=== File Statistics: " ++ filename ++ " ==="
  putStrLn $ "Lines: " ++ show (countLines content)
  putStrLn $ "Words: " ++ show (countWords content)
  putStrLn $ "Characters: " ++ show (countChars content)
  putStrLn $ "Non-whitespace chars: " ++ show (countNonWhitespace content)
  putStrLn $ "Longest line: " ++ show (longestLine content) ++ " characters"
  putStrLn $ "Average word length: " ++ show (avgWordLength content) ++ " characters"

  putStrLn "\nTop 10 most frequent words:"
  let top = topWords 10 content
  mapM_ (\(count, word) -> putStrLn $ "  " ++ word ++ ": " ++ show count) top

-- Process file
processFile :: FilePath -> IO ()
processFile filepath = do
  handle <- openFile filepath ReadMode
  content <- hGetContents handle
  printStats filepath content
  hClose handle

-- Demo with sample file
demo :: IO ()
demo = do
  let sampleFile = "../tmp/sample.txt"
  let sampleContent = unlines
        [ "Hello World"
        , "This is a sample file"
        , "With multiple lines"
        , "For testing Haskell"
        , "File processing capabilities"
        , "Hello again"
        ]

  -- Create tmp directory and file
  writeFile sampleFile sampleContent
  putStrLn "Created sample file\n"

  -- Process the file
  processFile sampleFile

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> demo  -- No arguments, run demo
    (filepath:_) -> processFile filepath  -- Process specified file

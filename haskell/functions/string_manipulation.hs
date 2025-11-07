-- String manipulation examples in Haskell
module StringManipulation where

import Data.Char (toUpper, toLower)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

-- String concatenation
concatenateStrings :: String -> String -> String
concatenateStrings s1 s2 = s1 ++ " " ++ s2

-- String length
stringLength :: String -> Int
stringLength = length

-- Convert to uppercase
toUpperCase :: String -> String
toUpperCase = map toUpper

-- Convert to lowercase
toLowerCase :: String -> String
toLowerCase = map toLower

-- Reverse a string
reverseString :: String -> String
reverseString = reverse

-- Check if string contains substring
contains :: String -> String -> Bool
contains haystack needle = needle `isInfixOf` haystack

-- Check if string starts with prefix
startsWith :: String -> String -> Bool
startsWith = isPrefixOf

-- Check if string ends with suffix
endsWith :: String -> String -> Bool
endsWith = isSuffixOf

-- Split string by character
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delim (c:cs)
  | c == delim = "" : rest
  | otherwise = (c : head rest) : tail rest
  where rest = splitBy delim cs

-- Trim whitespace from both ends
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-- Replace all occurrences of character
replaceChar :: Char -> Char -> String -> String
replaceChar old new = map (\c -> if c == old then new else c)

-- Capitalize first letter
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

-- Demo function
demo :: IO ()
demo = do
  putStrLn "=== String Manipulation in Haskell ==="

  let str1 = "Hello"
  let str2 = "World"
  putStrLn $ "Concatenated: " ++ concatenateStrings str1 str2

  let greeting = "Hello, Haskell!"
  putStrLn $ "Length: " ++ show (stringLength greeting)
  putStrLn $ "Uppercase: " ++ toUpperCase greeting
  putStrLn $ "Lowercase: " ++ toLowerCase greeting
  putStrLn $ "Reversed: " ++ reverseString greeting

  putStrLn $ "Contains 'Haskell': " ++ show (contains greeting "Haskell")
  putStrLn $ "Starts with 'Hello': " ++ show (startsWith "Hello" greeting)
  putStrLn $ "Ends with '!': " ++ show (endsWith "!" greeting)

  let csv = "apple,banana,cherry"
  putStrLn $ "Split by comma: " ++ show (splitBy ',' csv)

  let padded = "  hello  "
  putStrLn $ "Trimmed: '" ++ trim padded ++ "'"

  let text = "hello world"
  putStrLn $ "Replace 'o' with 'a': " ++ replaceChar 'o' 'a' text

  let name = "alice"
  putStrLn $ "Capitalized: " ++ capitalize name

-- Run demo
main :: IO ()
main = demo

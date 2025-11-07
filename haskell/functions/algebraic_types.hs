-- Algebraic data types and type classes in Haskell
module AlgebraicTypes where

-- Simple data type
data Color = Red | Green | Blue deriving (Show, Eq)

-- Data type with parameters
data Point = Point Double Double deriving (Show)

-- Recursive data type (Binary Tree)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Maybe-like type (Option)
data Option a = None | Some a deriving (Show, Eq)

-- Either-like type (Result)
data Result e a = Error e | Success a deriving (Show)

-- Type synonym
type Name = String
type Age = Int
type Person = (Name, Age)

-- Record syntax
data Customer = Customer
  { customerId :: Int
  , customerName :: String
  , customerEmail :: String
  , customerAge :: Int
  } deriving (Show)

-- Pattern matching on data types
describeColor :: Color -> String
describeColor Red = "This is red"
describeColor Green = "This is green"
describeColor Blue = "This is blue"

-- Pattern matching on Point
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- Tree operations
insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Empty = Node x Empty Empty
insertTree x (Node val left right)
  | x < val = Node val (insertTree x left) right
  | x > val = Node val left (insertTree x right)
  | otherwise = Node val left right

searchTree :: Ord a => a -> Tree a -> Bool
searchTree _ Empty = False
searchTree x (Node val left right)
  | x == val = True
  | x < val = searchTree x left
  | otherwise = searchTree x right

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node val left right) = treeToList left ++ [val] ++ treeToList right

-- Option operations
mapOption :: (a -> b) -> Option a -> Option b
mapOption _ None = None
mapOption f (Some x) = Some (f x)

unwrapOr :: a -> Option a -> a
unwrapOr def None = def
unwrapOr _ (Some x) = x

-- Result operations
mapResult :: (a -> b) -> Result e a -> Result e b
mapResult _ (Error e) = Error e
mapResult f (Success x) = Success (f x)

-- Type classes example
class Describable a where
  describe :: a -> String

instance Describable Color where
  describe = describeColor

instance Describable Point where
  describe (Point x y) = "Point at (" ++ show x ++ ", " ++ show y ++ ")"

-- Functor instance for Option
instance Functor Option where
  fmap _ None = None
  fmap f (Some x) = Some (f x)

-- Applicative instance for Option
instance Applicative Option where
  pure = Some
  None <*> _ = None
  (Some f) <*> something = fmap f something

-- Monad instance for Option
instance Monad Option where
  return = Some
  None >>= _ = None
  (Some x) >>= f = f x

-- Higher-order functions with types
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Currying examples
add :: Int -> Int -> Int
add x y = x + y

addFive :: Int -> Int
addFive = add 5

-- Demo function
demo :: IO ()
demo = do
  putStrLn "=== Algebraic Data Types in Haskell ==="

  -- Colors
  putStrLn $ "\n--- Colors ---"
  let color = Red
  putStrLn $ "Color: " ++ show color
  putStrLn $ describe color

  -- Points
  putStrLn $ "\n--- Points ---"
  let p1 = Point 0 0
  let p2 = Point 3 4
  putStrLn $ describe p1
  putStrLn $ describe p2
  putStrLn $ "Distance: " ++ show (distance p1 p2)

  -- Trees
  putStrLn $ "\n--- Binary Tree ---"
  let tree = insertTree 5 $ insertTree 3 $ insertTree 7 $
             insertTree 1 $ insertTree 9 Empty
  putStrLn $ "Tree: " ++ show tree
  putStrLn $ "Sorted list: " ++ show (treeToList tree)
  putStrLn $ "Contains 7: " ++ show (searchTree 7 tree)
  putStrLn $ "Contains 4: " ++ show (searchTree 4 tree)

  -- Option
  putStrLn $ "\n--- Option Type ---"
  let some = Some 42
  let none = None :: Option Int
  putStrLn $ "Some: " ++ show some
  putStrLn $ "None: " ++ show none
  putStrLn $ "Map Some: " ++ show (mapOption (* 2) some)
  putStrLn $ "Map None: " ++ show (mapOption (* 2) none)
  putStrLn $ "Unwrap Some: " ++ show (unwrapOr 0 some)
  putStrLn $ "Unwrap None: " ++ show (unwrapOr 0 none)

  -- Result
  putStrLn $ "\n--- Result Type ---"
  let success = Success 100 :: Result String Int
  let failure = Error "Something went wrong" :: Result String Int
  putStrLn $ "Success: " ++ show success
  putStrLn $ "Error: " ++ show failure
  putStrLn $ "Map Success: " ++ show (mapResult (* 2) success)
  putStrLn $ "Map Error: " ++ show (mapResult (* 2) failure)

  -- Records
  putStrLn $ "\n--- Records ---"
  let customer = Customer 1 "Alice" "alice@example.com" 30
  putStrLn $ "Customer: " ++ show customer
  putStrLn $ "Name: " ++ customerName customer
  putStrLn $ "Email: " ++ customerEmail customer

  -- Higher-order functions
  putStrLn $ "\n--- Higher-order Functions ---"
  let addOne = (+ 1)
  let double = (* 2)
  let composed = compose double addOne
  putStrLn $ "Compose (double . addOne) 5: " ++ show (composed 5)
  putStrLn $ "Twice addOne 5: " ++ show (twice addOne 5)

  -- Currying
  putStrLn $ "\n--- Currying ---"
  putStrLn $ "add 3 5: " ++ show (add 3 5)
  putStrLn $ "addFive 10: " ++ show (addFive 10)

-- Run demo
main :: IO ()
main = demo

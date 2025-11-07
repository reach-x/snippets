module Main

factorial : Nat -> Nat
factorial Z = 1
factorial (S k) = (S k) * factorial k

main : IO ()
main = do
  putStrLn "Hello from Idris!"
  printLn (factorial 5)

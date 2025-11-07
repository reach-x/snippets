// List operations in F#
// Demonstrating functional programming with immutable lists

module ListOperations

open System

// Demo function
let demo () =
    printfn "\n=== List Operations in F# ===\n"

    // Creating lists
    let numbers = [1; 2; 3; 4; 5]
    let fruits = ["apple"; "banana"; "cherry"]

    printfn "Numbers: %A" numbers
    printfn "Fruits: %A" fruits

    // List length
    printfn "\nLength: %d" (List.length numbers)

    // Head (first element)
    printfn "First element (head): %d" (List.head numbers)

    // Tail (rest of list)
    printfn "Rest of list (tail): %A" (List.tail numbers)

    // Prepend element (cons operator)
    printfn "\nPrepend 0: %A" (0 :: numbers)

    // Append lists
    printfn "Append [6;7;8]: %A" (numbers @ [6; 7; 8])

    // Reverse list
    printfn "Reversed: %A" (List.rev numbers)

    // Contains check
    printfn "\nContains 3: %b" (List.contains 3 numbers)
    printfn "Contains 10: %b" (List.contains 10 numbers)

    // Find element
    let found = List.tryFind (fun x -> x > 3) numbers
    printfn "\nFirst > 3: %A" found

    // Map function over list
    printfn "\nMap (square): %A" (List.map (fun x -> x * x) numbers)

    // Filter list
    printfn "Filter (even): %A" (List.filter (fun x -> x % 2 = 0) numbers)

    // Fold (reduce) left
    printfn "\nFold left (sum): %d" (List.fold (+) 0 numbers)
    printfn "Fold left (product): %d" (List.fold (*) 1 numbers)

    // Fold right
    printfn "Fold right (list): %A" (List.foldBack (fun x acc -> x :: acc) numbers [])

    // Exists and forall
    printfn "\nExists (> 4): %b" (List.exists (fun x -> x > 4) numbers)
    printfn "For all (> 0): %b" (List.forall (fun x -> x > 0) numbers)

    // Sort list
    let unsorted = [5; 2; 8; 1; 9; 3]
    printfn "\nUnsorted: %A" unsorted
    printfn "Sorted: %A" (List.sort unsorted)
    printfn "Sorted descending: %A" (List.sortDescending unsorted)

    // Take and skip
    printfn "\nTake 3: %A" (List.take 3 numbers)
    printfn "Skip 2: %A" (List.skip 2 numbers)

    // Zip two lists
    let letters = ['a'; 'b'; 'c'; 'd'; 'e']
    printfn "\nZip: %A" (List.zip numbers letters)

    // Unzip
    let zipped = [(1, 'a'); (2, 'b'); (3, 'c')]
    let (nums, lets) = List.unzip zipped
    printfn "Unzip: (%A, %A)" nums lets

    // Partition
    let (evens, odds) = List.partition (fun x -> x % 2 = 0) numbers
    printfn "\nPartition even:"
    printfn "  Evens: %A" evens
    printfn "  Odds: %A" odds

    // Group by
    let grouped = List.groupBy (fun x -> x % 3) [1..9]
    printfn "\nGroup by (mod 3): %A" grouped

    // Remove duplicates
    let withDupes = [1; 2; 2; 3; 3; 3; 4]
    printfn "\nWith duplicates: %A" withDupes
    printfn "Distinct: %A" (List.distinct withDupes)

    // Chunk
    printfn "\nChunk 3: %A" (List.chunkBySize 3 [1..10])

    // Windowed
    printfn "Windowed 3: %A" (List.windowed 3 numbers)

    // List comprehension
    let squares = [ for x in 1..5 -> x * x ]
    printfn "\nList comprehension (squares): %A" squares

    let evenSquares = [ for x in 1..10 do
                          if x % 2 = 0 then
                              yield x * x ]
    printfn "Even squares: %A" evenSquares

    // Range
    printfn "\nRange [1..10]: %A" [1..10]
    printfn "Range [1..2..10]: %A" [1..2..10]  // step by 2

    // Pattern matching on lists
    let rec listSum lst =
        match lst with
        | [] -> 0
        | head :: tail -> head + listSum tail

    printfn "\nList sum (pattern matching): %d" (listSum numbers)

    // Helper functions
    printfn "\n=== Helper Functions ==="
    printfn "Sum: %d" (sum numbers)
    printfn "Product: %d" (product numbers)
    printfn "Maximum: %d" (maximum numbers)
    printfn "Minimum: %d" (minimum numbers)
    printfn "Average: %.2f" (average numbers)

// Helper functions

let sum lst = List.fold (+) 0 lst

let product lst = List.fold (*) 1 lst

let maximum lst = List.max lst

let minimum lst = List.min lst

let average lst =
    let total = List.sum lst |> float
    total / float (List.length lst)

let range start stop =
    [start..stop]

let take n lst = List.take n lst

let drop n lst = List.skip n lst

let flatten lst = List.collect id lst

let unique lst = List.distinct lst

// Run demo
[<EntryPoint>]
let main argv =
    demo()
    0

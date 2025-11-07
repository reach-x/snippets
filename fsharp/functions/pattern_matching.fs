// Pattern matching and discriminated unions in F#

module PatternMatching

open System

// Discriminated unions
type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float
    | Triangle of base_: float * height: float

type Option<'T> =
    | Some of 'T
    | None

type Result<'T, 'E> =
    | Ok of 'T
    | Error of 'E

// Pattern matching on shapes
let area shape =
    match shape with
    | Circle radius -> Math.PI * radius * radius
    | Rectangle (width, height) -> width * height
    | Triangle (base_, height) -> 0.5 * base_ * height

// Pattern matching with guards
let describe number =
    match number with
    | n when n < 0 -> "negative"
    | 0 -> "zero"
    | n when n > 0 -> "positive"
    | _ -> "unknown"

// Record types
type Person = {
    Name: string
    Age: int
    City: string
}

// Pattern matching on records
let greet person =
    match person with
    | { Name = name; Age = age } when age < 18 ->
        sprintf "%s is a minor" name
    | { Name = name; Age = age } ->
        sprintf "%s is %d years old" name age

// Active patterns
let (|Even|Odd|) n =
    if n % 2 = 0 then Even else Odd

let (|Positive|Negative|Zero|) n =
    if n > 0 then Positive
    elif n < 0 then Negative
    else Zero

// Recursive pattern matching
let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | n -> n * factorial (n - 1)

let rec fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fibonacci (n - 1) + fibonacci (n - 2)

// Demo
let demo () =
    printfn "\n=== Pattern Matching in F# ===\n"

    // Shape pattern matching
    printfn "=== Shapes ==="
    let circle = Circle 5.0
    let rectangle = Rectangle (4.0, 6.0)
    let triangle = Triangle (3.0, 4.0)

    printfn "Circle area: %.2f" (area circle)
    printfn "Rectangle area: %.2f" (area rectangle)
    printfn "Triangle area: %.2f" (area triangle)

    // Number description
    printfn "\n=== Number Description ==="
    printfn "Describe 0: %s" (describe 0)
    printfn "Describe -5: %s" (describe -5)
    printfn "Describe 10: %s" (describe 10)

    // Record pattern matching
    printfn "\n=== Records ==="
    let person1 = { Name = "Alice"; Age = 30; City = "NYC" }
    let person2 = { Name = "Bob"; Age = 15; City = "LA" }
    printfn "%s" (greet person1)
    printfn "%s" (greet person2)

    // Active patterns
    printfn "\n=== Active Patterns ==="
    let checkParity n =
        match n with
        | Even -> sprintf "%d is even" n
        | Odd -> sprintf "%d is odd" n

    printfn "%s" (checkParity 4)
    printfn "%s" (checkParity 7)

    let checkSign n =
        match n with
        | Positive -> sprintf "%d is positive" n
        | Negative -> sprintf "%d is negative" n
        | Zero -> "zero"

    printfn "%s" (checkSign 5)
    printfn "%s" (checkSign -3)
    printfn "%s" (checkSign 0)

    // Recursion
    printfn "\n=== Recursion ==="
    printfn "Factorial 5: %d" (factorial 5)
    printfn "Fibonacci 10: %d" (fibonacci 10)

    // List pattern matching
    printfn "\n=== List Patterns ==="
    let rec sumList lst =
        match lst with
        | [] -> 0
        | head :: tail -> head + sumList tail

    printfn "Sum [1..5]: %d" (sumList [1..5])

[<EntryPoint>]
let main argv =
    demo()
    0

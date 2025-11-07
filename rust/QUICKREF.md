# Rust Quick Reference

## Basic Syntax
```rust
fn main() {
    // Variables (immutable by default)
    let name = "Alice";
    let mut age = 30;  // Mutable
    const PI: f64 = 3.14;

    // Comments
    // Single line comment
    /*
     Multi-line comment
    */
}
```

## Data Types
```rust
// Integers
let x: i32 = 42;
let y: u64 = 100;

// Floats
let pi: f64 = 3.14;

// Boolean
let is_active: bool = true;

// Character
let letter: char = 'A';

// String types
let s1: &str = "string slice";
let s2: String = String::from("owned string");

// Tuple
let tuple: (i32, f64, char) = (42, 3.14, 'A');
let (x, y, z) = tuple;  // Destructuring
```

## Collections
```rust
// Vector (dynamic array)
let mut numbers: Vec<i32> = vec![1, 2, 3, 4, 5];
numbers.push(6);
numbers.pop();

// Array (fixed size)
let arr: [i32; 5] = [1, 2, 3, 4, 5];

// HashMap
use std::collections::HashMap;
let mut map = HashMap::new();
map.insert("key", "value");

// Iterators
let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();
let evens: Vec<&i32> = numbers.iter().filter(|x| *x % 2 == 0).collect();
```

## Control Flow
```rust
// If expression
let result = if age >= 18 {
    "Adult"
} else {
    "Minor"
};

// Loop
loop {
    break;
}

// While
let mut count = 0;
while count < 5 {
    count += 1;
}

// For
for i in 0..5 {
    println!("{}", i);
}

for item in &numbers {
    println!("{}", item);
}

// Match
match value {
    1 => println!("One"),
    2 | 3 => println!("Two or three"),
    4..=10 => println!("Four through ten"),
    _ => println!("Something else"),
}
```

## Functions
```rust
// Basic function
fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

// Multiple returns (tuple)
fn divide(a: f64, b: f64) -> Option<f64> {
    if b == 0.0 {
        None
    } else {
        Some(a / b)
    }
}

// Generic function
fn largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];
    for item in list {
        if item > largest {
            largest = item;
        }
    }
    largest
}

// Closures
let add_one = |x| x + 1;
let multiply = |x, y| x * y;
```

## Structs and Enums
```rust
// Struct
struct Person {
    name: String,
    age: u32,
}

impl Person {
    fn new(name: String, age: u32) -> Self {
        Person { name, age }
    }

    fn greet(&self) -> String {
        format!("Hello, I'm {}", self.name)
    }

    fn birthday(&mut self) {
        self.age += 1;
    }
}

// Enum
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
}

// Option
let some_number: Option<i32> = Some(5);
let no_number: Option<i32> = None;

// Result
fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 {
        Err(String::from("Division by zero"))
    } else {
        Ok(a / b)
    }
}
```

## Ownership and Borrowing
```rust
// Ownership
let s1 = String::from("hello");
let s2 = s1;  // s1 is moved, no longer valid

// Borrowing (immutable)
let s = String::from("hello");
let len = calculate_length(&s);  // Borrow

// Mutable borrowing
let mut s = String::from("hello");
change(&mut s);

fn calculate_length(s: &String) -> usize {
    s.len()
}

fn change(s: &mut String) {
    s.push_str(", world");
}

// Lifetimes
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```

## Traits
```rust
// Define trait
trait Summary {
    fn summarize(&self) -> String;
}

// Implement trait
impl Summary for Person {
    fn summarize(&self) -> String {
        format!("{} is {} years old", self.name, self.age)
    }
}

// Trait bounds
fn notify<T: Summary>(item: &T) {
    println!("{}", item.summarize());
}
```

## Error Handling
```rust
// Unwrap (panics on error)
let result = some_operation().unwrap();

// Expect (panics with message)
let result = some_operation().expect("Operation failed");

// ? operator (propagate error)
fn read_file() -> Result<String, io::Error> {
    let mut file = File::open("file.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

// Match on Result
match divide(10.0, 0.0) {
    Ok(result) => println!("Result: {}", result),
    Err(e) => println!("Error: {}", e),
}

// if let
if let Some(value) = optional_value {
    println!("Got: {}", value);
}
```

## Pattern Matching
```rust
// Match
let number = 13;
match number {
    1 => println!("One"),
    2 | 3 | 5 | 7 | 11 => println!("Prime"),
    13..=19 => println!("Teen"),
    _ => println!("Other"),
}

// Destructuring
let Point { x, y } = point;
let (first, second, ..) = tuple;
```

## Modules and Crates
```rust
// Module
mod my_module {
    pub fn public_function() {}
    fn private_function() {}
}

// Use
use std::collections::HashMap;
use std::io::{self, Write};

// External crate (in Cargo.toml)
[dependencies]
serde = "1.0"
```

## Common Patterns
```rust
// Iterate with index
for (index, value) in numbers.iter().enumerate() {
    println!("{}: {}", index, value);
}

// Chain methods
let result = numbers
    .iter()
    .filter(|x| *x > 0)
    .map(|x| x * 2)
    .sum();

// Clone
let s2 = s1.clone();

// String conversion
let s: String = "hello".to_string();
let s: String = String::from("hello");
```

## Tips
- Use `snake_case` for variables, functions, modules
- Use `PascalCase` for types, traits
- Use `SCREAMING_SNAKE_CASE` for constants
- Prefer `&str` over `String` for function parameters
- Use `cargo` for project management
- `cargo build` to compile, `cargo run` to execute
- `cargo test` to run tests
- Embrace the borrow checker (it prevents bugs!)

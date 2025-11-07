# Go Quick Reference

## Basic Syntax
```go
package main

import "fmt"

// Variables
var name string = "Alice"
age := 30  // Short declaration
const PI = 3.14

// Comments
// Single line comment
/*
 Multi-line comment
*/
```

## Data Types
```go
// Basic types
var integer int = 42
var floating float64 = 3.14
var text string = "Hello"
var isActive bool = true

// Zero values
var i int        // 0
var f float64    // 0.0
var b bool       // false
var s string     // ""
```

## Collections
```go
// Array (fixed size)
var arr [5]int = [5]int{1, 2, 3, 4, 5}

// Slice (dynamic array)
numbers := []int{1, 2, 3, 4, 5}
numbers = append(numbers, 6)

// Map
person := map[string]interface{}{
    "name": "John",
    "age":  30,
}

// Make built-in
slice := make([]int, 5, 10)  // length 5, capacity 10
myMap := make(map[string]int)
```

## Control Flow
```go
// If statement
if age >= 18 {
    fmt.Println("Adult")
} else if age >= 13 {
    fmt.Println("Teenager")
} else {
    fmt.Println("Child")
}

// If with initialization
if err := doSomething(); err != nil {
    return err
}

// For loop (only loop statement)
for i := 0; i < 5; i++ {
    fmt.Println(i)
}

// While equivalent
count := 0
for count < 5 {
    fmt.Println(count)
    count++
}

// Infinite loop
for {
    break  // or continue
}

// Range
for index, value := range slice {
    fmt.Println(index, value)
}

// Switch
switch day {
case "Monday":
    fmt.Println("Start of week")
case "Friday":
    fmt.Println("TGIF")
default:
    fmt.Println("Regular day")
}
```

## Functions
```go
// Basic function
func greet(name string) string {
    return fmt.Sprintf("Hello, %s!", name)
}

// Multiple return values
func divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}

// Named return values
func split(sum int) (x, y int) {
    x = sum * 4 / 9
    y = sum - x
    return  // naked return
}

// Variadic function
func sum(numbers ...int) int {
    total := 0
    for _, num := range numbers {
        total += num
    }
    return total
}
```

## Structs and Methods
```go
// Struct definition
type Person struct {
    Name string
    Age  int
}

// Method (value receiver)
func (p Person) Greet() string {
    return fmt.Sprintf("Hello, I'm %s", p.Name)
}

// Method (pointer receiver)
func (p *Person) Birthday() {
    p.Age++
}

// Struct embedding (composition)
type Employee struct {
    Person
    JobTitle string
}
```

## Interfaces
```go
type Speaker interface {
    Speak() string
}

type Dog struct {
    Name string
}

func (d Dog) Speak() string {
    return "Woof!"
}

// Empty interface (any type)
var anything interface{}
```

## Pointers
```go
x := 10
p := &x      // pointer to x
fmt.Println(*p)  // dereference
*p = 20      // change x through pointer

// new built-in
ptr := new(int)  // *int, initialized to zero value
```

## Error Handling
```go
result, err := riskyOperation()
if err != nil {
    return err
}

// Panic and recover
defer func() {
    if r := recover(); r != nil {
        fmt.Println("Recovered:", r)
    }
}()
panic("something went wrong")
```

## Concurrency
```go
// Goroutine
go func() {
    fmt.Println("Running concurrently")
}()

// Channels
ch := make(chan int)
go func() {
    ch <- 42  // Send
}()
value := <-ch  // Receive

// Buffered channel
ch := make(chan int, 2)

// Select
select {
case msg := <-ch1:
    fmt.Println(msg)
case msg := <-ch2:
    fmt.Println(msg)
case <-time.After(time.Second):
    fmt.Println("Timeout")
}

// WaitGroup
var wg sync.WaitGroup
wg.Add(1)
go func() {
    defer wg.Done()
    // do work
}()
wg.Wait()
```

## Package Management
```go
// Import
import (
    "fmt"
    "strings"
    "github.com/user/package"
)

// Alias import
import m "math"

// Init function (runs automatically)
func init() {
    // Initialization code
}
```

## Common Patterns
```go
// Defer (executes when function returns)
defer file.Close()

// Type assertion
str, ok := value.(string)
if ok {
    fmt.Println(str)
}

// Type switch
switch v := value.(type) {
case string:
    fmt.Println("String:", v)
case int:
    fmt.Println("Int:", v)
}
```

## Tips
- Use `gofmt` or `goimports` for formatting
- Use `snake_case` for package names (single word preferred)
- Use `PascalCase` for exported names, `camelCase` for unexported
- Always handle errors, don't ignore them
- Use interfaces for abstraction
- Prefer composition over inheritance
- Use goroutines for concurrency
- `go build` to compile, `go run` to execute

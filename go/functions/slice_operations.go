package main

import "fmt"

func main() {
	numbers := []int{1, 2, 3, 4, 5}

	fmt.Printf("Numbers: %v\n", numbers)
	fmt.Printf("Length: %d\n", len(numbers))

	sum := 0
	for _, num := range numbers {
		sum += num
	}
	fmt.Printf("Sum: %d\n", sum)

	numbers = append(numbers, 6)
	fmt.Printf("After append: %v\n", numbers)

	numbers = numbers[:len(numbers)-1]
	fmt.Printf("After removing last: %v\n", numbers)

	squared := make([]int, len(numbers))
	for i, num := range numbers {
		squared[i] = num * num
	}
	fmt.Printf("Squared: %v\n", squared)

	var evens []int
	for _, num := range numbers {
		if num%2 == 0 {
			evens = append(evens, num)
		}
	}
	fmt.Printf("Even numbers: %v\n", evens)
}

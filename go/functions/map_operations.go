package main

import "fmt"

func main() {
	person := map[string]interface{}{
		"name": "John",
		"age":  30,
		"city": "New York",
	}

	fmt.Printf("Person: %v\n", person)
	fmt.Printf("Name: %v\n", person["name"])
	fmt.Printf("Age: %v\n", person["age"])

	person["email"] = "john@example.com"
	fmt.Printf("After adding email: %v\n", person)

	if name, exists := person["name"]; exists {
		fmt.Printf("Name exists: %v\n", name)
	}

	fmt.Println("\nIterating over map:")
	for key, value := range person {
		fmt.Printf("  %s: %v\n", key, value)
	}

	delete(person, "email")
	fmt.Printf("After deleting email: %v\n", person)
}

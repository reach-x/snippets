package main

import "fmt"

type Person struct {
	Name string
	Age  int
}

func (p *Person) Greet() string {
	return fmt.Sprintf("Hello, I'm %s and I'm %d years old", p.Name, p.Age)
}

func (p *Person) Birthday() string {
	p.Age++
	return fmt.Sprintf("Happy birthday! Now %d years old", p.Age)
}

type Employee struct {
	Person
	JobTitle string
}

func (e *Employee) Work() string {
	return fmt.Sprintf("%s is working as a %s", e.Name, e.JobTitle)
}

func main() {
	person := Person{Name: "Alice", Age: 30}
	fmt.Println(person.Greet())
	fmt.Println(person.Birthday())
	fmt.Printf("%+v\n", person)

	employee := Employee{
		Person:   Person{Name: "Bob", Age: 25},
		JobTitle: "Software Engineer",
	}
	fmt.Println(employee.Greet())
	fmt.Println(employee.Work())
	fmt.Printf("%+v\n", employee)
}

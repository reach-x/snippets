package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
)

type Person struct {
	Name   string   `json:"name"`
	Age    int      `json:"age"`
	Skills []string `json:"skills"`
	Active bool     `json:"active"`
}

func main() {
	person := Person{
		Name:   "John Doe",
		Age:    30,
		Skills: []string{"Go", "Python", "Docker"},
		Active: true,
	}

	jsonBytes, err := json.MarshalIndent(person, "", "  ")
	if err != nil {
		fmt.Printf("Error marshaling: %v\n", err)
		return
	}

	fmt.Printf("JSON String:\n%s\n", string(jsonBytes))

	var parsedPerson Person
	err = json.Unmarshal(jsonBytes, &parsedPerson)
	if err != nil {
		fmt.Printf("Error unmarshaling: %v\n", err)
		return
	}

	fmt.Printf("\nParsed data: %+v\n", parsedPerson)
	fmt.Printf("Name: %s\n", parsedPerson.Name)
	fmt.Printf("Skills: %v\n", parsedPerson.Skills)

	jsonFile := "../tmp/data.json"
	err = ioutil.WriteFile(jsonFile, jsonBytes, 0644)
	if err != nil {
		fmt.Printf("Error writing file: %v\n", err)
		return
	}
	fmt.Printf("\nWritten to %s\n", jsonFile)
}

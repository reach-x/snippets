package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	testFile := "../tmp/test_output.txt"

	data := "Hello from Go!\nThis is a test file.\n"
	err := ioutil.WriteFile(testFile, []byte(data), 0644)
	if err != nil {
		fmt.Printf("Error writing file: %v\n", err)
		return
	}
	fmt.Printf("Written to %s\n", testFile)

	content, err := ioutil.ReadFile(testFile)
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		return
	}
	fmt.Printf("Read content:\n%s", string(content))

	info, err := os.Stat(testFile)
	if err == nil {
		fmt.Printf("File exists: %s\n", testFile)
		fmt.Printf("File size: %d bytes\n", info.Size())
	}

	file, err := os.OpenFile(testFile, os.O_APPEND|os.O_WRONLY, 0644)
	if err != nil {
		fmt.Printf("Error opening file: %v\n", err)
		return
	}
	defer file.Close()

	_, err = file.WriteString("Appended line\n")
	if err != nil {
		fmt.Printf("Error appending: %v\n", err)
	} else {
		fmt.Println("Appended to file")
	}
}

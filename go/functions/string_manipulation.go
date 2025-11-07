package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "  Hello, World!  "

	fmt.Printf("Original: '%s'\n", text)
	fmt.Printf("Upper: '%s'\n", strings.ToUpper(text))
	fmt.Printf("Lower: '%s'\n", strings.ToLower(text))
	fmt.Printf("Trim: '%s'\n", strings.TrimSpace(text))
	fmt.Printf("Replace: '%s'\n", strings.Replace(text, "World", "Go", -1))
	fmt.Printf("Split: %v\n", strings.Split(strings.TrimSpace(text), ", "))
	fmt.Printf("Contains: %v\n", strings.Contains(text, "World"))
	fmt.Printf("HasPrefix: %v\n", strings.HasPrefix(strings.TrimSpace(text), "Hello"))
	fmt.Printf("HasSuffix: %v\n", strings.HasSuffix(strings.TrimSpace(text), "!"))
	fmt.Printf("Repeat: '%s'\n", strings.Repeat("-", 20))
}

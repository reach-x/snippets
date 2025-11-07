#!/usr/bin/env zsh
# Zsh scripting examples

echo ""
echo "=== Zsh Examples ==="
echo ""

# Variables
name="Alice"
age=30
echo "Name: $name, Age: $age"

# Arrays (1-indexed in zsh)
colors=(red green blue yellow)
echo "Colors: ${colors[@]}"
echo "First color: ${colors[1]}"
echo "Count: ${#colors[@]}"

# Associative arrays
typeset -A person
person[name]="Bob"
person[age]=25
person[city]="NYC"
echo "\nPerson: ${person[name]}, ${person[age]}, ${person[city]}"

# For loop
echo "\nLoop through colors:"
for color in ${colors[@]}; do
    echo "  - $color"
done

# Range
for i in {1..5}; do
    echo "Number: $i"
done

# If statement
if [[ $age -gt 18 ]]; then
    echo "\nAdult"
else
    echo "\nMinor"
fi

# Functions
function greet() {
    echo "Hello, $1!"
}

greet "Charlie"

# Function with return value
function add() {
    echo $(($1 + $2))
}

result=$(add 5 3)
echo "5 + 3 = $result"

# String operations
text="hello world"
echo "\nUppercase: ${text:u}"
echo "Lowercase: ${text:l}"
echo "Capitalize: ${text:u:0:1}${text:l:1}"
echo "Length: ${#text}"

# Array operations
echo "\nArray slice [1,3]: ${colors[1,3]}"
echo "Reverse: ${(Oa)colors[@]}"

# Pattern matching
files=(*.txt)
echo "Text files: ${files[@]}"

# Globs
setopt extended_glob
echo "Zsh supports powerful globbing"

echo "\nZsh examples complete!"

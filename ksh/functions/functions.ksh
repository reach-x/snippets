#!/bin/ksh
# Korn Shell scripting examples

echo ""
echo "=== Korn Shell Examples ==="
echo ""

# Variables
name="Alice"
age=30
echo "Name: $name, Age: $age"

# Arrays
set -A colors red green blue yellow
echo "Colors: ${colors[@]}"
echo "First color: ${colors[0]}"
echo "Count: ${#colors[@]}"

# Associative arrays (ksh93)
typeset -A person
person[name]="Bob"
person[age]=25
person[city]="NYC"
echo "Person: ${person[name]}, ${person[age]}, ${person[city]}"

# For loop
echo ""
echo "Loop through colors:"
for color in "${colors[@]}"; do
    echo "  - $color"
done

# Range (ksh93)
for ((i=1; i<=5; i++)); do
    echo "Number: $i"
done

# If statement
if [[ $age -gt 18 ]]; then
    echo ""
    echo "Adult"
else
    echo ""
    echo "Minor"
fi

# Functions
function greet {
    echo "Hello, $1!"
}

greet "Charlie"

# Function with return value
function add {
    typeset result=$(($1 + $2))
    echo $result
}

result=$(add 5 3)
echo "5 + 3 = $result"

# String operations
text="hello world"
echo ""
echo "Uppercase: ${text^^}"
echo "Lowercase: ${text,,}"
echo "Length: ${#text}"

# Substring
echo "Substring [0,5]: ${text:0:5}"

# Pattern matching
files=(*.txt)
echo "Text files: ${files[@]}"

# Select statement (menu)
echo ""
echo "Select statement example:"
PS3="Choose an option: "
options=("Option 1" "Option 2" "Quit")
select opt in "${options[@]}"; do
    case $opt in
        "Option 1")
            echo "You chose option 1"
            break
            ;;
        "Option 2")
            echo "You chose option 2"
            break
            ;;
        "Quit")
            break
            ;;
        *)
            echo "Invalid option"
            ;;
    esac
done

# Coprocess (ksh93 feature)
# Allows bidirectional communication with background process
cat |& # Start coprocess
print -p "Hello from coprocess"
read -p response

# Arithmetic
((sum = 5 + 3))
echo ""
echo "Arithmetic: 5 + 3 = $sum"

# Here document
cat <<EOF

This is a here document.
It can span multiple lines.
Variables are expanded: $name
EOF

echo "Korn Shell examples complete!"

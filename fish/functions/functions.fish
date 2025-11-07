#!/usr/bin/env fish
# Fish shell scripting examples

echo ""
echo "=== Fish Shell Examples ==="
echo ""

# Variables
set name "Alice"
set age 30
echo "Name: $name, Age: $age"

# Arrays (lists)
set colors red green blue
echo "Colors: $colors"
echo "First color: $colors[1]"
echo "Count: "(count $colors)

# For loop
echo ""
echo "Loop through colors:"
for color in $colors
    echo "  - $color"
end

# Range
for i in (seq 1 5)
    echo "Number: $i"
end

# If statement
echo ""
if test $age -gt 18
    echo "Adult"
else
    echo "Minor"
end

# Functions
function greet
    echo "Hello, $argv[1]!"
end

greet "Bob"

# Function with return value
function add
    math $argv[1] + $argv[2]
end

set result (add 5 3)
echo "5 + 3 = $result"

# Command substitution
set files (ls)
echo "File count: "(count $files)

# String operations
set text "hello world"
echo "Uppercase: "(string upper $text)
echo "Length: "(string length $text)

# Path operations
set PATH $PATH /usr/local/bin
echo "PATH updated"

# Abbreviations
abbr -a gs 'git status'
abbr -a gc 'git commit'

echo ""
echo "Fish shell examples complete!"

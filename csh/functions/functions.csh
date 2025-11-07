#!/bin/csh
# C Shell scripting examples

echo ""
echo "=== C Shell Examples ==="
echo ""

# Variables
set name = "Alice"
set age = 30
echo "Name: $name, Age: $age"

# Arrays
set colors = (red green blue yellow)
echo "Colors: $colors"
echo "First color: $colors[1]"
echo "Count: $#colors"

# Foreach loop
echo ""
echo "Loop through colors:"
foreach color ($colors)
    echo "  - $color"
end

# Range loop
echo ""
foreach i (`seq 1 5`)
    echo "Number: $i"
end

# If statement
echo ""
if ($age > 18) then
    echo "Adult"
else
    echo "Minor"
endif

# Aliases
alias ll 'ls -la'
alias gs 'git status'

# Environment variables
setenv PATH "${PATH}:/usr/local/bin"

# String operations
set text = "hello world"
echo "Text: $text"
echo "Length: `echo $text | wc -c`"

# File tests
if (-f /etc/hosts) then
    echo "/etc/hosts exists"
endif

if (-d /tmp) then
    echo "/tmp is a directory"
endif

# Switch statement
set day = Monday
switch ($day)
    case Monday:
        echo "Start of the week"
        breaksw
    case Friday:
        echo "End of the week"
        breaksw
    default:
        echo "Midweek"
        breaksw
endsw

# Command substitution
set files_count = `ls | wc -l`
echo "Files in current directory: $files_count"

# Exit status
echo ""
if ($status == 0) then
    echo "Last command succeeded"
endif

echo ""
echo "C Shell examples complete!"

#!/bin/bash

text="  Hello, World!  "

echo "Original: '$text'"
echo "Upper: '${text^^}'"
echo "Lower: '${text,,}'"

trimmed=$(echo "$text" | xargs)
echo "Trim: '$trimmed'"

replaced="${text/World/Bash}"
echo "Replace: '$replaced'"

echo "Length: ${#text}"

if [[ $text == *"World"* ]]; then
    echo "Contains 'World': true"
else
    echo "Contains 'World': false"
fi

substring="${trimmed:0:5}"
echo "Substring: '$substring'"

repeated=$(printf '%0.s-' {1..20})
echo "Repeat: '$repeated'"

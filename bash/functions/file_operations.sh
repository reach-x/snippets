#!/bin/bash

test_file="../tmp/test_output.txt"

# Write to file
echo "Hello from Bash!" > "$test_file"
echo "This is a test file." >> "$test_file"
echo "Written to $test_file"

# Read from file
echo -e "\nRead content:"
cat "$test_file"

# Check if file exists
if [ -f "$test_file" ]; then
    echo -e "\nFile exists: $test_file"
    file_size=$(wc -c < "$test_file")
    echo "File size: $file_size bytes"
fi

# Append to file
echo "Appended line" >> "$test_file"
echo "Appended to file"

# Count lines
line_count=$(wc -l < "$test_file")
echo "Line count: $line_count"

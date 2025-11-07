#!/bin/bash

numbers=(1 2 3 4 5)

echo "Numbers: ${numbers[@]}"
echo "Length: ${#numbers[@]}"

sum=0
for num in "${numbers[@]}"; do
    ((sum += num))
done
echo "Sum: $sum"

max=${numbers[0]}
for num in "${numbers[@]}"; do
    ((num > max)) && max=$num
done
echo "Max: $max"

min=${numbers[0]}
for num in "${numbers[@]}"; do
    ((num < min)) && min=$num
done
echo "Min: $min"

numbers+=(6)
echo "After adding 6: ${numbers[@]}"

unset 'numbers[-1]'
echo "After removing last: ${numbers[@]}"

echo "First element: ${numbers[0]}"
echo "Last element: ${numbers[-1]}"

echo "Even numbers:"
for num in "${numbers[@]}"; do
    if ((num % 2 == 0)); then
        echo "  $num"
    fi
done

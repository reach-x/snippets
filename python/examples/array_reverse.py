#!/usr/bin/env python3

def reverse_array(arr):
    """Reverse an array in place"""
    left, right = 0, len(arr) - 1
    while left < right:
        arr[left], arr[right] = arr[right], arr[left]
        left += 1
        right -= 1
    return arr

def reverse_array_pythonic(arr):
    """Reverse using Python slicing"""
    return arr[::-1]

def reverse_array_builtin(arr):
    """Reverse using built-in method"""
    arr_copy = arr.copy()
    arr_copy.reverse()
    return arr_copy

if __name__ == '__main__':
    test_arr = [1, 2, 3, 4, 5]

    print(f"Original: {test_arr}")
    print(f"Reversed (manual): {reverse_array(test_arr.copy())}")
    print(f"Reversed (pythonic): {reverse_array_pythonic(test_arr)}")
    print(f"Reversed (builtin): {reverse_array_builtin(test_arr)}")

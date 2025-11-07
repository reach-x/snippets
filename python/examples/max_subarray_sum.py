#!/usr/bin/env python3

def max_subarray_sum(arr):
    """
    Find maximum sum of contiguous subarray
    Kadane's Algorithm - O(n)
    """
    if not arr:
        return 0

    max_sum = current_sum = arr[0]

    for num in arr[1:]:
        current_sum = max(num, current_sum + num)
        max_sum = max(max_sum, current_sum)

    return max_sum

def max_subarray_with_indices(arr):
    """Return max sum and the subarray indices"""
    if not arr:
        return 0, 0, 0

    max_sum = current_sum = arr[0]
    max_start = max_end = 0
    current_start = 0

    for i in range(1, len(arr)):
        if arr[i] > current_sum + arr[i]:
            current_sum = arr[i]
            current_start = i
        else:
            current_sum += arr[i]

        if current_sum > max_sum:
            max_sum = current_sum
            max_start = current_start
            max_end = i

    return max_sum, max_start, max_end

if __name__ == '__main__':
    arr = [-2, 1, -3, 4, -1, 2, 1, -5, 4]

    print(f"Array: {arr}")

    max_sum = max_subarray_sum(arr)
    print(f"Maximum subarray sum: {max_sum}")

    max_sum, start, end = max_subarray_with_indices(arr)
    print(f"Subarray: {arr[start:end+1]}")
    print(f"Sum: {max_sum}")

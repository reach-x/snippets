#!/usr/bin/env python3

def binary_search(arr, target):
    """Binary search - O(log n)"""
    left, right = 0, len(arr) - 1

    while left <= right:
        mid = left + (right - left) // 2

        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1

def binary_search_recursive(arr, target, left=0, right=None):
    """Binary search using recursion"""
    if right is None:
        right = len(arr) - 1

    if left > right:
        return -1

    mid = left + (right - left) // 2

    if arr[mid] == target:
        return mid
    elif arr[mid] < target:
        return binary_search_recursive(arr, target, mid + 1, right)
    else:
        return binary_search_recursive(arr, target, left, mid - 1)

if __name__ == '__main__':
    arr = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    targets = [7, 10, 1, 19]

    print(f"Array: {arr}\n")

    for target in targets:
        index = binary_search(arr, target)
        print(f"Target {target}: {'Found at index ' + str(index) if index != -1 else 'Not found'}")

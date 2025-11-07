#!/usr/bin/env python3

def merge_sorted_arrays(arr1, arr2):
    """Merge two sorted arrays into one sorted array"""
    result = []
    i, j = 0, 0

    while i < len(arr1) and j < len(arr2):
        if arr1[i] < arr2[j]:
            result.append(arr1[i])
            i += 1
        else:
            result.append(arr2[j])
            j += 1

    result.extend(arr1[i:])
    result.extend(arr2[j:])

    return result

if __name__ == '__main__':
    arr1 = [1, 3, 5, 7, 9]
    arr2 = [2, 4, 6, 8, 10]

    merged = merge_sorted_arrays(arr1, arr2)
    print(f"Array 1: {arr1}")
    print(f"Array 2: {arr2}")
    print(f"Merged: {merged}")

#!/usr/bin/env python3

def find_duplicates_hash(arr):
    """Find duplicates using hash set - O(n)"""
    seen = set()
    duplicates = set()

    for num in arr:
        if num in seen:
            duplicates.add(num)
        seen.add(num)

    return list(duplicates)

def find_duplicates_counter(arr):
    """Find duplicates using Counter"""
    from collections import Counter
    counts = Counter(arr)
    return [num for num, count in counts.items() if count > 1]

def has_duplicate(arr):
    """Check if array has any duplicates"""
    return len(arr) != len(set(arr))

if __name__ == '__main__':
    arr = [1, 2, 3, 4, 2, 5, 6, 3, 7]

    print(f"Array: {arr}")
    print(f"Duplicates: {find_duplicates_hash(arr)}")
    print(f"Has duplicates: {has_duplicate(arr)}")

"""
Contains Duplicate

Given an integer array nums, return true if any value appears at least twice in
the array, and return false if every element is distinct.

LeetCode: https://leetcode.com/problems/contains-duplicate/
Difficulty: Easy
Pattern: Hash Set
"""

from typing import List


def contains_duplicate_brute_force(nums: List[int]) -> bool:
    """
    Brute force approach: Compare every pair of elements.

    Time Complexity: O(n^2)
    Space Complexity: O(1)
    """
    for i in range(len(nums)):
        for j in range(i + 1, len(nums)):
            if nums[i] == nums[j]:
                return True
    return False


def contains_duplicate_sorting(nums: List[int]) -> bool:
    """
    Sorting approach: Sort array and check adjacent elements.

    Time Complexity: O(n log n)
    Space Complexity: O(1) or O(n) depending on sorting algorithm
    """
    nums_sorted = sorted(nums)

    for i in range(len(nums_sorted) - 1):
        if nums_sorted[i] == nums_sorted[i + 1]:
            return True

    return False


def contains_duplicate(nums: List[int]) -> bool:
    """
    Optimal approach: Use hash set to track seen elements.

    Time Complexity: O(n)
    Space Complexity: O(n)

    Why it works:
    - Hash set provides O(1) lookup
    - We iterate through array once
    - If we see a number we've already seen, we found a duplicate
    """
    seen = set()

    for num in nums:
        if num in seen:
            return True
        seen.add(num)

    return False


def contains_duplicate_pythonic(nums: List[int]) -> bool:
    """
    Pythonic one-liner: Compare set length with list length.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    return len(set(nums)) != len(nums)


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [1, 2, 3, 1],
            'expected': True,
            'description': 'Contains duplicate (1 appears twice)'
        },
        {
            'nums': [1, 2, 3, 4],
            'expected': False,
            'description': 'All distinct elements'
        },
        {
            'nums': [1, 1, 1, 3, 3, 4, 3, 2, 4, 2],
            'expected': True,
            'description': 'Multiple duplicates'
        },
        {
            'nums': [],
            'expected': False,
            'description': 'Empty array'
        },
        {
            'nums': [1],
            'expected': False,
            'description': 'Single element'
        },
        {
            'nums': [0, 0],
            'expected': True,
            'description': 'Two identical elements'
        },
        {
            'nums': [-1, -2, -3, -1],
            'expected': True,
            'description': 'Negative numbers with duplicate'
        }
    ]

    print("=" * 60)
    print("CONTAINS DUPLICATE - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = contains_duplicate(test['nums'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['nums']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Brute Force: Time O(n^2), Space O(1)")
    print("Sorting: Time O(n log n), Space O(1) to O(n)")
    print("Hash Set (Optimal): Time O(n), Space O(n)")
    print("Pythonic: Time O(n), Space O(n)")

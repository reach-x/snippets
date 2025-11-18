"""
1. Two Sum

Given an array of integers nums and an integer target, return indices of the two
numbers such that they add up to target. You may assume that each input would have
exactly one solution, and you may not use the same element twice.

LeetCode: https://leetcode.com/problems/two-sum/
Difficulty: Easy
Pattern: Hash Map
"""

from typing import List


def two_sum_brute_force(nums: List[int], target: int) -> List[int]:
    """
    Brute force approach: Check every pair of numbers.

    Time Complexity: O(n^2)
    Space Complexity: O(1)
    """
    for i in range(len(nums)):
        for j in range(i + 1, len(nums)):
            if nums[i] + nums[j] == target:
                return [i, j]
    return []


def two_sum(nums: List[int], target: int) -> List[int]:
    """
    Optimal approach: Use hash map to store complements.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    seen = {}  # value -> index

    for i, num in enumerate(nums):
        complement = target - num
        if complement in seen:
            return [seen[complement], i]
        seen[num] = i

    return []


def two_sum_with_details(nums: List[int], target: int) -> dict:
    """
    Returns detailed information about the solution.
    """
    seen = {}

    for i, num in enumerate(nums):
        complement = target - num
        if complement in seen:
            return {
                'indices': [seen[complement], i],
                'values': [complement, num],
                'sum': complement + num
            }
        seen[num] = i

    return {'indices': [], 'values': [], 'sum': None}


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [2, 7, 11, 15],
            'target': 9,
            'expected': [0, 1],
            'description': 'Basic case'
        },
        {
            'nums': [3, 2, 4],
            'target': 6,
            'expected': [1, 2],
            'description': 'Numbers not at start'
        },
        {
            'nums': [3, 3],
            'target': 6,
            'expected': [0, 1],
            'description': 'Duplicate numbers'
        },
        {
            'nums': [-1, -2, -3, -4, -5],
            'target': -8,
            'expected': [2, 4],
            'description': 'Negative numbers'
        },
        {
            'nums': [0, 4, 3, 0],
            'target': 0,
            'expected': [0, 3],
            'description': 'Target is zero'
        }
    ]

    print("=" * 60)
    print("TWO SUM - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = two_sum(test['nums'], test['target'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: nums = {test['nums']}, target = {test['target']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

        # Show detailed solution
        details = two_sum_with_details(test['nums'], test['target'])
        if details['indices']:
            print(f"Solution: {details['values'][0]} + {details['values'][1]} = {details['sum']}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Brute Force: Time O(n^2), Space O(1)")
    print("Hash Map (Optimal): Time O(n), Space O(n)")

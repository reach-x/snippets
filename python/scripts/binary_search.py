"""
7. Binary Search

Given an array of integers nums which is sorted in ascending order, and an integer target,
write a function to search target in nums. If target exists, then return its index.
Otherwise, return -1. You must write an algorithm with O(log n) runtime complexity.

LeetCode: https://leetcode.com/problems/binary-search/
Difficulty: Easy
Pattern: Binary Search
"""

from typing import List


def binary_search_iterative(nums: List[int], target: int) -> int:
    """
    Iterative binary search.

    Time Complexity: O(log n)
    Space Complexity: O(1)
    """
    left, right = 0, len(nums) - 1

    while left <= right:
        mid = left + (right - left) // 2  # Avoid overflow

        if nums[mid] == target:
            return mid
        elif nums[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1


def binary_search_recursive(nums: List[int], target: int) -> int:
    """
    Recursive binary search.

    Time Complexity: O(log n)
    Space Complexity: O(log n) due to recursion stack
    """
    def search(left: int, right: int) -> int:
        if left > right:
            return -1

        mid = left + (right - left) // 2

        if nums[mid] == target:
            return mid
        elif nums[mid] < target:
            return search(mid + 1, right)
        else:
            return search(left, mid - 1)

    return search(0, len(nums) - 1)


# Alias for optimal solution
binary_search = binary_search_iterative


def binary_search_with_details(nums: List[int], target: int) -> dict:
    """
    Returns detailed search information.
    """
    left, right = 0, len(nums) - 1
    steps = []
    iterations = 0

    while left <= right:
        iterations += 1
        mid = left + (right - left) // 2

        steps.append({
            'iteration': iterations,
            'left': left,
            'right': right,
            'mid': mid,
            'mid_value': nums[mid],
            'comparison': 'found' if nums[mid] == target else ('go_right' if nums[mid] < target else 'go_left')
        })

        if nums[mid] == target:
            return {
                'found': True,
                'index': mid,
                'value': nums[mid],
                'iterations': iterations,
                'steps': steps
            }
        elif nums[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return {
        'found': False,
        'index': -1,
        'value': None,
        'iterations': iterations,
        'steps': steps
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [-1, 0, 3, 5, 9, 12],
            'target': 9,
            'expected': 4,
            'description': 'Target in middle-right'
        },
        {
            'nums': [-1, 0, 3, 5, 9, 12],
            'target': 2,
            'expected': -1,
            'description': 'Target not found'
        },
        {
            'nums': [5],
            'target': 5,
            'expected': 0,
            'description': 'Single element - found'
        },
        {
            'nums': [5],
            'target': 3,
            'expected': -1,
            'description': 'Single element - not found'
        },
        {
            'nums': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
            'target': 1,
            'expected': 0,
            'description': 'Target at start'
        },
        {
            'nums': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
            'target': 10,
            'expected': 9,
            'description': 'Target at end'
        },
        {
            'nums': [2, 5],
            'target': 5,
            'expected': 1,
            'description': 'Two elements'
        }
    ]

    print("=" * 70)
    print("BINARY SEARCH - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        result = binary_search(test['nums'], test['target'])
        details = binary_search_with_details(test['nums'], test['target'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Array: {test['nums']}")
        print(f"Target: {test['target']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Iterations: {details['iterations']}")

        if details['found']:
            print(f"Found at index {details['index']} with value {details['value']}")
        else:
            print(f"Not found in array")

        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("BINARY SEARCH TEMPLATE")
    print("=" * 70)
    print("""
def binary_search(nums, target):
    left, right = 0, len(nums) - 1

    while left <= right:
        mid = left + (right - left) // 2  # Avoid overflow

        if nums[mid] == target:
            return mid
        elif nums[mid] < target:
            left = mid + 1  # Search right half
        else:
            right = mid - 1  # Search left half

    return -1  # Not found
""")

    print("\n" + "=" * 70)
    print("KEY POINTS")
    print("=" * 70)
    print("1. Array must be sorted")
    print("2. Use left + (right - left) // 2 to avoid integer overflow")
    print("3. Time: O(log n), Space: O(1) for iterative")
    print("4. Each iteration eliminates half the search space")

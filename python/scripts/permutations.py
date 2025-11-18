"""
13. Permutations

Given an array nums of distinct integers, return all the possible permutations.
You can return the answer in any order.

LeetCode: https://leetcode.com/problems/permutations/
Difficulty: Medium
Pattern: Backtracking
"""

from typing import List


def permutations_backtrack(nums: List[int]) -> List[List[int]]:
    """
    Backtracking approach with swapping.

    Time Complexity: O(n! * n)
    Space Complexity: O(n! * n) for output, O(n) for recursion
    """
    result = []

    def backtrack(start: int):
        # Base case: reached end of array
        if start == len(nums):
            result.append(nums[:])
            return

        for i in range(start, len(nums)):
            # Swap
            nums[start], nums[i] = nums[i], nums[start]

            # Recurse
            backtrack(start + 1)

            # Backtrack (undo swap)
            nums[start], nums[i] = nums[i], nums[start]

    backtrack(0)
    return result


def permutations_choice_list(nums: List[int]) -> List[List[int]]:
    """
    Backtracking with choice list approach.

    Time Complexity: O(n! * n)
    Space Complexity: O(n! * n)
    """
    result = []

    def backtrack(path: List[int], choices: List[int]):
        # Base case: no more choices
        if not choices:
            result.append(path)
            return

        for i in range(len(choices)):
            # Choose
            new_path = path + [choices[i]]
            new_choices = choices[:i] + choices[i+1:]

            # Explore
            backtrack(new_path, new_choices)

    backtrack([], nums)
    return result


def permutations_iterative(nums: List[int]) -> List[List[int]]:
    """
    Iterative approach.

    Time Complexity: O(n! * n)
    Space Complexity: O(n! * n)
    """
    result = [[]]

    for num in nums:
        new_result = []
        for perm in result:
            for i in range(len(perm) + 1):
                new_result.append(perm[:i] + [num] + perm[i:])
        result = new_result

    return result


# Alias for optimal solution
permute = permutations_backtrack


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [1, 2, 3],
            'expected_count': 6,
            'description': 'Three elements'
        },
        {
            'nums': [0, 1],
            'expected_count': 2,
            'description': 'Two elements'
        },
        {
            'nums': [1],
            'expected_count': 1,
            'description': 'Single element'
        },
        {
            'nums': [1, 2, 3, 4],
            'expected_count': 24,
            'description': 'Four elements (4!)'
        }
    ]

    print("=" * 70)
    print("PERMUTATIONS - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        result_backtrack = permutations_backtrack(test['nums'][:])
        result_choice = permutations_choice_list(test['nums'][:])
        result_iterative = permutations_iterative(test['nums'][:])

        passed_backtrack = len(result_backtrack) == test['expected_count']
        passed_choice = len(result_choice) == test['expected_count']
        passed_iterative = len(result_iterative) == test['expected_count']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['nums']}")
        print(f"Expected Count: {test['expected_count']}")
        print(f"Got (Backtrack): {len(result_backtrack)} permutations")
        print(f"Got (Choice List): {len(result_choice)} permutations")
        print(f"Got (Iterative): {len(result_iterative)} permutations")

        if len(test['nums']) <= 3:
            print(f"All Permutations:")
            for perm in sorted(result_backtrack):
                print(f"  {perm}")

        print(f"Status: {'PASS' if passed_backtrack and passed_choice and passed_iterative else 'FAIL'}")

    print("\n" + "=" * 70)
    print("BACKTRACKING TEMPLATE (SWAP METHOD)")
    print("=" * 70)
    print("""
def permute(nums):
    result = []

    def backtrack(start):
        if start == len(nums):
            result.append(nums[:])  # Make a copy
            return

        for i in range(start, len(nums)):
            # Swap
            nums[start], nums[i] = nums[i], nums[start]

            # Recurse
            backtrack(start + 1)

            # Backtrack
            nums[start], nums[i] = nums[i], nums[start]

    backtrack(0)
    return result
""")

    print("\n" + "=" * 70)
    print("COMPLEXITY ANALYSIS")
    print("=" * 70)
    print("Time: O(n! * n)")
    print("  - n! permutations to generate")
    print("  - Each permutation takes O(n) to create/copy")
    print("\nSpace: O(n! * n)")
    print("  - Storing all permutations")
    print("  - Recursion depth is O(n)")
    print("\nFor n elements: n! permutations")
    print("  1! = 1")
    print("  2! = 2")
    print("  3! = 6")
    print("  4! = 24")
    print("  5! = 120")

"""
Maximum Subarray (Kadane's Algorithm)

Given an integer array nums, find the subarray with the largest sum, and return its sum.

LeetCode: https://leetcode.com/problems/maximum-subarray/
Difficulty: Medium
Pattern: Dynamic Programming (Kadane's Algorithm)
"""

from typing import List


def max_subarray_brute_force(nums: List[int]) -> int:
    """
    Brute force approach: Try all possible subarrays.

    Time Complexity: O(n^3) - with nested sum calculation
    Space Complexity: O(1)
    """
    max_sum = float('-inf')

    for i in range(len(nums)):
        for j in range(i, len(nums)):
            # Calculate sum of subarray from i to j
            current_sum = sum(nums[i:j+1])
            max_sum = max(max_sum, current_sum)

    return max_sum


def max_subarray_optimized_brute_force(nums: List[int]) -> int:
    """
    Optimized brute force: Calculate sum incrementally.

    Time Complexity: O(n^2)
    Space Complexity: O(1)
    """
    max_sum = float('-inf')

    for i in range(len(nums)):
        current_sum = 0
        for j in range(i, len(nums)):
            # Add next element instead of recalculating entire sum
            current_sum += nums[j]
            max_sum = max(max_sum, current_sum)

    return max_sum


def max_subarray(nums: List[int]) -> int:
    """
    Kadane's Algorithm - Optimal dynamic programming approach.

    Time Complexity: O(n)
    Space Complexity: O(1)

    Why it works:
    - At each position, we decide: extend current subarray or start new one
    - If current_sum becomes negative, it can't help future subarrays
    - Keep track of maximum sum seen so far

    Key insight: current_sum = max(num, current_sum + num)
    - If adding current element to existing sum is worse than starting fresh,
      we start a new subarray from current element
    """
    max_sum = nums[0]  # Track global maximum
    current_sum = nums[0]  # Track current subarray sum

    for i in range(1, len(nums)):
        # Extend current subarray or start new one
        current_sum = max(nums[i], current_sum + nums[i])

        # Update global maximum
        max_sum = max(max_sum, current_sum)

    return max_sum


def max_subarray_with_indices(nums: List[int]) -> dict:
    """
    Kadane's Algorithm with tracking of subarray indices.

    Returns dictionary with max sum and the subarray indices.
    """
    max_sum = nums[0]
    current_sum = nums[0]

    # Track indices
    max_start = 0
    max_end = 0
    current_start = 0

    for i in range(1, len(nums)):
        # If we start new subarray, update start index
        if nums[i] > current_sum + nums[i]:
            current_sum = nums[i]
            current_start = i
        else:
            current_sum = current_sum + nums[i]

        # Update maximum and its indices
        if current_sum > max_sum:
            max_sum = current_sum
            max_start = current_start
            max_end = i

    return {
        'max_sum': max_sum,
        'start': max_start,
        'end': max_end,
        'subarray': nums[max_start:max_end+1]
    }


def max_subarray_divide_conquer(nums: List[int]) -> int:
    """
    Divide and conquer approach (less practical but educational).

    Time Complexity: O(n log n)
    Space Complexity: O(log n) for recursion stack
    """
    def helper(left: int, right: int) -> int:
        if left == right:
            return nums[left]

        mid = (left + right) // 2

        # Max subarray is either:
        # 1. Entirely in left half
        left_sum = helper(left, mid)

        # 2. Entirely in right half
        right_sum = helper(mid + 1, right)

        # 3. Crosses the middle
        # Find max sum extending left from mid
        left_cross_sum = float('-inf')
        current_sum = 0
        for i in range(mid, left - 1, -1):
            current_sum += nums[i]
            left_cross_sum = max(left_cross_sum, current_sum)

        # Find max sum extending right from mid+1
        right_cross_sum = float('-inf')
        current_sum = 0
        for i in range(mid + 1, right + 1):
            current_sum += nums[i]
            right_cross_sum = max(right_cross_sum, current_sum)

        cross_sum = left_cross_sum + right_cross_sum

        return max(left_sum, right_sum, cross_sum)

    return helper(0, len(nums) - 1)


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [-2, 1, -3, 4, -1, 2, 1, -5, 4],
            'expected': 6,
            'description': 'Subarray [4,-1,2,1] has largest sum 6'
        },
        {
            'nums': [1],
            'expected': 1,
            'description': 'Single element'
        },
        {
            'nums': [5, 4, -1, 7, 8],
            'expected': 23,
            'description': 'All elements positive sum'
        },
        {
            'nums': [-1, -2, -3, -4],
            'expected': -1,
            'description': 'All negative - pick least negative'
        },
        {
            'nums': [-2, -1],
            'expected': -1,
            'description': 'Two negative numbers'
        },
        {
            'nums': [1, 2, -1, -2, 2, 1, -2, 1, 4, -5, 4],
            'expected': 6,
            'description': 'Complex case'
        }
    ]

    print("=" * 60)
    print("MAXIMUM SUBARRAY (KADANE'S ALGORITHM) - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = max_subarray(test['nums'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['nums']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")

        # Show subarray details
        details = max_subarray_with_indices(test['nums'])
        print(f"Subarray: {details['subarray']} (indices {details['start']} to {details['end']})")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Brute Force: Time O(n^3), Space O(1)")
    print("Optimized Brute Force: Time O(n^2), Space O(1)")
    print("Kadane's Algorithm (Optimal): Time O(n), Space O(1)")
    print("Divide and Conquer: Time O(n log n), Space O(log n)")

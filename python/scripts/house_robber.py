"""
16. House Robber

You are a professional robber planning to rob houses along a street. Each house
has a certain amount of money stashed, the only constraint stopping you from
robbing each of them is that adjacent houses have security systems connected and
it will automatically contact the police if two adjacent houses were broken into
on the same night.

Given an integer array nums representing the amount of money of each house,
return the maximum amount of money you can rob tonight without alerting the police.

LeetCode: https://leetcode.com/problems/house-robber/
Difficulty: Medium
Pattern: Dynamic Programming
"""

from typing import List


def rob_recursive(nums: List[int]) -> int:
    """
    Naive recursive approach.

    Time Complexity: O(2^n)
    Space Complexity: O(n) for recursion stack
    """
    def helper(i: int) -> int:
        if i >= len(nums):
            return 0

        # Two choices: rob this house or skip it
        rob_current = nums[i] + helper(i + 2)
        skip_current = helper(i + 1)

        return max(rob_current, skip_current)

    return helper(0)


def rob_memoization(nums: List[int]) -> int:
    """
    Top-down DP with memoization.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    memo = {}

    def helper(i: int) -> int:
        if i >= len(nums):
            return 0

        if i in memo:
            return memo[i]

        memo[i] = max(nums[i] + helper(i + 2), helper(i + 1))
        return memo[i]

    return helper(0)


def rob_tabulation(nums: List[int]) -> int:
    """
    Bottom-up DP with tabulation.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    if not nums:
        return 0
    if len(nums) == 1:
        return nums[0]

    n = len(nums)
    dp = [0] * n
    dp[0] = nums[0]
    dp[1] = max(nums[0], nums[1])

    for i in range(2, n):
        # Either rob current house + max from i-2, or skip current house
        dp[i] = max(nums[i] + dp[i - 2], dp[i - 1])

    return dp[n - 1]


def rob(nums: List[int]) -> int:
    """
    Optimal: Space-optimized DP.

    Time Complexity: O(n)
    Space Complexity: O(1)
    """
    if not nums:
        return 0
    if len(nums) == 1:
        return nums[0]

    prev2 = nums[0]  # Max money up to house i-2
    prev1 = max(nums[0], nums[1])  # Max money up to house i-1

    for i in range(2, len(nums)):
        current = max(nums[i] + prev2, prev1)
        prev2 = prev1
        prev1 = current

    return prev1


def rob_with_houses(nums: List[int]) -> dict:
    """
    Returns which houses to rob for maximum profit.
    """
    if not nums:
        return {'max_money': 0, 'houses': []}
    if len(nums) == 1:
        return {'max_money': nums[0], 'houses': [0]}

    n = len(nums)
    dp = [0] * n
    dp[0] = nums[0]
    dp[1] = max(nums[0], nums[1])

    for i in range(2, n):
        dp[i] = max(nums[i] + dp[i - 2], dp[i - 1])

    # Backtrack to find which houses
    houses = []
    i = n - 1
    while i >= 0:
        if i == 0 or dp[i] != dp[i - 1]:
            houses.append(i)
            i -= 2
        else:
            i -= 1

    houses.reverse()

    return {
        'max_money': dp[n - 1],
        'houses': houses,
        'amounts': [nums[h] for h in houses]
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [1, 2, 3, 1],
            'expected': 4,
            'description': 'Rob houses 0 and 2'
        },
        {
            'nums': [2, 7, 9, 3, 1],
            'expected': 12,
            'description': 'Rob houses 0, 2, and 4'
        },
        {
            'nums': [2, 1, 1, 2],
            'expected': 4,
            'description': 'Rob houses 0 and 3'
        },
        {
            'nums': [5, 3, 4, 11, 2],
            'expected': 16,
            'description': 'Rob houses 0 and 3'
        },
        {
            'nums': [1],
            'expected': 1,
            'description': 'Single house'
        },
        {
            'nums': [2, 1],
            'expected': 2,
            'description': 'Two houses'
        }
    ]

    print("=" * 70)
    print("HOUSE ROBBER - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        result_memo = rob_memoization(test['nums'][:])
        result_tab = rob_tabulation(test['nums'][:])
        result_optimal = rob(test['nums'][:])
        details = rob_with_houses(test['nums'][:])

        passed = (result_memo == test['expected'] and
                 result_tab == test['expected'] and
                 result_optimal == test['expected'])

        print(f"\nTest {i}: {test['description']}")
        print(f"Houses: {test['nums']}")
        print(f"Expected: ${test['expected']}")
        print(f"Got (Memoization): ${result_memo}")
        print(f"Got (Tabulation): ${result_tab}")
        print(f"Got (Optimal): ${result_optimal}")
        print(f"Rob houses at indices: {details['houses']}")
        print(f"With amounts: {details['amounts']}")
        print(f"Total: ${sum(details['amounts'])}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("DP RECURRENCE RELATION")
    print("=" * 70)
    print("""
dp[i] = max(nums[i] + dp[i-2], dp[i-1])

Where:
  dp[i] = maximum money we can rob up to house i

Two choices at each house:
  1. Rob house i: nums[i] + dp[i-2]
     (can't rob i-1, so take max from i-2)
  2. Skip house i: dp[i-1]
     (take max money up to previous house)

Base cases:
  dp[0] = nums[0]
  dp[1] = max(nums[0], nums[1])
""")

    print("\n" + "=" * 70)
    print("APPROACH COMPARISON")
    print("=" * 70)
    print("Naive Recursive: Time O(2^n), Space O(n)")
    print("Memoization: Time O(n), Space O(n)")
    print("Tabulation: Time O(n), Space O(n)")
    print("Optimal: Time O(n), Space O(1) - BEST")

"""
3Sum

Given an integer array nums, return all the triplets [nums[i], nums[j], nums[k]]
such that i != j, i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0.

Notice that the solution set must not contain duplicate triplets.

LeetCode: https://leetcode.com/problems/3sum/
Difficulty: Medium
Pattern: Two Pointers (after sorting)
"""

from typing import List


def three_sum_brute_force(nums: List[int]) -> List[List[int]]:
    """
    Brute force approach: Try all triplet combinations.

    Time Complexity: O(n^3)
    Space Complexity: O(1) excluding result
    """
    result = set()  # Use set to avoid duplicates

    for i in range(len(nums)):
        for j in range(i + 1, len(nums)):
            for k in range(j + 1, len(nums)):
                if nums[i] + nums[j] + nums[k] == 0:
                    # Sort triplet to handle duplicates
                    triplet = tuple(sorted([nums[i], nums[j], nums[k]]))
                    result.add(triplet)

    return [list(triplet) for triplet in result]


def three_sum_hash_set(nums: List[int]) -> List[List[int]]:
    """
    Hash set approach: Fix one number, use two sum on remainder.

    Time Complexity: O(n^2)
    Space Complexity: O(n)
    """
    result = set()

    for i in range(len(nums)):
        # For each nums[i], find pairs that sum to -nums[i]
        target = -nums[i]
        seen = set()

        for j in range(i + 1, len(nums)):
            complement = target - nums[j]

            if complement in seen:
                # Found triplet, sort to handle duplicates
                triplet = tuple(sorted([nums[i], nums[j], complement]))
                result.add(triplet)

            seen.add(nums[j])

    return [list(triplet) for triplet in result]


def three_sum(nums: List[int]) -> List[List[int]]:
    """
    Optimal two pointers approach after sorting.

    Time Complexity: O(n^2)
    Space Complexity: O(1) excluding result (sorting is in-place)

    Why it works:
    1. Sort the array first
    2. Fix first number (nums[i])
    3. Use two pointers on remaining array to find pairs that sum to -nums[i]
    4. Skip duplicates to avoid duplicate triplets
    """
    nums.sort()  # Sort for two-pointer technique
    result = []

    for i in range(len(nums) - 2):
        # Skip duplicate values for first number
        if i > 0 and nums[i] == nums[i - 1]:
            continue

        # Early termination: if first number is positive, no way to get sum = 0
        if nums[i] > 0:
            break

        # Two pointers for remaining array
        left = i + 1
        right = len(nums) - 1
        target = -nums[i]

        while left < right:
            current_sum = nums[left] + nums[right]

            if current_sum == target:
                result.append([nums[i], nums[left], nums[right]])

                # Skip duplicates for second number
                while left < right and nums[left] == nums[left + 1]:
                    left += 1

                # Skip duplicates for third number
                while left < right and nums[right] == nums[right - 1]:
                    right -= 1

                left += 1
                right -= 1

            elif current_sum < target:
                left += 1
            else:
                right -= 1

    return result


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [-1, 0, 1, 2, -1, -4],
            'expected': [[-1, -1, 2], [-1, 0, 1]],
            'description': 'Two triplets found'
        },
        {
            'nums': [0, 1, 1],
            'expected': [],
            'description': 'No triplets sum to zero'
        },
        {
            'nums': [0, 0, 0],
            'expected': [[0, 0, 0]],
            'description': 'All zeros'
        },
        {
            'nums': [-2, 0, 1, 1, 2],
            'expected': [[-2, 0, 2], [-2, 1, 1]],
            'description': 'Multiple valid triplets'
        },
        {
            'nums': [1, 2, -2, -1],
            'expected': [],
            'description': 'No valid triplets'
        },
        {
            'nums': [-4, -1, -1, 0, 1, 2],
            'expected': [[-1, -1, 2], [-1, 0, 1]],
            'description': 'Duplicates in input'
        }
    ]

    print("=" * 60)
    print("3SUM - TEST RESULTS")
    print("=" * 60)

    def normalize_result(result):
        """Sort result for comparison"""
        return sorted([sorted(triplet) for triplet in result])

    for i, test in enumerate(test_cases, 1):
        result = three_sum(test['nums'])
        normalized_result = normalize_result(result)
        normalized_expected = normalize_result(test['expected'])
        passed = normalized_result == normalized_expected

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['nums']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Brute Force: Time O(n^3), Space O(1)")
    print("Hash Set: Time O(n^2), Space O(n)")
    print("Two Pointers (Optimal): Time O(n^2), Space O(1)")
    print("\nKey Insight: Sort first, then for each number,")
    print("use two pointers to find pairs that complete the triplet")

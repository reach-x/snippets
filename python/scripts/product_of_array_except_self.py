"""
Product of Array Except Self

Given an integer array nums, return an array answer such that answer[i] is equal
to the product of all the elements of nums except nums[i].

The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
You must write an algorithm that runs in O(n) time and without using the division operator.

LeetCode: https://leetcode.com/problems/product-of-array-except-self/
Difficulty: Medium
Pattern: Prefix/Suffix Products
"""

from typing import List


def product_except_self_division(nums: List[int]) -> List[int]:
    """
    Division approach (NOT ALLOWED in problem, but shown for understanding).

    Calculate total product, then divide by each element.
    Problem: Doesn't work with zeros and problem forbids division.

    Time Complexity: O(n)
    Space Complexity: O(1) excluding output array
    """
    total_product = 1
    zero_count = 0

    for num in nums:
        if num != 0:
            total_product *= num
        else:
            zero_count += 1

    # If more than one zero, all products are zero
    if zero_count > 1:
        return [0] * len(nums)

    result = []
    for num in nums:
        if zero_count == 1:
            result.append(0 if num != 0 else total_product)
        else:
            result.append(total_product // num)

    return result


def product_except_self_prefix_suffix_arrays(nums: List[int]) -> List[int]:
    """
    Prefix/Suffix arrays approach: Calculate products before and after each index.

    Time Complexity: O(n)
    Space Complexity: O(n) for prefix and suffix arrays

    Why it works:
    - product_except_self[i] = (product of all elements before i) * (product of all after i)
    - We precompute prefix products: [1, nums[0], nums[0]*nums[1], ...]
    - We precompute suffix products: [..., nums[n-2]*nums[n-1], nums[n-1], 1]
    - result[i] = prefix[i] * suffix[i]
    """
    n = len(nums)

    # Prefix products: prefix[i] = product of all elements before index i
    prefix = [1] * n
    for i in range(1, n):
        prefix[i] = prefix[i - 1] * nums[i - 1]

    # Suffix products: suffix[i] = product of all elements after index i
    suffix = [1] * n
    for i in range(n - 2, -1, -1):
        suffix[i] = suffix[i + 1] * nums[i + 1]

    # Combine prefix and suffix
    result = []
    for i in range(n):
        result.append(prefix[i] * suffix[i])

    return result


def product_except_self(nums: List[int]) -> List[int]:
    """
    Optimal approach: Use output array to store prefix, then multiply suffix in-place.

    Time Complexity: O(n)
    Space Complexity: O(1) excluding output array (which doesn't count per problem)

    Why it works:
    - First pass: Build prefix products in result array
    - Second pass: Multiply by suffix products on the fly
    - This eliminates the need for separate prefix/suffix arrays
    """
    n = len(nums)
    result = [1] * n

    # Calculate prefix products and store in result
    # result[i] contains product of all elements before index i
    prefix = 1
    for i in range(n):
        result[i] = prefix
        prefix *= nums[i]

    # Calculate suffix products and multiply with existing prefix products
    # Traverse from right to left
    suffix = 1
    for i in range(n - 1, -1, -1):
        result[i] *= suffix  # Multiply existing prefix with suffix
        suffix *= nums[i]

    return result


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [1, 2, 3, 4],
            'expected': [24, 12, 8, 6],
            'description': 'Basic case: [2*3*4, 1*3*4, 1*2*4, 1*2*3]'
        },
        {
            'nums': [-1, 1, 0, -3, 3],
            'expected': [0, 0, 9, 0, 0],
            'description': 'Contains zero'
        },
        {
            'nums': [2, 3, 4, 5],
            'expected': [60, 40, 30, 24],
            'description': 'All positive numbers'
        },
        {
            'nums': [-1, -2, -3, -4],
            'expected': [-24, -12, -8, -6],
            'description': 'All negative numbers'
        },
        {
            'nums': [1, 1],
            'expected': [1, 1],
            'description': 'Two ones'
        },
        {
            'nums': [5],
            'expected': [1],
            'description': 'Single element'
        },
        {
            'nums': [0, 0],
            'expected': [0, 0],
            'description': 'Multiple zeros'
        }
    ]

    print("=" * 60)
    print("PRODUCT OF ARRAY EXCEPT SELF - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = product_except_self(test['nums'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['nums']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Division: Time O(n), Space O(1) - NOT ALLOWED")
    print("Prefix/Suffix Arrays: Time O(n), Space O(n)")
    print("Optimal (In-place): Time O(n), Space O(1) excluding output")
    print("\nKey Insight: Build prefix products left-to-right,")
    print("then multiply suffix products right-to-left")

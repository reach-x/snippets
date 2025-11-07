#!/usr/bin/env python3

def two_sum(nums, target):
    """
    Find two numbers that add up to target
    Returns indices of the two numbers
    """
    seen = {}

    for i, num in enumerate(nums):
        complement = target - num
        if complement in seen:
            return [seen[complement], i]
        seen[num] = i

    return []

def two_sum_brute_force(nums, target):
    """Brute force approach - O(n^2)"""
    for i in range(len(nums)):
        for j in range(i + 1, len(nums)):
            if nums[i] + nums[j] == target:
                return [i, j]
    return []

if __name__ == '__main__':
    nums = [2, 7, 11, 15]
    target = 9

    print(f"Array: {nums}")
    print(f"Target: {target}")
    result = two_sum(nums, target)
    print(f"Indices: {result}")
    if result:
        print(f"Values: [{nums[result[0]]}, {nums[result[1]]}]")

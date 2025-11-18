"""
Container With Most Water

You are given an integer array height of length n. There are n vertical lines
drawn such that the two endpoints of the ith line are (i, 0) and (i, height[i]).

Find two lines that together with the x-axis form a container, such that the
container contains the most water.

Return the maximum amount of water a container can store.

LeetCode: https://leetcode.com/problems/container-with-most-water/
Difficulty: Medium
Pattern: Two Pointers
"""

from typing import List


def max_area_brute_force(height: List[int]) -> int:
    """
    Brute force approach: Try all pairs of lines.

    Time Complexity: O(n^2)
    Space Complexity: O(1)
    """
    max_water = 0

    for i in range(len(height)):
        for j in range(i + 1, len(height)):
            # Width is distance between lines
            width = j - i
            # Height is limited by shorter line
            current_height = min(height[i], height[j])
            # Area = width * height
            area = width * current_height
            max_water = max(max_water, area)

    return max_water


def max_area(height: List[int]) -> int:
    """
    Optimal two pointers approach.

    Time Complexity: O(n)
    Space Complexity: O(1)

    Why it works:
    - Start with widest container (leftmost and rightmost lines)
    - Area is limited by the shorter line
    - To potentially find larger area, we must move the shorter line inward
    - Moving the taller line inward can only decrease area (less width, same or less height)
    - Moving the shorter line inward might find a taller line that compensates for less width

    Key insight: Greedily move the pointer at the shorter line
    """
    left = 0
    right = len(height) - 1
    max_water = 0

    while left < right:
        # Calculate current area
        width = right - left
        current_height = min(height[left], height[right])
        area = width * current_height
        max_water = max(max_water, area)

        # Move the pointer at the shorter line
        # This is the only way we might find a larger area
        if height[left] < height[right]:
            left += 1
        else:
            right -= 1

    return max_water


def max_area_with_details(height: List[int]) -> dict:
    """
    Two pointers approach with tracking of best container indices.
    """
    left = 0
    right = len(height) - 1
    max_water = 0
    best_left = 0
    best_right = 0

    while left < right:
        width = right - left
        current_height = min(height[left], height[right])
        area = width * current_height

        if area > max_water:
            max_water = area
            best_left = left
            best_right = right

        if height[left] < height[right]:
            left += 1
        else:
            right -= 1

    return {
        'max_area': max_water,
        'left_index': best_left,
        'right_index': best_right,
        'left_height': height[best_left],
        'right_height': height[best_right],
        'width': best_right - best_left,
        'height': min(height[best_left], height[best_right])
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'height': [1, 8, 6, 2, 5, 4, 8, 3, 7],
            'expected': 49,
            'description': 'Example case: area between index 1 and 8 (7 * 7 = 49)'
        },
        {
            'height': [1, 1],
            'expected': 1,
            'description': 'Two lines of height 1'
        },
        {
            'height': [4, 3, 2, 1, 4],
            'expected': 16,
            'description': 'First and last lines: width 4, height 4'
        },
        {
            'height': [1, 2, 1],
            'expected': 2,
            'description': 'Use outer lines'
        },
        {
            'height': [2, 3, 4, 5, 18, 17, 6],
            'expected': 17,
            'description': 'Tall lines in middle'
        },
        {
            'height': [1, 3, 2, 5, 25, 24, 5],
            'expected': 24,
            'description': 'Very tall line in middle'
        }
    ]

    print("=" * 60)
    print("CONTAINER WITH MOST WATER - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = max_area(test['height'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['height']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")

        # Show details
        details = max_area_with_details(test['height'])
        print(f"Best container: index {details['left_index']} to {details['right_index']}")
        print(f"  Width: {details['width']}, Height: {details['height']}")
        print(f"  Left height: {details['left_height']}, Right height: {details['right_height']}")
        print(f"  Area: {details['width']} × {details['height']} = {details['max_area']}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Brute Force: Time O(n^2), Space O(1)")
    print("Two Pointers (Optimal): Time O(n), Space O(1)")
    print("\nKey Insight: Always move the pointer at the shorter line")
    print("because moving the taller line can only decrease area")

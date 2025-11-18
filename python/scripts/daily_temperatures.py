"""
Daily Temperatures

Given an array of integers temperatures represents the daily temperatures, return
an array answer such that answer[i] is the number of days you have to wait after
the ith day to get a warmer temperature. If there is no future day for which this
is possible, keep answer[i] == 0 instead.

LeetCode: https://leetcode.com/problems/daily-temperatures/
Difficulty: Medium
Pattern: Monotonic Stack
"""

from typing import List


def daily_temperatures_brute_force(temperatures: List[int]) -> List[int]:
    """
    Brute force approach: For each day, scan forward to find warmer day.

    Time Complexity: O(n^2)
    Space Complexity: O(1) excluding output
    """
    n = len(temperatures)
    result = [0] * n

    for i in range(n):
        for j in range(i + 1, n):
            if temperatures[j] > temperatures[i]:
                result[i] = j - i
                break

    return result


def daily_temperatures(temperatures: List[int]) -> List[int]:
    """
    Optimal monotonic stack approach.

    Time Complexity: O(n) - each index pushed and popped at most once
    Space Complexity: O(n) for the stack

    Why it works (Monotonic Decreasing Stack):
    - Stack stores indices of days we haven't found warmer days for yet
    - Stack is kept in decreasing temperature order (top to bottom)
    - When we see a warmer day, it's the answer for all cooler days on stack
    - Pop all days that found their answer, push current day

    Key insight: We only care about days that might have future warmer days
    """
    n = len(temperatures)
    result = [0] * n
    stack = []  # Stack of indices (monotonic decreasing by temperature)

    for current_day in range(n):
        current_temp = temperatures[current_day]

        # Process all previous days that are cooler than today
        # Today is the first warmer day for all of them
        while stack and temperatures[stack[-1]] < current_temp:
            prev_day = stack.pop()
            result[prev_day] = current_day - prev_day

        # Add current day to stack
        # We haven't found a warmer day for it yet
        stack.append(current_day)

    # Days remaining in stack have no warmer future day (already 0)
    return result


def daily_temperatures_with_explanation(temperatures: List[int]) -> dict:
    """
    Monotonic stack with detailed step-by-step explanation.
    """
    n = len(temperatures)
    result = [0] * n
    stack = []
    steps = []

    for current_day in range(n):
        current_temp = temperatures[current_day]
        step_info = {
            'day': current_day,
            'temp': current_temp,
            'actions': []
        }

        # Pop cooler days
        while stack and temperatures[stack[-1]] < current_temp:
            prev_day = stack.pop()
            wait_days = current_day - prev_day
            result[prev_day] = wait_days
            step_info['actions'].append(
                f"Day {prev_day} (temp {temperatures[prev_day]}) waits {wait_days} days"
            )

        stack.append(current_day)
        step_info['stack'] = stack.copy()
        steps.append(step_info)

    return {
        'result': result,
        'steps': steps
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'temperatures': [73, 74, 75, 71, 69, 72, 76, 73],
            'expected': [1, 1, 4, 2, 1, 1, 0, 0],
            'description': 'Standard case with multiple patterns'
        },
        {
            'temperatures': [30, 40, 50, 60],
            'expected': [1, 1, 1, 0],
            'description': 'Increasing temperatures'
        },
        {
            'temperatures': [30, 60, 90],
            'expected': [1, 1, 0],
            'description': 'Strictly increasing'
        },
        {
            'temperatures': [90, 80, 70, 60],
            'expected': [0, 0, 0, 0],
            'description': 'Decreasing temperatures - no warmer days'
        },
        {
            'temperatures': [75, 75, 75],
            'expected': [0, 0, 0],
            'description': 'All same temperature'
        },
        {
            'temperatures': [89, 62, 70, 58, 47, 47, 46, 76, 100, 70],
            'expected': [8, 1, 5, 4, 3, 2, 1, 1, 0, 0],
            'description': 'Complex case'
        }
    ]

    print("=" * 60)
    print("DAILY TEMPERATURES - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = daily_temperatures(test['temperatures'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['temperatures']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    # Show detailed example
    print("\n" + "=" * 60)
    print("DETAILED EXAMPLE: [73, 74, 75, 71, 69, 72, 76, 73]")
    print("=" * 60)

    example = [73, 74, 75, 71, 69, 72, 76, 73]
    details = daily_temperatures_with_explanation(example)

    for step in details['steps']:
        print(f"\nDay {step['day']}: Temp = {step['temp']}")
        if step['actions']:
            for action in step['actions']:
                print(f"  - {action}")
        print(f"  Stack (indices): {step['stack']}")

    print(f"\nFinal result: {details['result']}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Brute Force: Time O(n^2), Space O(1)")
    print("Monotonic Stack (Optimal): Time O(n), Space O(n)")
    print("\nKey Insight: Maintain stack of indices in decreasing temp order")
    print("When we see warmer day, it answers all cooler days on stack")

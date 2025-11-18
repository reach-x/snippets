"""
11. Merge Intervals

Given an array of intervals where intervals[i] = [start_i, end_i], merge all
overlapping intervals, and return an array of the non-overlapping intervals
that cover all the intervals in the input.

LeetCode: https://leetcode.com/problems/merge-intervals/
Difficulty: Medium
Pattern: Sorting, Intervals
"""

from typing import List


def merge_intervals(intervals: List[List[int]]) -> List[List[int]]:
    """
    Merge overlapping intervals.

    Time Complexity: O(n log n) due to sorting
    Space Complexity: O(n) for output (or O(log n) if not counting output)
    """
    if not intervals:
        return []

    # Sort by start time
    intervals.sort(key=lambda x: x[0])

    merged = [intervals[0]]

    for current in intervals[1:]:
        last_merged = merged[-1]

        # If current overlaps with last merged interval
        if current[0] <= last_merged[1]:
            # Merge by updating the end time
            last_merged[1] = max(last_merged[1], current[1])
        else:
            # No overlap, add as new interval
            merged.append(current)

    return merged


def merge_intervals_detailed(intervals: List[List[int]]) -> dict:
    """
    Returns detailed merge information.
    """
    if not intervals:
        return {
            'merged': [],
            'original_count': 0,
            'merged_count': 0,
            'steps': []
        }

    intervals.sort(key=lambda x: x[0])
    merged = [intervals[0]]
    steps = [f"Start with first interval: {intervals[0]}"]

    for i, current in enumerate(intervals[1:], 1):
        last_merged = merged[-1]

        if current[0] <= last_merged[1]:
            old_end = last_merged[1]
            last_merged[1] = max(last_merged[1], current[1])
            steps.append(f"Step {i}: Merge {current} with {[last_merged[0], old_end]} -> {last_merged}")
        else:
            merged.append(current)
            steps.append(f"Step {i}: Add new interval {current} (no overlap)")

    return {
        'merged': merged,
        'original_count': len(intervals),
        'merged_count': len(merged),
        'reduction': len(intervals) - len(merged),
        'steps': steps
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'intervals': [[1, 3], [2, 6], [8, 10], [15, 18]],
            'expected': [[1, 6], [8, 10], [15, 18]],
            'description': 'Basic merge case'
        },
        {
            'intervals': [[1, 4], [4, 5]],
            'expected': [[1, 5]],
            'description': 'Adjacent intervals (touching)'
        },
        {
            'intervals': [[1, 4], [0, 4]],
            'expected': [[0, 4]],
            'description': 'Second starts before first'
        },
        {
            'intervals': [[1, 4], [2, 3]],
            'expected': [[1, 4]],
            'description': 'Second contained in first'
        },
        {
            'intervals': [[1, 4], [5, 6]],
            'expected': [[1, 4], [5, 6]],
            'description': 'No overlap'
        },
        {
            'intervals': [[1, 10], [2, 3], [4, 5], [6, 7], [8, 9]],
            'expected': [[1, 10]],
            'description': 'All contained in first'
        },
        {
            'intervals': [[2, 3], [4, 5], [6, 7], [8, 9], [1, 10]],
            'expected': [[1, 10]],
            'description': 'Unsorted input'
        }
    ]

    print("=" * 70)
    print("MERGE INTERVALS - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        result = merge_intervals(test['intervals'].copy())
        details = merge_intervals_detailed(test['intervals'].copy())
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['intervals']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Original Count: {details['original_count']}")
        print(f"Merged Count: {details['merged_count']}")
        print(f"Reduction: {details['reduction']} intervals merged")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("ALGORITHM STEPS")
    print("=" * 70)
    print("""
1. Sort intervals by start time
2. Initialize result with first interval
3. For each subsequent interval:
   - If it overlaps with last interval in result:
     - Merge by extending the end time
   - Otherwise:
     - Add as new interval to result
4. Return merged intervals

Overlap Condition:
  current.start <= last.end

Merge Operation:
  last.end = max(last.end, current.end)
""")

    print("\n" + "=" * 70)
    print("KEY INSIGHTS")
    print("=" * 70)
    print("- Sorting is crucial for O(n) merging after O(n log n) sort")
    print("- Use max() when merging to handle contained intervals")
    print("- Watch for edge case: [1,4] and [4,5] should merge")
    print("- Time: O(n log n), Space: O(n)")

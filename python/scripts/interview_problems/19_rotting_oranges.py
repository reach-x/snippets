"""
19. Rotting Oranges

You are given an m x n grid where each cell can have one of three values:
- 0 representing an empty cell,
- 1 representing a fresh orange, or
- 2 representing a rotten orange.

Every minute, any fresh orange that is 4-directionally adjacent to a rotten orange
becomes rotten. Return the minimum number of minutes that must elapse until no cell
has a fresh orange. If this is impossible, return -1.

LeetCode: https://leetcode.com/problems/rotting-oranges/
Difficulty: Medium
Pattern: BFS, Multi-source BFS
"""

from typing import List
from collections import deque


def oranges_rotting(grid: List[List[int]]) -> int:
    """
    Multi-source BFS approach.

    Time Complexity: O(m * n)
    Space Complexity: O(m * n)
    """
    if not grid or not grid[0]:
        return -1

    rows, cols = len(grid), len(grid[0])
    queue = deque()
    fresh_count = 0

    # Find all rotten oranges and count fresh oranges
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 2:
                queue.append((r, c, 0))  # (row, col, time)
            elif grid[r][c] == 1:
                fresh_count += 1

    # If no fresh oranges, return 0
    if fresh_count == 0:
        return 0

    directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    max_time = 0

    # BFS from all rotten oranges simultaneously
    while queue:
        r, c, time = queue.popleft()
        max_time = max(max_time, time)

        for dr, dc in directions:
            nr, nc = r + dr, c + dc

            if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] == 1:
                grid[nr][nc] = 2  # Mark as rotten
                fresh_count -= 1
                queue.append((nr, nc, time + 1))

    # If there are still fresh oranges, return -1
    return max_time if fresh_count == 0 else -1


def oranges_rotting_with_details(grid: List[List[int]]) -> dict:
    """
    Returns detailed information about the rotting process.
    """
    if not grid or not grid[0]:
        return {'minutes': -1, 'steps': []}

    rows, cols = len(grid), len(grid[0])
    queue = deque()
    fresh_count = 0
    steps = []

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 2:
                queue.append((r, c, 0))
            elif grid[r][c] == 1:
                fresh_count += 1

    initial_fresh = fresh_count

    if fresh_count == 0:
        return {'minutes': 0, 'initial_fresh': 0, 'rotted': 0, 'steps': []}

    directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    max_time = 0
    current_time = -1

    while queue:
        r, c, time = queue.popleft()
        max_time = max(max_time, time)

        if time != current_time:
            current_time = time
            if current_time > 0:
                steps.append(f"Minute {current_time}: {initial_fresh - fresh_count} oranges rotted")

        for dr, dc in directions:
            nr, nc = r + dr, c + dc

            if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] == 1:
                grid[nr][nc] = 2
                fresh_count -= 1
                queue.append((nr, nc, time + 1))

    return {
        'minutes': max_time if fresh_count == 0 else -1,
        'initial_fresh': initial_fresh,
        'rotted': initial_fresh - fresh_count,
        'remaining_fresh': fresh_count,
        'steps': steps
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'grid': [
                [2, 1, 1],
                [1, 1, 0],
                [0, 1, 1]
            ],
            'expected': 4,
            'description': 'Standard case'
        },
        {
            'grid': [
                [2, 1, 1],
                [0, 1, 1],
                [1, 0, 1]
            ],
            'expected': -1,
            'description': 'Impossible to rot all'
        },
        {
            'grid': [[0, 2]],
            'expected': 0,
            'description': 'No fresh oranges'
        },
        {
            'grid': [[1]],
            'expected': -1,
            'description': 'Only fresh, no rotten'
        },
        {
            'grid': [
                [2, 2],
                [1, 1]
            ],
            'expected': 1,
            'description': 'Multiple rotten sources'
        }
    ]

    print("=" * 70)
    print("ROTTING ORANGES - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        grid_copy = [row[:] for row in test['grid']]
        result = oranges_rotting(grid_copy)

        grid_details = [row[:] for row in test['grid']]
        details = oranges_rotting_with_details(grid_details)

        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Initial Grid:")
        for row in test['grid']:
            print(f"  {row}")
        print(f"Expected: {test['expected']} minutes")
        print(f"Got: {result} minutes")

        if details['minutes'] >= 0:
            print(f"Initial fresh oranges: {details['initial_fresh']}")
            print(f"Rotted: {details['rotted']}")
            print(f"Remaining fresh: {details['remaining_fresh']}")
        else:
            print(f"Impossible to rot all oranges")

        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("MULTI-SOURCE BFS EXPLANATION")
    print("=" * 70)
    print("""
Algorithm:
1. Find all rotten oranges and add to queue
2. Count fresh oranges
3. BFS from all rotten oranges simultaneously
4. Each level represents one minute
5. If any fresh oranges remain, return -1

Key Insight:
- Multi-source BFS: Start from all rotten oranges at once
- They spread simultaneously, not one by one
- Track time with each queue entry (r, c, time)
- Level-by-level traversal ensures minimum time
""")

    print("\n" + "=" * 70)
    print("COMPLEXITY ANALYSIS")
    print("=" * 70)
    print("Time: O(m * n) - visit each cell at most once")
    print("Space: O(m * n) - queue can hold all cells in worst case")
    print("\nThis is a classic multi-source BFS problem")

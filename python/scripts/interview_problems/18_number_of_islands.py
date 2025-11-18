"""
18. Number of Islands

Given an m x n 2D binary grid which represents a map of '1's (land) and '0's (water),
return the number of islands. An island is surrounded by water and is formed by
connecting adjacent lands horizontally or vertically. You may assume all four edges
of the grid are all surrounded by water.

LeetCode: https://leetcode.com/problems/number-of-islands/
Difficulty: Medium
Pattern: DFS, BFS, Union Find, Matrix
"""

from typing import List
from collections import deque


def num_islands_dfs(grid: List[List[str]]) -> int:
    """
    DFS approach (recursive).

    Time Complexity: O(m * n)
    Space Complexity: O(m * n) for recursion stack in worst case
    """
    if not grid or not grid[0]:
        return 0

    rows, cols = len(grid), len(grid[0])
    count = 0

    def dfs(r: int, c: int):
        # Check boundaries and if it's land
        if r < 0 or r >= rows or c < 0 or c >= cols or grid[r][c] != '1':
            return

        # Mark as visited by changing to '0'
        grid[r][c] = '0'

        # Explore all 4 directions
        dfs(r + 1, c)  # down
        dfs(r - 1, c)  # up
        dfs(r, c + 1)  # right
        dfs(r, c - 1)  # left

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '1':
                count += 1
                dfs(r, c)  # Mark entire island as visited

    return count


def num_islands_bfs(grid: List[List[str]]) -> int:
    """
    BFS approach (iterative).

    Time Complexity: O(m * n)
    Space Complexity: O(min(m, n)) for queue
    """
    if not grid or not grid[0]:
        return 0

    rows, cols = len(grid), len(grid[0])
    count = 0
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]

    def bfs(start_r: int, start_c: int):
        queue = deque([(start_r, start_c)])
        grid[start_r][start_c] = '0'

        while queue:
            r, c = queue.popleft()

            for dr, dc in directions:
                nr, nc = r + dr, c + dc

                if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] == '1':
                    grid[nr][nc] = '0'
                    queue.append((nr, nc))

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '1':
                count += 1
                bfs(r, c)

    return count


# Alias for optimal solution
num_islands = num_islands_dfs


def num_islands_with_details(grid: List[List[str]]) -> dict:
    """
    Returns detailed information about islands.
    """
    if not grid or not grid[0]:
        return {'count': 0, 'islands': []}

    rows, cols = len(grid), len(grid[0])
    islands = []

    def dfs(r: int, c: int, island_cells: List):
        if r < 0 or r >= rows or c < 0 or c >= cols or grid[r][c] != '1':
            return

        grid[r][c] = '0'
        island_cells.append((r, c))

        dfs(r + 1, c, island_cells)
        dfs(r - 1, c, island_cells)
        dfs(r, c + 1, island_cells)
        dfs(r, c - 1, island_cells)

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '1':
                island_cells = []
                dfs(r, c, island_cells)
                islands.append({
                    'id': len(islands) + 1,
                    'cells': island_cells,
                    'size': len(island_cells)
                })

    return {
        'count': len(islands),
        'islands': islands
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'grid': [
                ['1', '1', '1', '1', '0'],
                ['1', '1', '0', '1', '0'],
                ['1', '1', '0', '0', '0'],
                ['0', '0', '0', '0', '0']
            ],
            'expected': 1,
            'description': 'One large island'
        },
        {
            'grid': [
                ['1', '1', '0', '0', '0'],
                ['1', '1', '0', '0', '0'],
                ['0', '0', '1', '0', '0'],
                ['0', '0', '0', '1', '1']
            ],
            'expected': 3,
            'description': 'Three separate islands'
        },
        {
            'grid': [
                ['1', '0', '1', '0', '1'],
                ['0', '1', '0', '1', '0'],
                ['1', '0', '1', '0', '1']
            ],
            'expected': 10,
            'description': 'Many small islands'
        },
        {
            'grid': [['1']],
            'expected': 1,
            'description': 'Single cell island'
        },
        {
            'grid': [['0']],
            'expected': 0,
            'description': 'No islands'
        }
    ]

    print("=" * 70)
    print("NUMBER OF ISLANDS - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        # Test DFS
        grid_dfs = [row[:] for row in test['grid']]
        result_dfs = num_islands_dfs(grid_dfs)

        # Test BFS
        grid_bfs = [row[:] for row in test['grid']]
        result_bfs = num_islands_bfs(grid_bfs)

        # Get details
        grid_details = [row[:] for row in test['grid']]
        details = num_islands_with_details(grid_details)

        passed = result_dfs == test['expected'] and result_bfs == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Grid:")
        for row in test['grid']:
            print(f"  {' '.join(row)}")
        print(f"Expected: {test['expected']} islands")
        print(f"Got (DFS): {result_dfs} islands")
        print(f"Got (BFS): {result_bfs} islands")

        if details['islands']:
            print(f"Island details:")
            for island in details['islands']:
                print(f"  Island {island['id']}: size {island['size']}, cells: {len(island['cells'])}")

        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("ALGORITHM EXPLANATION")
    print("=" * 70)
    print("""
DFS Approach:
1. Iterate through each cell in the grid
2. When we find a '1' (land):
   - Increment island count
   - Use DFS to mark all connected land as visited
   - DFS explores all 4 directions
3. Continue until all cells are processed

Key Insight:
- Each DFS call marks an entire island as visited
- So we only count once per connected component
""")

    print("\n" + "=" * 70)
    print("APPROACH COMPARISON")
    print("=" * 70)
    print("DFS (Recursive): Time O(m*n), Space O(m*n) worst case")
    print("BFS (Iterative): Time O(m*n), Space O(min(m,n))")
    print("\nBoth are acceptable, DFS is more concise")
    print("We modify the grid in-place to mark visited cells")

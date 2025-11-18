"""
8. Flood Fill

An image is represented by an m x n integer grid image where image[i][j] represents
the pixel value of the image. You are also given three integers sr, sc, and color.
You should perform a flood fill on the image starting from the pixel image[sr][sc].

To perform a flood fill, consider the starting pixel, plus any pixels connected
4-directionally to the starting pixel of the same color as the starting pixel,
plus any pixels connected 4-directionally to those pixels (also with the same color),
and so on. Replace the color of all of the aforementioned pixels with color.

LeetCode: https://leetcode.com/problems/flood-fill/
Difficulty: Easy
Pattern: DFS, BFS, Matrix
"""

from typing import List
from collections import deque
import copy


def flood_fill_dfs(image: List[List[int]], sr: int, sc: int, color: int) -> List[List[int]]:
    """
    DFS approach (recursive).

    Time Complexity: O(m * n)
    Space Complexity: O(m * n) for recursion stack in worst case
    """
    if not image or image[sr][sc] == color:
        return image

    original_color = image[sr][sc]
    rows, cols = len(image), len(image[0])

    def dfs(r: int, c: int):
        # Check boundaries and color
        if r < 0 or r >= rows or c < 0 or c >= cols or image[r][c] != original_color:
            return

        # Fill current pixel
        image[r][c] = color

        # Fill 4-directionally connected pixels
        dfs(r + 1, c)  # down
        dfs(r - 1, c)  # up
        dfs(r, c + 1)  # right
        dfs(r, c - 1)  # left

    dfs(sr, sc)
    return image


def flood_fill_bfs(image: List[List[int]], sr: int, sc: int, color: int) -> List[List[int]]:
    """
    BFS approach (iterative).

    Time Complexity: O(m * n)
    Space Complexity: O(m * n) for queue in worst case
    """
    if not image or image[sr][sc] == color:
        return image

    original_color = image[sr][sc]
    rows, cols = len(image), len(image[0])
    queue = deque([(sr, sc)])
    image[sr][sc] = color

    directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]

    while queue:
        r, c = queue.popleft()

        for dr, dc in directions:
            nr, nc = r + dr, c + dc

            if 0 <= nr < rows and 0 <= nc < cols and image[nr][nc] == original_color:
                image[nr][nc] = color
                queue.append((nr, nc))

    return image


# Alias for optimal solution
flood_fill = flood_fill_dfs


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'image': [[1, 1, 1], [1, 1, 0], [1, 0, 1]],
            'sr': 1,
            'sc': 1,
            'color': 2,
            'expected': [[2, 2, 2], [2, 2, 0], [2, 0, 1]],
            'description': 'Basic flood fill from center'
        },
        {
            'image': [[0, 0, 0], [0, 0, 0]],
            'sr': 0,
            'sc': 0,
            'color': 0,
            'expected': [[0, 0, 0], [0, 0, 0]],
            'description': 'Same color - no change'
        },
        {
            'image': [[0, 0, 0], [0, 1, 1]],
            'sr': 1,
            'sc': 1,
            'color': 1,
            'expected': [[0, 0, 0], [0, 1, 1]],
            'description': 'Fill with same color'
        },
        {
            'image': [[1]],
            'sr': 0,
            'sc': 0,
            'color': 2,
            'expected': [[2]],
            'description': 'Single pixel'
        },
        {
            'image': [[0, 0, 0], [0, 1, 0], [0, 0, 0]],
            'sr': 1,
            'sc': 1,
            'color': 2,
            'expected': [[0, 0, 0], [0, 2, 0], [0, 0, 0]],
            'description': 'Isolated pixel'
        }
    ]

    print("=" * 60)
    print("FLOOD FILL - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        # Test DFS
        image_dfs = copy.deepcopy(test['image'])
        result_dfs = flood_fill_dfs(image_dfs, test['sr'], test['sc'], test['color'])
        passed_dfs = result_dfs == test['expected']

        # Test BFS
        image_bfs = copy.deepcopy(test['image'])
        result_bfs = flood_fill_bfs(image_bfs, test['sr'], test['sc'], test['color'])
        passed_bfs = result_bfs == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Original Image:")
        for row in test['image']:
            print(f"  {row}")
        print(f"Start: ({test['sr']}, {test['sc']}), New Color: {test['color']}")
        print(f"Expected:")
        for row in test['expected']:
            print(f"  {row}")
        print(f"Got (DFS):")
        for row in result_dfs:
            print(f"  {row}")
        print(f"DFS Status: {'PASS' if passed_dfs else 'FAIL'}")
        print(f"BFS Status: {'PASS' if passed_bfs else 'FAIL'}")

    print("\n" + "=" * 60)
    print("APPROACH COMPARISON")
    print("=" * 60)
    print("DFS (Recursive): Time O(m*n), Space O(m*n)")
    print("BFS (Iterative): Time O(m*n), Space O(m*n)")
    print("\nBoth approaches have the same complexity.")
    print("DFS is more intuitive, BFS uses explicit queue.")
    print("\n4-Directional Movement: Up, Down, Left, Right")
    print("Directions: [(0,1), (0,-1), (1,0), (-1,0)]")

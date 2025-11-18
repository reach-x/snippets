"""
12. Word Search

Given an m x n grid of characters board and a string word, return true if word
exists in the grid. The word can be constructed from letters of sequentially
adjacent cells, where adjacent cells are horizontally or vertically neighboring.
The same letter cell may not be used more than once.

LeetCode: https://leetcode.com/problems/word-search/
Difficulty: Medium
Pattern: Backtracking, DFS, Matrix
"""

from typing import List


def word_search(board: List[List[str]], word: str) -> bool:
    """
    Backtracking DFS solution.

    Time Complexity: O(m * n * 4^L) where L is length of word
    Space Complexity: O(L) for recursion stack
    """
    if not board or not board[0]:
        return False

    rows, cols = len(board), len(board[0])

    def dfs(r: int, c: int, index: int) -> bool:
        # Found complete word
        if index == len(word):
            return True

        # Check boundaries and character match
        if (r < 0 or r >= rows or c < 0 or c >= cols or
            board[r][c] != word[index]):
            return False

        # Mark as visited
        temp = board[r][c]
        board[r][c] = '#'

        # Explore all 4 directions
        found = (dfs(r + 1, c, index + 1) or  # down
                dfs(r - 1, c, index + 1) or  # up
                dfs(r, c + 1, index + 1) or  # right
                dfs(r, c - 1, index + 1))    # left

        # Restore cell (backtrack)
        board[r][c] = temp

        return found

    # Try starting from each cell
    for r in range(rows):
        for c in range(cols):
            if board[r][c] == word[0] and dfs(r, c, 0):
                return True

    return False


def word_search_with_path(board: List[List[str]], word: str) -> dict:
    """
    Returns the path if word is found.
    """
    if not board or not board[0]:
        return {'found': False, 'path': []}

    rows, cols = len(board), len(board[0])
    path = []

    def dfs(r: int, c: int, index: int) -> bool:
        if index == len(word):
            return True

        if (r < 0 or r >= rows or c < 0 or c >= cols or
            board[r][c] != word[index]):
            return False

        temp = board[r][c]
        board[r][c] = '#'
        path.append((r, c, temp))

        found = (dfs(r + 1, c, index + 1) or
                dfs(r - 1, c, index + 1) or
                dfs(r, c + 1, index + 1) or
                dfs(r, c - 1, index + 1))

        if not found:
            path.pop()

        board[r][c] = temp

        return found

    for r in range(rows):
        for c in range(cols):
            if board[r][c] == word[0] and dfs(r, c, 0):
                return {
                    'found': True,
                    'path': path,
                    'coordinates': [(p[0], p[1]) for p in path],
                    'letters': [p[2] for p in path]
                }

    return {'found': False, 'path': []}


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'board': [
                ['A', 'B', 'C', 'E'],
                ['S', 'F', 'C', 'S'],
                ['A', 'D', 'E', 'E']
            ],
            'word': 'ABCCED',
            'expected': True,
            'description': 'Word exists with turns'
        },
        {
            'board': [
                ['A', 'B', 'C', 'E'],
                ['S', 'F', 'C', 'S'],
                ['A', 'D', 'E', 'E']
            ],
            'word': 'SEE',
            'expected': True,
            'description': 'Simple path'
        },
        {
            'board': [
                ['A', 'B', 'C', 'E'],
                ['S', 'F', 'C', 'S'],
                ['A', 'D', 'E', 'E']
            ],
            'word': 'ABCB',
            'expected': False,
            'description': 'Cannot reuse cells'
        },
        {
            'board': [['a']],
            'word': 'a',
            'expected': True,
            'description': 'Single cell match'
        },
        {
            'board': [['a', 'b'], ['c', 'd']],
            'word': 'abcd',
            'expected': False,
            'description': 'Word too long for board'
        }
    ]

    print("=" * 70)
    print("WORD SEARCH - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        board_copy = [row[:] for row in test['board']]
        result = word_search(board_copy, test['word'])

        board_copy = [row[:] for row in test['board']]
        details = word_search_with_path(board_copy, test['word'])

        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Board:")
        for row in test['board']:
            print(f"  {row}")
        print(f"Word: \"{test['word']}\"")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")

        if details['found']:
            print(f"Path found: {details['coordinates']}")
            print(f"Letters: {' -> '.join(details['letters'])}")

        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("BACKTRACKING TEMPLATE")
    print("=" * 70)
    print("""
def dfs(r, c, index):
    # Base case: found complete word
    if index == len(word):
        return True

    # Check boundaries and match
    if out_of_bounds or board[r][c] != word[index]:
        return False

    # Mark visited
    temp = board[r][c]
    board[r][c] = '#'

    # Try all 4 directions
    found = (dfs(r+1, c, index+1) or
             dfs(r-1, c, index+1) or
             dfs(r, c+1, index+1) or
             dfs(r, c-1, index+1))

    # Backtrack (restore)
    board[r][c] = temp

    return found
""")

    print("\n" + "=" * 70)
    print("KEY INSIGHTS")
    print("=" * 70)
    print("- Use board itself to track visited cells (mark with '#')")
    print("- Must restore cell after exploring (backtracking)")
    print("- Try all 4 directions at each step")
    print("- Time: O(m*n*4^L), Space: O(L) for recursion")

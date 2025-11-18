"""
9. Maximum Depth of Binary Tree

Given the root of a binary tree, return its maximum depth. A binary tree's maximum
depth is the number of nodes along the longest path from the root node down to the
farthest leaf node.

LeetCode: https://leetcode.com/problems/maximum-depth-of-binary-tree/
Difficulty: Easy
Pattern: DFS, BFS, Tree
"""

from typing import Optional
from collections import deque


class TreeNode:
    """Definition for a binary tree node."""
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right


def max_depth_dfs_recursive(root: Optional[TreeNode]) -> int:
    """
    DFS Recursive approach.

    Time Complexity: O(n)
    Space Complexity: O(h) where h is height (O(n) worst case for skewed tree)
    """
    if not root:
        return 0

    left_depth = max_depth_dfs_recursive(root.left)
    right_depth = max_depth_dfs_recursive(root.right)

    return 1 + max(left_depth, right_depth)


def max_depth_dfs_iterative(root: Optional[TreeNode]) -> int:
    """
    DFS Iterative approach using stack.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    if not root:
        return 0

    stack = [(root, 1)]
    max_depth = 0

    while stack:
        node, depth = stack.pop()
        max_depth = max(max_depth, depth)

        if node.left:
            stack.append((node.left, depth + 1))
        if node.right:
            stack.append((node.right, depth + 1))

    return max_depth


def max_depth_bfs(root: Optional[TreeNode]) -> int:
    """
    BFS approach using queue (level-order traversal).

    Time Complexity: O(n)
    Space Complexity: O(w) where w is max width of tree
    """
    if not root:
        return 0

    queue = deque([root])
    depth = 0

    while queue:
        depth += 1
        level_size = len(queue)

        for _ in range(level_size):
            node = queue.popleft()

            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)

    return depth


# Alias for optimal solution
max_depth = max_depth_dfs_recursive


# Helper Functions
def build_tree_from_list(values: list) -> Optional[TreeNode]:
    """
    Build binary tree from list representation (level-order).
    None represents null nodes.
    """
    if not values:
        return None

    root = TreeNode(values[0])
    queue = deque([root])
    i = 1

    while queue and i < len(values):
        node = queue.popleft()

        if i < len(values) and values[i] is not None:
            node.left = TreeNode(values[i])
            queue.append(node.left)
        i += 1

        if i < len(values) and values[i] is not None:
            node.right = TreeNode(values[i])
            queue.append(node.right)
        i += 1

    return root


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'tree': [3, 9, 20, None, None, 15, 7],
            'expected': 3,
            'description': 'Balanced tree'
        },
        {
            'tree': [1, None, 2],
            'expected': 2,
            'description': 'Right skewed tree'
        },
        {
            'tree': [],
            'expected': 0,
            'description': 'Empty tree'
        },
        {
            'tree': [0],
            'expected': 1,
            'description': 'Single node'
        },
        {
            'tree': [1, 2, 3, 4, 5],
            'expected': 3,
            'description': 'Complete tree'
        },
        {
            'tree': [1, 2, None, 3, None, 4, None, 5],
            'expected': 5,
            'description': 'Left skewed tree'
        }
    ]

    print("=" * 60)
    print("MAXIMUM DEPTH OF BINARY TREE - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        root = build_tree_from_list(test['tree'])

        result_dfs_rec = max_depth_dfs_recursive(root)
        result_dfs_iter = max_depth_dfs_iterative(root)
        result_bfs = max_depth_bfs(root)

        passed = (result_dfs_rec == test['expected'] and
                 result_dfs_iter == test['expected'] and
                 result_bfs == test['expected'])

        print(f"\nTest {i}: {test['description']}")
        print(f"Tree (level-order): {test['tree']}")
        print(f"Expected Depth: {test['expected']}")
        print(f"Got (DFS Recursive): {result_dfs_rec}")
        print(f"Got (DFS Iterative): {result_dfs_iter}")
        print(f"Got (BFS): {result_bfs}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("APPROACH COMPARISON")
    print("=" * 60)
    print("DFS Recursive: Time O(n), Space O(h)")
    print("DFS Iterative: Time O(n), Space O(n)")
    print("BFS: Time O(n), Space O(w)")
    print("\nwhere:")
    print("  n = total nodes")
    print("  h = height (log n for balanced, n for skewed)")
    print("  w = maximum width of tree")
    print("\nDFS Recursive is most concise and commonly used")

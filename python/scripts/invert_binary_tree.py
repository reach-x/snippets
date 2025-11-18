"""
Invert Binary Tree

Given the root of a binary tree, invert the tree, and return its root.

Inverting a tree means swapping the left and right children of all nodes.

LeetCode: https://leetcode.com/problems/invert-binary-tree/
Difficulty: Easy
Pattern: Tree Recursion / BFS / DFS
"""

from typing import Optional
from collections import deque


class TreeNode:
    """Definition for a binary tree node."""
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right


def invert_tree_recursive(root: Optional[TreeNode]) -> Optional[TreeNode]:
    """
    Recursive DFS approach (most elegant).

    Time Complexity: O(n) - visit each node once
    Space Complexity: O(h) - recursion stack, h = height of tree
                      O(n) worst case for skewed tree, O(log n) for balanced

    Why it works:
    - Recursively invert left and right subtrees
    - Swap the (now inverted) left and right children
    - Base case: null node returns null
    """
    # Base case: empty tree
    if not root:
        return None

    # Recursively invert left and right subtrees
    left_inverted = invert_tree_recursive(root.left)
    right_inverted = invert_tree_recursive(root.right)

    # Swap left and right children
    root.left = right_inverted
    root.right = left_inverted

    return root


def invert_tree(root: Optional[TreeNode]) -> Optional[TreeNode]:
    """
    Simplified recursive approach (cleaner).

    Time Complexity: O(n)
    Space Complexity: O(h)
    """
    if not root:
        return None

    # Swap children first
    root.left, root.right = root.right, root.left

    # Then recursively invert subtrees
    invert_tree(root.left)
    invert_tree(root.right)

    return root


def invert_tree_iterative_dfs(root: Optional[TreeNode]) -> Optional[TreeNode]:
    """
    Iterative DFS using stack.

    Time Complexity: O(n)
    Space Complexity: O(h) for the stack

    Why use iterative:
    - Avoids recursion stack overflow for very deep trees
    - More explicit control over traversal
    """
    if not root:
        return None

    stack = [root]

    while stack:
        node = stack.pop()

        # Swap left and right children
        node.left, node.right = node.right, node.left

        # Add children to stack for processing
        if node.left:
            stack.append(node.left)
        if node.right:
            stack.append(node.right)

    return root


def invert_tree_bfs(root: Optional[TreeNode]) -> Optional[TreeNode]:
    """
    BFS approach using queue (level-order traversal).

    Time Complexity: O(n)
    Space Complexity: O(w) where w = maximum width of tree

    Why use BFS:
    - More intuitive for some people (process level by level)
    - Better for wide trees (less space than DFS)
    """
    if not root:
        return None

    queue = deque([root])

    while queue:
        node = queue.popleft()

        # Swap left and right children
        node.left, node.right = node.right, node.left

        # Add children to queue for processing
        if node.left:
            queue.append(node.left)
        if node.right:
            queue.append(node.right)

    return root


# Helper functions for testing
def tree_to_list_bfs(root: Optional[TreeNode]) -> list:
    """Convert tree to list representation (BFS order)."""
    if not root:
        return []

    result = []
    queue = deque([root])

    while queue:
        node = queue.popleft()

        if node:
            result.append(node.val)
            queue.append(node.left)
            queue.append(node.right)
        else:
            result.append(None)

    # Remove trailing None values
    while result and result[-1] is None:
        result.pop()

    return result


def list_to_tree(values: list) -> Optional[TreeNode]:
    """Convert list representation to tree."""
    if not values:
        return None

    root = TreeNode(values[0])
    queue = deque([root])
    i = 1

    while queue and i < len(values):
        node = queue.popleft()

        # Left child
        if i < len(values) and values[i] is not None:
            node.left = TreeNode(values[i])
            queue.append(node.left)
        i += 1

        # Right child
        if i < len(values) and values[i] is not None:
            node.right = TreeNode(values[i])
            queue.append(node.right)
        i += 1

    return root


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'tree': [4, 2, 7, 1, 3, 6, 9],
            'expected': [4, 7, 2, 9, 6, 3, 1],
            'description': 'Complete binary tree'
        },
        {
            'tree': [2, 1, 3],
            'expected': [2, 3, 1],
            'description': 'Simple tree'
        },
        {
            'tree': [],
            'expected': [],
            'description': 'Empty tree'
        },
        {
            'tree': [1],
            'expected': [1],
            'description': 'Single node'
        },
        {
            'tree': [1, 2, None, 3],
            'expected': [1, None, 2, None, 3],
            'description': 'Left-skewed tree becomes right-skewed'
        }
    ]

    print("=" * 60)
    print("INVERT BINARY TREE - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        root = list_to_tree(test['tree'])
        inverted = invert_tree(root)
        result = tree_to_list_bfs(inverted)
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['tree']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("All approaches: Time O(n), visit each node once")
    print("Recursive DFS: Space O(h) - recursion stack")
    print("Iterative DFS: Space O(h) - explicit stack")
    print("BFS: Space O(w) - queue (w = max width)")
    print("\nKey Insight: Simply swap left and right children of every node")

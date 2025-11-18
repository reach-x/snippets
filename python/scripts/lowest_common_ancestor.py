"""
Lowest Common Ancestor of a Binary Search Tree

Given a binary search tree (BST), find the lowest common ancestor (LCA) of two
given nodes in the BST.

The lowest common ancestor is defined between two nodes p and q as the lowest
node in T that has both p and q as descendants (where we allow a node to be a
descendant of itself).

LeetCode: https://leetcode.com/problems/lowest-common-ancestor-of-a-binary-search-tree/
Difficulty: Medium
Pattern: Binary Search Tree / Tree Traversal
"""

from typing import Optional


class TreeNode:
    """Definition for a binary tree node."""
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right


def lowest_common_ancestor_bst_recursive(
    root: TreeNode, p: TreeNode, q: TreeNode
) -> TreeNode:
    """
    Recursive approach for BST (uses BST property).

    Time Complexity: O(h) where h = height of tree
    Space Complexity: O(h) for recursion stack

    Why it works (BST Property):
    - If both p and q are smaller than root, LCA is in left subtree
    - If both p and q are larger than root, LCA is in right subtree
    - Otherwise, root is the LCA (split point or one of p/q)
    """
    # Both nodes in left subtree
    if p.val < root.val and q.val < root.val:
        return lowest_common_ancestor_bst_recursive(root.left, p, q)

    # Both nodes in right subtree
    if p.val > root.val and q.val > root.val:
        return lowest_common_ancestor_bst_recursive(root.right, p, q)

    # Split point: one node on left, one on right (or root is one of them)
    return root


def lowest_common_ancestor_bst(
    root: TreeNode, p: TreeNode, q: TreeNode
) -> TreeNode:
    """
    Iterative approach for BST (more space efficient).

    Time Complexity: O(h)
    Space Complexity: O(1)
    """
    while root:
        # Both nodes in left subtree
        if p.val < root.val and q.val < root.val:
            root = root.left
        # Both nodes in right subtree
        elif p.val > root.val and q.val > root.val:
            root = root.right
        # Found split point
        else:
            return root

    return None


def lowest_common_ancestor_binary_tree(
    root: Optional[TreeNode], p: TreeNode, q: TreeNode
) -> Optional[TreeNode]:
    """
    Approach for general binary tree (not necessarily BST).

    Time Complexity: O(n) - may need to visit all nodes
    Space Complexity: O(h) for recursion stack

    Why it works:
    - If root is None or matches p or q, return root
    - Recursively search left and right subtrees
    - If both subtrees return non-null, root is LCA (split point)
    - If only one subtree returns non-null, that's the LCA
    """
    # Base case: empty tree or found one of the nodes
    if not root or root == p or root == q:
        return root

    # Search left and right subtrees
    left = lowest_common_ancestor_binary_tree(root.left, p, q)
    right = lowest_common_ancestor_binary_tree(root.right, p, q)

    # If both sides return non-null, root is the LCA
    if left and right:
        return root

    # Otherwise, return whichever side is non-null
    return left if left else right


def lowest_common_ancestor_with_path(
    root: TreeNode, p: TreeNode, q: TreeNode
) -> TreeNode:
    """
    Alternative approach: Find paths to both nodes, then find divergence point.

    Time Complexity: O(n)
    Space Complexity: O(h) for storing paths
    """
    def find_path(node: TreeNode, target: TreeNode, path: list) -> bool:
        """Find path from root to target node."""
        if not node:
            return False

        path.append(node)

        if node == target:
            return True

        # Search in subtrees
        if find_path(node.left, target, path) or find_path(node.right, target, path):
            return True

        path.pop()
        return False

    # Find paths to both nodes
    path_p = []
    path_q = []

    find_path(root, p, path_p)
    find_path(root, q, path_q)

    # Find last common node in both paths
    lca = None
    for node_p, node_q in zip(path_p, path_q):
        if node_p == node_q:
            lca = node_p
        else:
            break

    return lca


# Helper functions for testing
def list_to_bst(values: list) -> Optional[TreeNode]:
    """Convert sorted list to BST."""
    if not values:
        return None

    def build_bst(left: int, right: int) -> Optional[TreeNode]:
        if left > right:
            return None

        mid = (left + right) // 2
        node = TreeNode(values[mid])
        node.left = build_bst(left, mid - 1)
        node.right = build_bst(mid + 1, right)
        return node

    return build_bst(0, len(values) - 1)


def find_node(root: TreeNode, val: int) -> Optional[TreeNode]:
    """Find node with given value in BST."""
    if not root:
        return None

    if val == root.val:
        return root
    elif val < root.val:
        return find_node(root.left, val)
    else:
        return find_node(root.right, val)


# Test Cases
if __name__ == "__main__":
    print("=" * 60)
    print("LOWEST COMMON ANCESTOR - TEST RESULTS")
    print("=" * 60)

    # Build BST: [2, 1, 3] -> tree with 2 as root, 1 left, 3 right
    # Build BST: [1, 2, 3, 4, 5, 6, 7] (balanced)
    test_cases = [
        {
            'values': [1, 2, 3, 4, 5, 6, 7],
            'p_val': 1,
            'q_val': 3,
            'expected_val': 2,
            'description': 'LCA of 1 and 3 is 2'
        },
        {
            'values': [1, 2, 3, 4, 5, 6, 7],
            'p_val': 1,
            'q_val': 7,
            'expected_val': 4,
            'description': 'LCA of 1 and 7 is root (4)'
        },
        {
            'values': [1, 2, 3, 4, 5, 6, 7],
            'p_val': 2,
            'q_val': 3,
            'expected_val': 2,
            'description': 'LCA where one node is ancestor of other'
        },
        {
            'values': [1, 2, 3],
            'p_val': 1,
            'q_val': 3,
            'expected_val': 2,
            'description': 'Simple tree'
        }
    ]

    for i, test in enumerate(test_cases, 1):
        root = list_to_bst(test['values'])
        p = find_node(root, test['p_val'])
        q = find_node(root, test['q_val'])
        lca = lowest_common_ancestor_bst(root, p, q)

        passed = lca and lca.val == test['expected_val']

        print(f"\nTest {i}: {test['description']}")
        print(f"BST values: {test['values']}")
        print(f"p = {test['p_val']}, q = {test['q_val']}")
        print(f"Expected LCA: {test['expected_val']}")
        print(f"Got LCA: {lca.val if lca else None}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("BST Recursive: Time O(h), Space O(h)")
    print("BST Iterative (Optimal): Time O(h), Space O(1)")
    print("Binary Tree: Time O(n), Space O(h)")
    print("Path Finding: Time O(n), Space O(h)")
    print("\nKey Insight (BST): Use BST property to determine")
    print("whether to search left, right, or if current node is LCA")

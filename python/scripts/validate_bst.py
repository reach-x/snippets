"""
Validate Binary Search Tree

Given the root of a binary tree, determine if it is a valid binary search tree (BST).

A valid BST is defined as follows:
- The left subtree of a node contains only nodes with keys less than the node's key.
- The right subtree of a node contains only nodes with keys greater than the node's key.
- Both the left and right subtrees must also be binary search trees.

LeetCode: https://leetcode.com/problems/validate-binary-search-tree/
Difficulty: Medium
Pattern: Tree / DFS / Recursion
"""

from typing import Optional


class TreeNode:
    """Definition for a binary tree node."""
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right


def is_valid_bst_wrong(root: Optional[TreeNode]) -> bool:
    """
    WRONG APPROACH - Common mistake!

    Only checks if left < root < right at each node.
    Doesn't validate global BST property.

    Example failure: Tree [5, 1, 6, null, null, 4, 7]
    Node 4 is in right subtree of 5, but 4 < 5 (invalid!)
    """
    if not root:
        return True

    if root.left and root.left.val >= root.val:
        return False

    if root.right and root.right.val <= root.val:
        return False

    return is_valid_bst_wrong(root.left) and is_valid_bst_wrong(root.right)


def is_valid_bst_recursive(root: Optional[TreeNode]) -> bool:
    """
    Correct recursive approach with range validation.

    Time Complexity: O(n) - visit each node once
    Space Complexity: O(h) - recursion stack, h = height

    Why it works:
    - Each node must be within a valid range [min_val, max_val]
    - Left child range: [parent's min, parent's val)
    - Right child range: (parent's val, parent's max]
    - Root has range [-infinity, +infinity]
    """
    def validate(node: Optional[TreeNode], min_val: float, max_val: float) -> bool:
        # Empty tree is valid BST
        if not node:
            return True

        # Current node must be within range
        if node.val <= min_val or node.val >= max_val:
            return False

        # Validate left subtree (values must be < node.val)
        # Validate right subtree (values must be > node.val)
        return (validate(node.left, min_val, node.val) and
                validate(node.right, node.val, max_val))

    return validate(root, float('-inf'), float('inf'))


def is_valid_bst(root: Optional[TreeNode]) -> bool:
    """
    Inorder traversal approach (elegant solution).

    Time Complexity: O(n)
    Space Complexity: O(h)

    Why it works:
    - Inorder traversal of BST produces sorted sequence
    - If we see any value <= previous value, it's not a valid BST
    """
    def inorder(node: Optional[TreeNode]) -> bool:
        nonlocal prev

        if not node:
            return True

        # Check left subtree
        if not inorder(node.left):
            return False

        # Check current node
        if prev is not None and node.val <= prev:
            return False

        prev = node.val

        # Check right subtree
        return inorder(node.right)

    prev = None
    return inorder(root)


def is_valid_bst_iterative(root: Optional[TreeNode]) -> bool:
    """
    Iterative inorder traversal approach.

    Time Complexity: O(n)
    Space Complexity: O(h)
    """
    stack = []
    current = root
    prev_val = None

    while stack or current:
        # Go to leftmost node
        while current:
            stack.append(current)
            current = current.left

        # Process node
        current = stack.pop()

        # Check BST property
        if prev_val is not None and current.val <= prev_val:
            return False

        prev_val = current.val

        # Move to right subtree
        current = current.right

    return True


def is_valid_bst_with_path(root: Optional[TreeNode]) -> dict:
    """
    Validation with detailed path information for debugging.

    Returns dict with validation result and violation details if any.
    """
    def validate(node: Optional[TreeNode], min_val: float, max_val: float, path: list) -> dict:
        if not node:
            return {'valid': True, 'path': path}

        current_path = path + [node.val]

        # Check current node
        if node.val <= min_val or node.val >= max_val:
            return {
                'valid': False,
                'path': current_path,
                'violation': f"Node {node.val} violates range ({min_val}, {max_val})",
                'min_allowed': min_val,
                'max_allowed': max_val
            }

        # Check left subtree
        left_result = validate(node.left, min_val, node.val, current_path)
        if not left_result['valid']:
            return left_result

        # Check right subtree
        right_result = validate(node.right, node.val, max_val, current_path)
        if not right_result['valid']:
            return right_result

        return {'valid': True, 'path': current_path}

    return validate(root, float('-inf'), float('inf'), [])


# Helper functions for testing
def list_to_tree(values: list, index: int = 0) -> Optional[TreeNode]:
    """Convert list to binary tree (level-order)."""
    if index >= len(values) or values[index] is None:
        return None

    node = TreeNode(values[index])
    node.left = list_to_tree(values, 2 * index + 1)
    node.right = list_to_tree(values, 2 * index + 2)

    return node


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'tree': [2, 1, 3],
            'expected': True,
            'description': 'Valid BST: simple case'
        },
        {
            'tree': [5, 1, 4, None, None, 3, 6],
            'expected': False,
            'description': 'Invalid: 3 is in right subtree of 5 but 3 < 5'
        },
        {
            'tree': [1, 1],
            'expected': False,
            'description': 'Invalid: duplicate values (left = root)'
        },
        {
            'tree': [5, 4, 6, None, None, 3, 7],
            'expected': False,
            'description': 'Invalid: 3 in right subtree of 5'
        },
        {
            'tree': [10, 5, 15, None, None, 6, 20],
            'expected': False,
            'description': 'Invalid: 6 < 10 but in right subtree'
        },
        {
            'tree': [3, 1, 5, 0, 2, 4, 6],
            'expected': True,
            'description': 'Valid BST: complete tree'
        },
        {
            'tree': [1, None, 3, 2],
            'expected': True,
            'description': 'Valid BST: right-skewed'
        },
        {
            'tree': [],
            'expected': True,
            'description': 'Empty tree is valid BST'
        }
    ]

    print("=" * 60)
    print("VALIDATE BINARY SEARCH TREE - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        root = list_to_tree(test['tree']) if test['tree'] else None
        result = is_valid_bst(root)
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Tree (level-order): {test['tree']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")

        # Show detailed validation for failures
        if not passed or not result:
            details = is_valid_bst_with_path(root) if root else {'valid': True}
            if not details['valid']:
                print(f"Violation: {details['violation']}")
                print(f"Path to violation: {details['path']}")

        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("All approaches: Time O(n), Space O(h)")
    print("\nRange Validation: Track valid range for each node")
    print("Inorder Traversal: Check if values are in sorted order")
    print("\nCommon Mistake: Only checking immediate children")
    print("Must validate ALL descendants are in valid range!")

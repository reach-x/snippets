"""
BINARY TREE DFS (DEPTH-FIRST SEARCH) PATTERN - Comprehensive Guide

DFS explores a tree by going as deep as possible before backtracking. It's the
foundation for most tree problems and uses recursion or an explicit stack.

WHEN TO USE:
- Tree traversal (preorder, inorder, postorder)
- Path finding problems
- Tree validation
- Finding depths, heights, diameters
- Subtree problems
- Serialization/deserialization

TIME COMPLEXITY: O(n) - visit each node once
SPACE COMPLEXITY: O(h) where h is height
  - Balanced tree: O(log n)
  - Skewed tree: O(n)

THREE DFS TRAVERSALS:
1. Preorder: Root → Left → Right (process before children)
2. Inorder: Left → Root → Right (for BST, gives sorted order)
3. Postorder: Left → Right → Root (process after children)
"""

from typing import List, Optional


# ============================================================================
# TREE NODE DEFINITION
# ============================================================================

class TreeNode:
    """Simple binary tree node with value and left/right children."""
    def __init__(self, value=0, left_child=None, right_child=None):
        self.value = value
        self.left = left_child
        self.right = right_child


# ============================================================================
# PATTERN 1: TREE TRAVERSALS (RECURSIVE)
# ============================================================================

def preorder_recursive(root: Optional[TreeNode]) -> List[int]:
    """
    Preorder traversal: Root → Left → Right

    Process parent node BEFORE visiting children.

    Example:
        Tree:    1
                / \
               2   3
              / \
             4   5
        Preorder: [1, 2, 4, 5, 3]

    Use when:
    - Need to process parent before children
    - Copying/cloning a tree
    - Prefix expression evaluation
    - Serializing a tree

    Why it works:
    - Process current node first (root)
    - Then recursively process left subtree
    - Then recursively process right subtree

    Time Complexity: O(n) - visit each node once
    Space Complexity: O(h) - recursion stack depth

    Args:
        root: Root of binary tree

    Returns:
        List of node values in preorder sequence
    """
    result = []

    def depth_first_search(current_node):
        """Helper function to perform recursive DFS."""
        # Base case: null node
        if not current_node:
            return

        # Preorder: process root first
        result.append(current_node.value)

        # Then traverse left subtree
        depth_first_search(current_node.left)

        # Then traverse right subtree
        depth_first_search(current_node.right)

    depth_first_search(root)
    return result


def inorder_recursive(root: Optional[TreeNode]) -> List[int]:
    """
    Inorder traversal: Left → Root → Right

    Process left subtree, then root, then right subtree.

    Example:
        Tree:    1
                / \
               2   3
              / \
             4   5
        Inorder: [4, 2, 5, 1, 3]

    Use when:
    - Need sorted order from BST (Binary Search Tree)
    - Expression tree evaluation (infix notation)
    - Validating BST property

    Why it works:
    - First recursively process left subtree (smaller values in BST)
    - Then process current node
    - Then recursively process right subtree (larger values in BST)
    - For BST, this gives sorted order!

    Time Complexity: O(n)
    Space Complexity: O(h)

    Args:
        root: Root of binary tree

    Returns:
        List of node values in inorder sequence
    """
    result = []

    def depth_first_search(current_node):
        """Helper function to perform recursive DFS."""
        # Base case: null node
        if not current_node:
            return

        # Inorder: traverse left first
        depth_first_search(current_node.left)

        # Then process root
        result.append(current_node.value)

        # Then traverse right
        depth_first_search(current_node.right)

    depth_first_search(root)
    return result


def postorder_recursive(root: Optional[TreeNode]) -> List[int]:
    """
    Postorder traversal: Left → Right → Root

    Process children BEFORE processing parent.

    Example:
        Tree:    1
                / \
               2   3
              / \
             4   5
        Postorder: [4, 5, 2, 3, 1]

    Use when:
    - Need to process children before parent
    - Deleting/freeing a tree (free children first)
    - Calculating tree size/height
    - Postfix expression evaluation

    Why it works:
    - First recursively process left subtree
    - Then recursively process right subtree
    - Finally process current node (after children done)
    - Bottom-up calculation pattern

    Time Complexity: O(n)
    Space Complexity: O(h)

    Args:
        root: Root of binary tree

    Returns:
        List of node values in postorder sequence
    """
    result = []

    def depth_first_search(current_node):
        """Helper function to perform recursive DFS."""
        # Base case: null node
        if not current_node:
            return

        # Postorder: traverse left first
        depth_first_search(current_node.left)

        # Then traverse right
        depth_first_search(current_node.right)

        # Finally process root
        result.append(current_node.value)

    depth_first_search(root)
    return result


# ============================================================================
# PATTERN 2: TREE TRAVERSALS (ITERATIVE WITH STACK)
# ============================================================================

def preorder_iterative(root: Optional[TreeNode]) -> List[int]:
    """
    Preorder traversal using explicit stack (iterative approach).

    Example:
        Tree:    1
                / \
               2   3
        Stack operations:
        1. Pop 1, push 3, push 2
        2. Pop 2 (left first due to LIFO)
        3. Pop 3

    Strategy: Process root, push right child then left child to stack.
    - Right pushed first ensures left processed first (LIFO - Last In First Out)

    Why iterative approach:
    - Useful when recursion depth might cause stack overflow
    - Makes the call stack explicit
    - Same time/space complexity as recursive

    Time Complexity: O(n)
    Space Complexity: O(h) - stack can have at most h nodes

    Args:
        root: Root of binary tree

    Returns:
        List of node values in preorder sequence
    """
    # Handle empty tree
    if not root:
        return []

    result = []
    node_stack = [root]

    while node_stack:
        # Pop and process current node
        current_node = node_stack.pop()
        result.append(current_node.value)

        # Push right child first (will be processed second due to LIFO)
        if current_node.right:
            node_stack.append(current_node.right)

        # Push left child second (will be processed first due to LIFO)
        if current_node.left:
            node_stack.append(current_node.left)

    return result


def inorder_iterative(root: Optional[TreeNode]) -> List[int]:
    """
    Inorder traversal using explicit stack (iterative approach).

    Example:
        Tree:    1
                / \
               2   3
        Process: Go left to 2, process 2, go right (none), backtrack to 1,
                 process 1, go right to 3, process 3

    Strategy: Go left as far as possible, process node, then go right.
    - Keep going left and pushing to stack
    - When can't go left, pop and process
    - Then try going right

    Why this pattern:
    - Mimics the recursive call stack explicitly
    - Left subtree fully processed before root
    - Root processed before right subtree

    Time Complexity: O(n)
    Space Complexity: O(h)

    Args:
        root: Root of binary tree

    Returns:
        List of node values in inorder sequence
    """
    result = []
    node_stack = []
    current_node = root

    # Continue while there are nodes to process
    while current_node or node_stack:
        # Go left as far as possible, pushing nodes to stack
        while current_node:
            node_stack.append(current_node)
            current_node = current_node.left

        # Process node (current_node is now None, pop from stack)
        current_node = node_stack.pop()
        result.append(current_node.value)

        # Move to right subtree
        current_node = current_node.right

    return result


def postorder_iterative(root: Optional[TreeNode]) -> List[int]:
    """
    Postorder traversal using two stacks (iterative approach).

    Example:
        Tree:    1
                / \
               2   3
        Stack1 processes: Root → Right → Left (modified preorder)
        Stack2 reverses to get: Left → Right → Root (postorder)

    Strategy: Modified preorder (Root → Right → Left) reversed
    - First stack does Root → Right → Left
    - Second stack reverses it to Left → Right → Root
    - Clever trick to achieve postorder iteratively

    Why two stacks:
    - Postorder is tricky to do with one stack
    - Easier to do reverse of (Root → Right → Left)
    - Which gives us (Left → Right → Root) = postorder!

    Time Complexity: O(n)
    Space Complexity: O(n) - both stacks can have O(n) nodes in worst case

    Args:
        root: Root of binary tree

    Returns:
        List of node values in postorder sequence
    """
    # Handle empty tree
    if not root:
        return []

    # First stack for modified preorder traversal
    stack_one = [root]
    # Second stack to reverse the result
    stack_two = []

    # Modified preorder: Root → Right → Left
    while stack_one:
        current_node = stack_one.pop()
        stack_two.append(current_node)

        # Push left first (opposite of normal preorder)
        if current_node.left:
            stack_one.append(current_node.left)

        # Push right second (opposite of normal preorder)
        if current_node.right:
            stack_one.append(current_node.right)

    # Reverse stack_two to get postorder
    return [node.value for node in reversed(stack_two)]


# ============================================================================
# PATTERN 3: DEPTH AND HEIGHT
# ============================================================================

def max_depth(root: Optional[TreeNode]) -> int:
    """
    Find maximum depth (height) of binary tree.

    Example:
        Tree:    1        Depth:    1
                / \                / \
               2   3              2   2
              / \                / \
             4   5              3   3
        Maximum depth = 3 (root to leaf 4 or 5)

    Depth = longest path from root to any leaf node.

    Why it works:
    - Recursively find max depth of left and right subtrees
    - Current node's depth = 1 + max(left_depth, right_depth)
    - Leaf node returns 1, null returns 0

    Time Complexity: O(n) - visit every node
    Space Complexity: O(h) - recursion stack

    Args:
        root: Root of binary tree

    Returns:
        Maximum depth of tree (0 for empty tree)
    """
    # Base case: empty tree has depth 0
    if not root:
        return 0

    # Recursively find depth of left subtree
    left_depth = max_depth(root.left)

    # Recursively find depth of right subtree
    right_depth = max_depth(root.right)

    # Current depth = 1 (current node) + max of subtrees
    return 1 + max(left_depth, right_depth)


def min_depth(root: Optional[TreeNode]) -> int:
    """
    Find minimum depth to nearest leaf node.

    Example:
        Tree:    1
                / \
               2   3
              /
             4
        Minimum depth = 2 (root → 3)

    Important: A leaf has BOTH children as None.
    - Can't count path to node with only one child as complete

    Why it works:
    - If one child is None, must take the path through other child
    - If both children exist, take minimum of both paths
    - Can't short-circuit through null child

    Time Complexity: O(n) - might visit all nodes
    Space Complexity: O(h)

    Args:
        root: Root of binary tree

    Returns:
        Minimum depth to nearest leaf
    """
    # Base case: empty tree
    if not root:
        return 0

    # If left child is None, can't count this as complete path
    # Must go through right child
    if not root.left:
        return 1 + min_depth(root.right)

    # If right child is None, must go through left child
    if not root.right:
        return 1 + min_depth(root.left)

    # Both children exist: take minimum path
    left_min_depth = min_depth(root.left)
    right_min_depth = min_depth(root.right)

    return 1 + min(left_min_depth, right_min_depth)


# ============================================================================
# PATTERN 4: PATH SUM PROBLEMS
# ============================================================================

def has_path_sum(root: Optional[TreeNode], target_sum: int) -> bool:
    """
    Check if any root-to-leaf path exists with given sum.

    Example:
        Tree:    5
                / \
               4   8
              /   / \
             11  13  4
            /  \      \
           7    2      1

        has_path_sum(root, 22) → True (5→4→11→2 = 22)

    Strategy:
    - At each node, subtract node value from remaining sum
    - At leaf, check if remaining sum equals leaf value
    - Return true if ANY path matches

    Why it works:
    - Reduce problem at each step (subtract current value)
    - Base case: at leaf, check if sum matches
    - Recursively check left OR right path

    Time Complexity: O(n) - might visit all nodes
    Space Complexity: O(h)

    Args:
        root: Root of binary tree
        target_sum: Target sum to find

    Returns:
        True if any root-to-leaf path sums to target
    """
    # Base case: empty tree has no paths
    if not root:
        return False

    # Leaf node: check if this completes the path with correct sum
    if not root.left and not root.right:
        return target_sum == root.value

    # Recurse with reduced sum (subtract current node value)
    remaining_sum = target_sum - root.value

    # Check if either left OR right path has the remaining sum
    left_has_path = has_path_sum(root.left, remaining_sum)
    right_has_path = has_path_sum(root.right, remaining_sum)

    return left_has_path or right_has_path


def path_sum_all_paths(root: Optional[TreeNode], target_sum: int) -> List[List[int]]:
    """
    Find ALL root-to-leaf paths that sum to target.

    Example:
        Tree:    5
                / \
               4   8
              /   / \
             11  13  4
            /  \    / \
           7    2  5   1

        path_sum_all_paths(root, 22) → [[5,4,11,2], [5,8,4,5]]

    Strategy: DFS with backtracking
    - Build path as we go down
    - At leaf, check sum and save path if matches
    - Backtrack by removing current node before returning

    Why backtracking:
    - Need to explore all paths
    - Path list is reused, must remove node after exploring
    - Copy path to result (path[:]) because it will be modified

    Time Complexity: O(n²) worst case (need to copy paths)
    Space Complexity: O(h) for recursion + path storage

    Args:
        root: Root of binary tree
        target_sum: Target sum to find

    Returns:
        List of all paths that sum to target
    """
    result = []

    def depth_first_search(current_node, current_sum, current_path):
        """DFS helper with backtracking."""
        # Base case: null node
        if not current_node:
            return

        # Add current node to path
        current_path.append(current_node.value)
        current_sum += current_node.value

        # Leaf node: check if sum matches target
        if not current_node.left and not current_node.right:
            if current_sum == target_sum:
                # Found a valid path - must copy it!
                result.append(current_path[:])
        else:
            # Not a leaf: continue exploring children
            depth_first_search(current_node.left, current_sum, current_path)
            depth_first_search(current_node.right, current_sum, current_path)

        # Backtrack: remove current node from path
        current_path.pop()

    depth_first_search(root, 0, [])
    return result


def path_sum_any_path(root: Optional[TreeNode], target_sum: int) -> int:
    """
    Count paths that sum to target (path doesn't need to start at root or end at leaf).

    Example:
        Tree:    10
                /  \
               5   -3
              / \    \
             3   2   11
            / \   \
           3  -2   1

        path_sum_any_path(root, 8) → 3 paths:
        - 5 → 3
        - 5 → 2 → 1
        - -3 → 11

    Strategy: For each node, count paths ending at that node
    - Recursively check if current sum equals target
    - Also start new paths from each node

    Why this works:
    - Each node can be the end of a path
    - Each node can also be the start of a new path
    - Count both possibilities

    Time Complexity: O(n²) worst case (skewed tree)
    Space Complexity: O(h)

    Args:
        root: Root of binary tree
        target_sum: Target sum to find

    Returns:
        Count of all paths summing to target
    """
    def count_paths_ending_here(current_node, current_sum):
        """Count paths that end at current_node with given running sum."""
        if not current_node:
            return 0

        # Add current node's value to running sum
        current_sum += current_node.value

        # Check if current path sum equals target
        path_count = 1 if current_sum == target_sum else 0

        # Add paths from children (extending current path)
        path_count += count_paths_ending_here(current_node.left, current_sum)
        path_count += count_paths_ending_here(current_node.right, current_sum)

        return path_count

    # Base case: empty tree
    if not root:
        return 0

    # Count paths:
    # 1. Paths starting from current root
    paths_from_root = count_paths_ending_here(root, 0)

    # 2. Paths starting from left subtree (not including root)
    paths_from_left = path_sum_any_path(root.left, target_sum)

    # 3. Paths starting from right subtree (not including root)
    paths_from_right = path_sum_any_path(root.right, target_sum)

    return paths_from_root + paths_from_left + paths_from_right


# ============================================================================
# PATTERN 5: TREE DIAMETER AND PATHS
# ============================================================================

def diameter_of_tree(root: Optional[TreeNode]) -> int:
    """
    Find diameter (longest path between any two nodes in the tree).

    Example:
        Tree:    1
                / \
               2   3
              / \
             4   5
        Diameter = 3 (path: 4 → 2 → 5, or 4 → 2 → 1 → 3)

    Path length = number of edges (not nodes).
    Diameter at any node = left_height + right_height

    Why it works:
    - Longest path might not go through root
    - At each node, calculate path through that node
    - Path through node = left_height + right_height
    - Keep track of maximum seen so far

    Time Complexity: O(n) - visit each node once
    Space Complexity: O(h)

    Args:
        root: Root of binary tree

    Returns:
        Diameter (longest path between any two nodes)
    """
    # Use list to allow modification in nested function
    max_diameter = [0]

    def calculate_height(current_node):
        """Calculate height and update diameter."""
        # Base case: null node has height 0
        if not current_node:
            return 0

        # Get height of left and right subtrees
        left_height = calculate_height(current_node.left)
        right_height = calculate_height(current_node.right)

        # Update diameter: path through current node
        # = edges in left subtree + edges in right subtree
        current_diameter = left_height + right_height
        max_diameter[0] = max(max_diameter[0], current_diameter)

        # Return height of current node
        return 1 + max(left_height, right_height)

    calculate_height(root)
    return max_diameter[0]


def max_path_sum(root: Optional[TreeNode]) -> int:
    """
    Find maximum path sum (path between any two nodes).

    Example:
        Tree:   -10
                /  \
               9   20
                  /  \
                 15   7
        Maximum path sum = 42 (15 → 20 → 7)

    Path can:
    - Start and end at any nodes
    - Include negative values
    - Not necessarily go through root

    Why it works:
    - At each node, calculate max sum path through that node
    - Path through node = node.value + left_sum + right_sum
    - Can ignore negative paths (take max with 0)
    - Update global maximum

    Time Complexity: O(n)
    Space Complexity: O(h)

    Args:
        root: Root of binary tree

    Returns:
        Maximum path sum
    """
    # Use list to allow modification in nested function
    maximum_sum = [float('-inf')]

    def calculate_max_path(current_node):
        """Calculate max path sum and update global maximum."""
        # Base case: null node contributes 0
        if not current_node:
            return 0

        # Get max sum from left and right subtrees
        # Use max with 0 to ignore negative paths
        left_sum = max(0, calculate_max_path(current_node.left))
        right_sum = max(0, calculate_max_path(current_node.right))

        # Path sum through current node (can use both children)
        path_through_current = current_node.value + left_sum + right_sum

        # Update global maximum
        maximum_sum[0] = max(maximum_sum[0], path_through_current)

        # Return max sum of single path (can only go one direction)
        # Parent can only use current node + one of the child paths
        return current_node.value + max(left_sum, right_sum)

    calculate_max_path(root)
    return maximum_sum[0]


# ============================================================================
# PATTERN 6: TREE VALIDATION
# ============================================================================

def is_valid_bst(root: Optional[TreeNode]) -> bool:
    """
    Validate if tree is a valid binary search tree.

    Example:
        Valid BST:    2         Invalid BST:   5
                     / \                      / \
                    1   3                    1   4
                                                / \
                                               3   6
        (1 < 2 < 3, valid)      (3 < 5 but 3 not < 4, invalid!)

    BST property: For ALL nodes:
    - All left descendants < node.value
    - All right descendants > node.value

    Why passing bounds works:
    - Each node has valid range from ancestors
    - Left child: upper bound becomes parent value
    - Right child: lower bound becomes parent value
    - Check if current value within bounds

    Time Complexity: O(n) - check all nodes
    Space Complexity: O(h)

    Args:
        root: Root of binary tree

    Returns:
        True if valid BST, False otherwise
    """
    def validate(current_node, minimum_value, maximum_value):
        """Validate node is within bounds and children are valid."""
        # Base case: null node is valid
        if not current_node:
            return True

        # Check if current value is within valid range
        # Note: values must be strictly between bounds (not equal)
        if not (minimum_value < current_node.value < maximum_value):
            return False

        # Validate left subtree: all values must be < current value
        left_valid = validate(current_node.left, minimum_value, current_node.value)

        # Validate right subtree: all values must be > current value
        right_valid = validate(current_node.right, current_node.value, maximum_value)

        return left_valid and right_valid

    # Start with infinite bounds
    return validate(root, float('-inf'), float('inf'))


def is_balanced(root: Optional[TreeNode]) -> bool:
    """
    Check if tree is height-balanced.

    Example:
        Balanced:     1         Unbalanced:    1
                     / \                         \
                    2   3                         2
                   /                               \
                  4                                 3

    Height-balanced: For ALL nodes, height difference of left and right
    subtrees is at most 1.

    Why returning -1 works:
    - Use -1 to signal unbalanced subtree
    - Propagates unbalanced state up
    - Otherwise return actual height
    - Check balance while calculating height (single pass!)

    Time Complexity: O(n)
    Space Complexity: O(h)

    Args:
        root: Root of binary tree

    Returns:
        True if balanced, False otherwise
    """
    def check_height(current_node):
        """
        Check if subtree is balanced and return height.
        Returns -1 if unbalanced, otherwise returns height.
        """
        # Base case: null node has height 0
        if not current_node:
            return 0

        # Check left subtree height
        left_height = check_height(current_node.left)
        if left_height == -1:  # Left subtree unbalanced
            return -1

        # Check right subtree height
        right_height = check_height(current_node.right)
        if right_height == -1:  # Right subtree unbalanced
            return -1

        # Check balance at current node
        if abs(left_height - right_height) > 1:
            return -1  # Current node makes tree unbalanced

        # Tree is balanced at this node, return actual height
        return 1 + max(left_height, right_height)

    # Tree is balanced if check_height doesn't return -1
    return check_height(root) != -1


def is_same_tree(first_root: Optional[TreeNode], second_root: Optional[TreeNode]) -> bool:
    """
    Check if two trees are structurally identical with same values.

    Example:
        Tree 1:  1         Tree 2:  1         Same: True
                / \                / \
               2   3              2   3

        Tree 1:  1         Tree 2:  1         Same: False
                /                    \
               2                      2

    Why it works:
    - Both null: trees are same (base case)
    - One null, other not: trees different
    - Both exist: check value and recurse on children
    - All three conditions must match

    Time Complexity: O(min(n1, n2)) - stop at first difference
    Space Complexity: O(min(h1, h2))

    Args:
        first_root: Root of first tree
        second_root: Root of second tree

    Returns:
        True if trees are identical, False otherwise
    """
    # Both trees empty: they match
    if not first_root and not second_root:
        return True

    # One tree empty, other not: they don't match
    if not first_root or not second_root:
        return False

    # Both trees exist: check value and children
    values_match = (first_root.value == second_root.value)
    left_subtrees_match = is_same_tree(first_root.left, second_root.left)
    right_subtrees_match = is_same_tree(first_root.right, second_root.right)

    return values_match and left_subtrees_match and right_subtrees_match


# ============================================================================
# PATTERN 7: LOWEST COMMON ANCESTOR
# ============================================================================

def lowest_common_ancestor(root: TreeNode, first_node: TreeNode,
                          second_node: TreeNode) -> TreeNode:
    """
    Find lowest common ancestor (LCA) of two nodes.

    Example:
        Tree:    3
                / \
               5   1
              / \
             6   2

        LCA(6, 2) = 5
        LCA(5, 1) = 3

    LCA is the deepest node that has both nodes as descendants.
    A node can be a descendant of itself.

    Why it works:
    - If current node is one of the targets, return it
    - Search both subtrees
    - If both subtrees return non-null, current node is LCA
    - Otherwise, LCA is in the subtree that returned non-null

    Time Complexity: O(n) - might visit all nodes
    Space Complexity: O(h)

    Args:
        root: Root of binary tree
        first_node: First target node
        second_node: Second target node

    Returns:
        Lowest common ancestor node
    """
    # Base case: null node or found one of target nodes
    if not root or root == first_node or root == second_node:
        return root

    # Search in left and right subtrees
    left_result = lowest_common_ancestor(root.left, first_node, second_node)
    right_result = lowest_common_ancestor(root.right, first_node, second_node)

    # If both subtrees returned a node, current node is the LCA
    # (first_node in one subtree, second_node in other subtree)
    if left_result and right_result:
        return root

    # Otherwise, return the non-null result
    # (both nodes are in the same subtree)
    return left_result if left_result else right_result


# ============================================================================
# PATTERN 8: TREE CONSTRUCTION
# ============================================================================

def build_tree_preorder_inorder(preorder: List[int], inorder: List[int]) -> Optional[TreeNode]:
    """
    Construct binary tree from preorder and inorder traversals.

    Example:
        Preorder: [3, 9, 20, 15, 7]
        Inorder:  [9, 3, 15, 20, 7]

        Tree:    3
                / \
               9  20
                  / \
                 15  7

    Why it works:
    - Preorder: first element is always root
    - Inorder: elements before root are left subtree, after are right
    - Recursively build left and right subtrees
    - Need both traversals to uniquely determine tree

    Time Complexity: O(n²) due to index search (can optimize to O(n) with hashmap)
    Space Complexity: O(n)

    Args:
        preorder: Preorder traversal list
        inorder: Inorder traversal list

    Returns:
        Root of constructed tree
    """
    # Base case: empty lists
    if not preorder or not inorder:
        return None

    # First element in preorder is the root
    root_value = preorder[0]
    root = TreeNode(root_value)

    # Find root position in inorder traversal
    # This divides inorder into left and right subtrees
    middle_index = inorder.index(root_value)

    # Recursively build left subtree
    # Preorder: skip root, take next 'middle_index' elements
    # Inorder: take all elements before root
    root.left = build_tree_preorder_inorder(
        preorder[1:middle_index+1],
        inorder[:middle_index]
    )

    # Recursively build right subtree
    # Preorder: skip root and left subtree elements
    # Inorder: take all elements after root
    root.right = build_tree_preorder_inorder(
        preorder[middle_index+1:],
        inorder[middle_index+1:]
    )

    return root


# ============================================================================
# DFS TEMPLATE - UNIVERSAL PATTERN
# ============================================================================

def dfs_template_explanation():
    """
    UNIVERSAL DFS TEMPLATE - Master Pattern for Tree Problems

    # =================================================================
    # RECURSIVE APPROACH (Preferred for readability)
    # =================================================================

    def dfs(current_node, parameters):
        # 1. BASE CASE
        if not current_node:
            return base_value

        # 2. PREORDER: Process current node BEFORE children
        # Use for: top-down propagation, copying tree, serialization
        # ...

        # 3. RECURSE: Get results from children
        left_result = dfs(current_node.left, updated_parameters)
        right_result = dfs(current_node.right, updated_parameters)

        # 4. POSTORDER: Combine results AFTER children processed
        # Use for: bottom-up calculation, tree height, validation
        final_result = combine(left_result, right_result, current_node.value)

        return final_result


    # =================================================================
    # ITERATIVE APPROACH (Use when recursion depth is concern)
    # =================================================================

    def dfs_iterative(root):
        if not root:
            return

        node_stack = [root]

        while node_stack:
            current_node = node_stack.pop()

            # Process current node
            # ...

            # Add children to stack
            # Push right first so left is processed first (LIFO)
            if current_node.right:
                node_stack.append(current_node.right)
            if current_node.left:
                node_stack.append(current_node.left)


    # =================================================================
    # KEY PATTERNS - When to use what
    # =================================================================

    1. RETURN value from children → use POSTORDER
       Example: max_depth, diameter, path_sum

    2. PASS value to children → use PREORDER
       Example: path tracking, range validation

    3. BST in sorted order → use INORDER
       Example: validate BST, kth smallest in BST

    4. Bottom-up calculation → POSTORDER
       Example: tree height, subtree properties

    5. Top-down propagation → PREORDER
       Example: path construction, depth tracking

    6. Need parent context in children → pass as parameter (PREORDER)
       Example: BST bounds, path sum, depth level

    7. Need child results for parent → return from children (POSTORDER)
       Example: is balanced, diameter, max path sum


    # =================================================================
    # COMMON PATTERNS
    # =================================================================

    # Pattern 1: Max/Min value in tree
    def find_max(node):
        if not node:
            return float('-inf')
        return max(node.value, find_max(node.left), find_max(node.right))

    # Pattern 2: Check property in entire tree
    def all_positive(node):
        if not node:
            return True
        return node.value > 0 and all_positive(node.left) and all_positive(node.right)

    # Pattern 3: Path tracking with backtracking
    def find_paths(node, path):
        if not node:
            return
        path.append(node.value)  # Add
        # ... process ...
        find_paths(node.left, path)
        find_paths(node.right, path)
        path.pop()  # Backtrack

    # Pattern 4: Global state update
    def update_max_diameter(node):
        global max_diameter
        if not node:
            return 0
        left_h = update_max_diameter(node.left)
        right_h = update_max_diameter(node.right)
        max_diameter = max(max_diameter, left_h + right_h)
        return 1 + max(left_h, right_h)
    """
    pass


# ============================================================================
# TEST CASES
# ============================================================================

def create_sample_tree():
    """
    Create sample tree for testing:
           1
          / \
         2   3
        / \
       4   5
    """
    root = TreeNode(1)
    root.left = TreeNode(2)
    root.right = TreeNode(3)
    root.left.left = TreeNode(4)
    root.left.right = TreeNode(5)
    return root


def run_tests():
    """
    Comprehensive test cases demonstrating all DFS patterns.
    """
    print("=" * 70)
    print("BINARY TREE DFS PATTERN - COMPREHENSIVE TEST CASES")
    print("=" * 70)

    sample_tree = create_sample_tree()

    # Test 1: Traversals - Recursive
    print("\n1. TREE TRAVERSALS (RECURSIVE)")
    print("-" * 70)
    preorder_result = preorder_recursive(sample_tree)
    inorder_result = inorder_recursive(sample_tree)
    postorder_result = postorder_recursive(sample_tree)
    print(f"   Preorder:  {preorder_result}")
    print(f"   Inorder:   {inorder_result}")
    print(f"   Postorder: {postorder_result}")
    print(f"   Expected: [1,2,4,5,3], [4,2,5,1,3], [4,5,2,3,1]")
    print(f"\n   Why: Preorder processes root first, inorder gives sorted order for BST,")
    print(f"   postorder processes children before parent")

    # Test 2: Traversals - Iterative
    print("\n2. TREE TRAVERSALS (ITERATIVE)")
    print("-" * 70)
    preorder_iter = preorder_iterative(sample_tree)
    inorder_iter = inorder_iterative(sample_tree)
    postorder_iter = postorder_iterative(sample_tree)
    print(f"   Preorder:  {preorder_iter}")
    print(f"   Inorder:   {inorder_iter}")
    print(f"   Postorder: {postorder_iter}")
    print(f"\n   Why: Iterative uses explicit stack, same results as recursive")

    # Test 3: Depth
    print("\n3. TREE DEPTH")
    print("-" * 70)
    maximum_depth = max_depth(sample_tree)
    minimum_depth = min_depth(sample_tree)
    print(f"   Max depth: {maximum_depth}")
    print(f"   Min depth: {minimum_depth}")
    print(f"   Expected: 3 (root to 4/5), 2 (root to 3)")
    print(f"\n   Why: Max depth is longest path, min depth is shortest path to leaf")

    # Test 4: Diameter
    print("\n4. TREE DIAMETER")
    print("-" * 70)
    tree_diameter = diameter_of_tree(sample_tree)
    print(f"   Diameter: {tree_diameter}")
    print(f"   Expected: 3 (path: 4→2→5 or 4→2→1→3)")
    print(f"\n   Why: Diameter is longest path between any two nodes")
    print(f"   Path through node 2: left_height(1) + right_height(1) = 2")
    print(f"   Path through node 1: left_height(2) + right_height(1) = 3 ✓")

    # Test 5: BST Validation
    print("\n5. BST VALIDATION")
    print("-" * 70)
    valid_bst = TreeNode(2)
    valid_bst.left = TreeNode(1)
    valid_bst.right = TreeNode(3)
    is_valid = is_valid_bst(valid_bst)
    print(f"   Valid BST: {is_valid}")
    print(f"   Expected: True (1 < 2 < 3)")
    print(f"\n   Why: All left values < root < all right values")

    # Test 6: Same Tree
    print("\n6. SAME TREE CHECK")
    print("-" * 70)
    tree_copy = create_sample_tree()
    trees_same = is_same_tree(sample_tree, tree_copy)
    print(f"   Same tree: {trees_same}")
    print(f"   Expected: True")
    print(f"\n   Why: Both trees have identical structure and values")

    # Test 7: Balanced Tree
    print("\n7. BALANCED TREE")
    print("-" * 70)
    is_tree_balanced = is_balanced(sample_tree)
    print(f"   Is balanced: {is_tree_balanced}")
    print(f"   Expected: True")
    print(f"\n   Why: Height difference between left and right subtrees ≤ 1 at all nodes")
    print(f"   Node 1: left_height=2, right_height=1, diff=1 ✓")
    print(f"   Node 2: left_height=1, right_height=1, diff=0 ✓")

    # Summary
    print("\n" + "=" * 70)
    print("KEY INSIGHTS - BINARY TREE DFS PATTERNS")
    print("=" * 70)
    print("1. Preorder (Root→Left→Right): Process parent before children")
    print("2. Inorder (Left→Root→Right): BST gives sorted order")
    print("3. Postorder (Left→Right→Root): Process children before parent")
    print("4. Recursive DFS: Clean and intuitive, O(h) space for call stack")
    print("5. Iterative DFS: Uses explicit stack, same time/space complexity")
    print("6. Path problems: Use backtracking, copy path when adding to result")
    print("7. Validation: Pass bounds/constraints down the tree (preorder)")
    print("8. Diameter/Max Path: Update global variable during traversal (postorder)")
    print("9. Height calculations: Return value from children (postorder)")
    print("10. Balance check: Can combine with height calculation in single pass")
    print("=" * 70)


if __name__ == "__main__":
    run_tests()

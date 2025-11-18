"""
BINARY TREE BFS (BREADTH-FIRST SEARCH) PATTERN - Comprehensive Guide

BFS explores a tree level by level using a queue. It processes all nodes at the
current level before moving to the next level. Essential for level-order problems.

WHEN TO USE:
- Level-order traversal
- Finding minimum depth/shortest path
- Level-wise processing (averages, max per level)
- Zigzag traversal
- Right/left side view
- Connect nodes at same level

TIME COMPLEXITY: O(n) - visit each node once
SPACE COMPLEXITY: O(w) where w is max width
  - Perfect binary tree: O(n/2) = O(n)
  - Skewed tree: O(1)

KEY DIFFERENCE FROM DFS:
- DFS: Goes deep first (stack/recursion), good for paths
- BFS: Goes wide first (queue), good for levels

CORE CONCEPT: Use queue to process nodes level by level
"""

from typing import List, Optional
from collections import deque


# ============================================================================
# TREE NODE DEFINITIONS
# ============================================================================

class TreeNode:
    """Standard binary tree node."""
    def __init__(self, value=0, left_child=None, right_child=None):
        self.value = value
        self.left = left_child
        self.right = right_child


class Node:
    """Node with next pointer for level connection problems."""
    def __init__(self, value=0, left_child=None, right_child=None, next_node=None):
        self.value = value
        self.left = left_child
        self.right = right_child
        self.next = next_node


# ============================================================================
# QUICK REFERENCE - COPY-PASTE TEMPLATES
# ============================================================================
"""
Use these minimal templates during interviews. Copy and adapt as needed.
CRITICAL: Use deque() not list for queue! deque.popleft() is O(1), list.pop(0) is O(n)
"""

# TEMPLATE 1: Level-Order Traversal (Basic BFS)
def level_order_template(root: Optional[TreeNode]) -> List[List[int]]:
    """Process tree level by level."""
    if not root:
        return []

    result = []
    queue = deque([root])

    while queue:
        level_size = len(queue)  # CRITICAL: Capture size before processing
        level_values = []

        for _ in range(level_size):
            node = queue.popleft()
            level_values.append(node.value)

            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)

        result.append(level_values)

    return result


# TEMPLATE 2: Zigzag Level-Order (Alternate Directions)
def zigzag_template(root: Optional[TreeNode]) -> List[List[int]]:
    """Alternate left-to-right and right-to-left by level."""
    if not root:
        return []

    result = []
    queue = deque([root])
    left_to_right = True

    while queue:
        level_size = len(queue)
        level_values = []

        for _ in range(level_size):
            node = queue.popleft()
            level_values.append(node.value)

            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)

        if not left_to_right:
            level_values.reverse()

        result.append(level_values)
        left_to_right = not left_to_right

    return result


# TEMPLATE 3: Right Side View
def right_side_view_template(root: Optional[TreeNode]) -> List[int]:
    """Last node at each level (rightmost visible)."""
    if not root:
        return []

    result = []
    queue = deque([root])

    while queue:
        level_size = len(queue)

        for i in range(level_size):
            node = queue.popleft()

            if i == level_size - 1:  # Last node in level
                result.append(node.value)

            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)

    return result


# TEMPLATE 4: Minimum Depth (Shortest Path)
def min_depth_template(root: Optional[TreeNode]) -> int:
    """BFS finds shortest path to leaf."""
    if not root:
        return 0

    queue = deque([root])
    depth = 1

    while queue:
        level_size = len(queue)

        for _ in range(level_size):
            node = queue.popleft()

            if not node.left and not node.right:  # Leaf node
                return depth

            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)

        depth += 1

    return depth


# TEMPLATE 5: Average of Levels
def average_of_levels_template(root: Optional[TreeNode]) -> List[float]:
    """Calculate average value at each level."""
    if not root:
        return []

    result = []
    queue = deque([root])

    while queue:
        level_size = len(queue)
        level_sum = 0

        for _ in range(level_size):
            node = queue.popleft()
            level_sum += node.value

            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)

        result.append(level_sum / level_size)

    return result


# ============================================================================
# PATTERN 1: BASIC LEVEL-ORDER TRAVERSAL
# ============================================================================

def level_order_traversal(root: Optional[TreeNode]) -> List[List[int]]:
    """
    Level-order traversal: return values grouped by level.

    Example:
        Tree:     3
                 / \
                9  20
                  /  \
                 15   7
        Output: [[3], [9, 20], [15, 7]]

    BFS Template Steps:
    1. Use queue (FIFO), start with root
    2. For each level: get level size from queue length
    3. Process exactly that many nodes (current level)
    4. Add their children to queue (next level)
    5. Repeat until queue empty

    Why it works:
    - Queue maintains nodes in level order
    - Taking snapshot of queue length gives current level size
    - Processing exactly level_size nodes ensures we process one level at a time
    - Children added to queue become next level

    Time Complexity: O(n) - visit each node once
    Space Complexity: O(w) - queue holds at most one level (width w)

    Args:
        root: Root of binary tree

    Returns:
        List of lists, each inner list contains values at that level
    """
    # Handle empty tree
    if not root:
        return []

    result = []
    node_queue = deque([root])

    # Process tree level by level
    while node_queue:
        # Snapshot queue length = number of nodes at current level
        level_size = len(node_queue)
        level_values = []

        # Process all nodes at current level
        for _ in range(level_size):
            current_node = node_queue.popleft()
            level_values.append(current_node.value)

            # Add children to queue for next level
            if current_node.left:
                node_queue.append(current_node.left)
            if current_node.right:
                node_queue.append(current_node.right)

        result.append(level_values)

    return result


def level_order_bottom_up(root: Optional[TreeNode]) -> List[List[int]]:
    """
    Level-order traversal from bottom to top (leaf level first).

    Example:
        Tree:     3
                 / \
                9  20
                  /  \
                 15   7
        Output: [[15, 7], [9, 20], [3]]

    Strategy: Same as regular level-order, but reverse the result.

    Why it works:
    - Regular BFS gives top-to-bottom levels
    - Simply reverse the list of levels
    - Same time/space complexity

    Time Complexity: O(n)
    Space Complexity: O(w)

    Args:
        root: Root of binary tree

    Returns:
        List of levels from bottom to top
    """
    # Handle empty tree
    if not root:
        return []

    result = []
    node_queue = deque([root])

    while node_queue:
        level_size = len(node_queue)
        level_values = []

        for _ in range(level_size):
            current_node = node_queue.popleft()
            level_values.append(current_node.value)

            if current_node.left:
                node_queue.append(current_node.left)
            if current_node.right:
                node_queue.append(current_node.right)

        result.append(level_values)

    # Reverse to get bottom-up order
    return result[::-1]


# ============================================================================
# PATTERN 2: ZIGZAG LEVEL-ORDER TRAVERSAL
# ============================================================================

def zigzag_level_order(root: Optional[TreeNode]) -> List[List[int]]:
    """
    Zigzag traversal: alternate left-to-right and right-to-left by level.

    Example:
        Tree:     3
                 / \
                9  20
                  /  \
                 15   7
        Level 0 (→): [3]
        Level 1 (←): [20, 9]    # reversed!
        Level 2 (→): [15, 7]
        Output: [[3], [20, 9], [15, 7]]

    Strategy:
    - Regular BFS traversal
    - Track direction with boolean flag
    - Reverse level if going right-to-left
    - Toggle direction after each level

    Why it works:
    - BFS naturally processes left-to-right
    - For right-to-left levels, simply reverse the collected values
    - Toggle flag maintains alternating pattern

    Time Complexity: O(n)
    Space Complexity: O(w)

    Args:
        root: Root of binary tree

    Returns:
        List of levels in zigzag order
    """
    # Handle empty tree
    if not root:
        return []

    result = []
    node_queue = deque([root])
    left_to_right = True  # Start with left-to-right

    while node_queue:
        level_size = len(node_queue)
        level_values = []

        # Process current level
        for _ in range(level_size):
            current_node = node_queue.popleft()
            level_values.append(current_node.value)

            if current_node.left:
                node_queue.append(current_node.left)
            if current_node.right:
                node_queue.append(current_node.right)

        # Reverse level if going right-to-left
        if not left_to_right:
            level_values.reverse()

        result.append(level_values)

        # Toggle direction for next level
        left_to_right = not left_to_right

    return result


# ============================================================================
# PATTERN 3: LEVEL STATISTICS
# ============================================================================

def level_averages(root: Optional[TreeNode]) -> List[float]:
    """
    Calculate average value at each level.

    Example:
        Tree:     3
                 / \
                9  20
                  /  \
                 15   7
        Level 0: 3/1 = 3.0
        Level 1: (9+20)/2 = 14.5
        Level 2: (15+7)/2 = 11.0
        Output: [3.0, 14.5, 11.0]

    Strategy:
    - BFS to process each level
    - Sum all values at level
    - Divide by level size

    Time Complexity: O(n)
    Space Complexity: O(w)

    Args:
        root: Root of binary tree

    Returns:
        List of average values, one per level
    """
    # Handle empty tree
    if not root:
        return []

    level_averages_result = []
    node_queue = deque([root])

    while node_queue:
        level_size = len(node_queue)
        level_sum = 0

        # Sum all values at current level
        for _ in range(level_size):
            current_node = node_queue.popleft()
            level_sum += current_node.value

            if current_node.left:
                node_queue.append(current_node.left)
            if current_node.right:
                node_queue.append(current_node.right)

        # Calculate and store average for this level
        level_average = level_sum / level_size
        level_averages_result.append(level_average)

    return level_averages_result


def max_at_each_level(root: Optional[TreeNode]) -> List[int]:
    """
    Find maximum value at each level.

    Example:
        Tree:     3
                 / \
                9  20
                  /  \
                 15   7
        Level 0 max: 3
        Level 1 max: 20
        Level 2 max: 15
        Output: [3, 20, 15]

    Strategy:
    - BFS to process each level
    - Track maximum while processing level
    - Store max for each level

    Time Complexity: O(n)
    Space Complexity: O(w)

    Args:
        root: Root of binary tree

    Returns:
        List of maximum values, one per level
    """
    # Handle empty tree
    if not root:
        return []

    maximum_values = []
    node_queue = deque([root])

    while node_queue:
        level_size = len(node_queue)
        level_maximum = float('-inf')

        # Find maximum value at current level
        for _ in range(level_size):
            current_node = node_queue.popleft()
            level_maximum = max(level_maximum, current_node.value)

            if current_node.left:
                node_queue.append(current_node.left)
            if current_node.right:
                node_queue.append(current_node.right)

        maximum_values.append(level_maximum)

    return maximum_values


# ============================================================================
# PATTERN 4: TREE VIEWS (RIGHT/LEFT SIDE)
# ============================================================================

def right_side_view(root: Optional[TreeNode]) -> List[int]:
    """
    Get values visible from right side (rightmost node at each level).

    Example:
        Tree:     1
                 / \
                2   3
               /
              4
        Right view sees: 1 (level 0), 3 (level 1), 4 (level 2)
        Output: [1, 3, 4]

    Strategy:
    - BFS level by level
    - For each level, track last node processed
    - Last node is rightmost (visible from right)

    Why it works:
    - BFS processes left-to-right within a level
    - Last node processed per level = rightmost node
    - Only these rightmost nodes are visible from right side

    Time Complexity: O(n)
    Space Complexity: O(w)

    Args:
        root: Root of binary tree

    Returns:
        List of values visible from right side
    """
    # Handle empty tree
    if not root:
        return []

    right_view_values = []
    node_queue = deque([root])

    while node_queue:
        level_size = len(node_queue)

        for node_index in range(level_size):
            current_node = node_queue.popleft()

            # Last node at this level (rightmost)
            if node_index == level_size - 1:
                right_view_values.append(current_node.value)

            if current_node.left:
                node_queue.append(current_node.left)
            if current_node.right:
                node_queue.append(current_node.right)

    return right_view_values


def left_side_view(root: Optional[TreeNode]) -> List[int]:
    """
    Get values visible from left side (leftmost node at each level).

    Example:
        Tree:     1
                 / \
                2   3
                 \
                  4
        Left view sees: 1 (level 0), 2 (level 1), 4 (level 2)
        Output: [1, 2, 4]

    Strategy:
    - BFS level by level
    - For each level, capture first node processed
    - First node is leftmost (visible from left)

    Why it works:
    - BFS processes left-to-right within a level
    - First node processed per level = leftmost node
    - Only these leftmost nodes are visible from left side

    Time Complexity: O(n)
    Space Complexity: O(w)

    Args:
        root: Root of binary tree

    Returns:
        List of values visible from left side
    """
    # Handle empty tree
    if not root:
        return []

    left_view_values = []
    node_queue = deque([root])

    while node_queue:
        level_size = len(node_queue)

        for node_index in range(level_size):
            current_node = node_queue.popleft()

            # First node at this level (leftmost)
            if node_index == 0:
                left_view_values.append(current_node.value)

            if current_node.left:
                node_queue.append(current_node.left)
            if current_node.right:
                node_queue.append(current_node.right)

    return left_view_values


# ============================================================================
# PATTERN 5: MINIMUM DEPTH
# ============================================================================

def min_depth_bfs(root: Optional[TreeNode]) -> int:
    """
    Find minimum depth to nearest leaf using BFS.

    Example:
        Tree:     3
                 / \
                9  20
                  /  \
                 15   7
        Leaf 9 is at depth 2
        Output: 2

    BFS is OPTIMAL for minimum depth:
    - Explores level by level (shortest path first)
    - Stops immediately when first leaf found
    - No need to explore entire tree

    Why BFS better than DFS for minimum:
    - DFS must explore all paths to find minimum
    - BFS finds shortest path first
    - Can return as soon as first leaf encountered

    Time Complexity: O(n) worst case (all nodes on last level)
                     Best case: O(w) if leaf on first level with leaves
    Space Complexity: O(w)

    Args:
        root: Root of binary tree

    Returns:
        Minimum depth (levels from root to nearest leaf)
    """
    # Handle empty tree
    if not root:
        return 0

    # Queue stores (node, depth) pairs
    node_queue = deque([(root, 1)])

    while node_queue:
        current_node, current_depth = node_queue.popleft()

        # First leaf encountered is at minimum depth!
        # Leaf = node with no children
        if not current_node.left and not current_node.right:
            return current_depth

        # Add children with incremented depth
        if current_node.left:
            node_queue.append((current_node.left, current_depth + 1))
        if current_node.right:
            node_queue.append((current_node.right, current_depth + 1))

    return 0


# ============================================================================
# PATTERN 6: CONNECT NODES AT SAME LEVEL
# ============================================================================

def connect_next_pointers(root: Optional[Node]) -> Optional[Node]:
    """
    Connect all nodes at same level using next pointer.

    Example:
        Tree:     1
                 / \
                2   3
               / \   \
              4   5   7

        After connecting:
        Level 0: 1 → None
        Level 1: 2 → 3 → None
        Level 2: 4 → 5 → 7 → None

    Each node's next should point to right neighbor at same level.
    Rightmost node at each level points to None.

    Strategy:
    - BFS level by level
    - For each node except last in level, point to next in queue
    - Queue naturally has next node in level

    Why it works:
    - Queue holds nodes in left-to-right order
    - queue[0] (front) is next node in same level
    - Connect current to queue[0] unless current is last in level

    Time Complexity: O(n)
    Space Complexity: O(w)

    Args:
        root: Root of tree with next pointers

    Returns:
        Root of tree with connected next pointers
    """
    # Handle empty tree
    if not root:
        return None

    node_queue = deque([root])

    while node_queue:
        level_size = len(node_queue)

        for node_index in range(level_size):
            current_node = node_queue.popleft()

            # Connect to next node if not last in level
            if node_index < level_size - 1:
                # Peek at front of queue (next node in level)
                current_node.next = node_queue[0]

            # Add children for next level
            if current_node.left:
                node_queue.append(current_node.left)
            if current_node.right:
                node_queue.append(current_node.right)

    return root


def connect_next_pointers_constant_space(root: Optional[Node]) -> Optional[Node]:
    """
    Connect nodes with O(1) space using already-connected levels.

    Example:
        Perfect binary tree - use next pointers from previous level
        to traverse and connect current level without queue.

    Strategy:
    - Use next pointers from level N to connect level N+1
    - Traverse level N using next pointers
    - For each node in level N, connect its children in level N+1

    Why O(1) space:
    - No queue needed!
    - Use existing next pointers to traverse current level
    - Connect children while traversing

    How it works:
    - Level 0 already "connected" (just root)
    - Use level 0's next pointers (none) to connect level 1
    - Use level 1's next pointers to connect level 2
    - Continue until no more levels

    Time Complexity: O(n)
    Space Complexity: O(1) - no queue, just pointers!

    Args:
        root: Root of PERFECT binary tree

    Returns:
        Root with connected next pointers
    """
    # Handle empty tree
    if not root:
        return None

    # Start with leftmost node of first level
    leftmost_node = root

    # Process each level (stop when reach leaf level)
    while leftmost_node.left:  # While not at leaf level
        # Traverse current level using next pointers
        current_node = leftmost_node

        while current_node:
            # Connect left child to right child
            current_node.left.next = current_node.right

            # Connect right child to next node's left child (if next exists)
            if current_node.next:
                current_node.right.next = current_node.next.left

            # Move to next node in current level
            current_node = current_node.next

        # Move down to next level (leftmost node)
        leftmost_node = leftmost_node.left

    return root


# ============================================================================
# PATTERN 7: VERTICAL ORDER TRAVERSAL
# ============================================================================

def vertical_order_traversal(root: Optional[TreeNode]) -> List[List[int]]:
    """
    Vertical order: group nodes by vertical column.

    Example:
        Tree:     3
                 / \
                9  20
                  /  \
                 15   7

        Columns: 9 is column -1
                 3,15 are column 0
                 20 is column 1
                 7 is column 2

        Output: [[9], [3, 15], [20], [7]]

    Column assignment:
    - Root is column 0
    - Left child: column - 1
    - Right child: column + 1

    Strategy:
    - BFS with (node, column) pairs
    - Group nodes by column in hash table
    - Return columns in sorted order

    Why BFS:
    - Naturally processes top-to-bottom within each column
    - Maintains level order when multiple nodes in same column

    Time Complexity: O(n log n) - n for BFS, log n for sorting columns
    Space Complexity: O(n) - hash table stores all nodes

    Args:
        root: Root of binary tree

    Returns:
        List of columns, each column is list of values top-to-bottom
    """
    # Handle empty tree
    if not root:
        return []

    from collections import defaultdict

    # Map column number to list of node values in that column
    column_table = defaultdict(list)

    # Queue stores (node, column) pairs
    node_queue = deque([(root, 0)])

    while node_queue:
        current_node, column_number = node_queue.popleft()

        # Add node value to its column
        column_table[column_number].append(current_node.value)

        # Left child goes to column - 1
        if current_node.left:
            node_queue.append((current_node.left, column_number - 1))

        # Right child goes to column + 1
        if current_node.right:
            node_queue.append((current_node.right, column_number + 1))

    # Sort columns by column number and return values
    sorted_columns = sorted(column_table.keys())
    return [column_table[column] for column in sorted_columns]


# ============================================================================
# PATTERN 8: SYMMETRIC TREE
# ============================================================================

def is_symmetric(root: Optional[TreeNode]) -> bool:
    """
    Check if tree is symmetric (mirror image of itself).

    Example:
        Symmetric:      1           Not Symmetric:  1
                       / \                         / \
                      2   2                       2   2
                     / \ / \                       \   \
                    3  4 4  3                       3   3

    Strategy: BFS with pairs of nodes to compare in mirror positions.
    - Compare left.left with right.right
    - Compare left.right with right.left

    Why it works:
    - For symmetry, mirrored positions must have equal values
    - Left subtree must be mirror of right subtree
    - Check pairs of mirrored positions

    Time Complexity: O(n)
    Space Complexity: O(w)

    Args:
        root: Root of binary tree

    Returns:
        True if tree is symmetric, False otherwise
    """
    # Empty tree is symmetric
    if not root:
        return True

    # Start by comparing root's left and right subtrees
    comparison_queue = deque([(root.left, root.right)])

    while comparison_queue:
        left_node, right_node = comparison_queue.popleft()

        # Both None: symmetric so far, continue
        if not left_node and not right_node:
            continue

        # One None or values differ: not symmetric
        if not left_node or not right_node or left_node.value != right_node.value:
            return False

        # Add children in mirror order for comparison
        # Left's left should match Right's right (outer pair)
        comparison_queue.append((left_node.left, right_node.right))

        # Left's right should match Right's left (inner pair)
        comparison_queue.append((left_node.right, right_node.left))

    # All pairs matched - tree is symmetric
    return True


# ============================================================================
# PATTERN 9: NODES AT DISTANCE K
# ============================================================================

def nodes_at_distance_k(root: Optional[TreeNode], target: TreeNode,
                       distance_k: int) -> List[int]:
    """
    Find all nodes at distance K from target node.

    Example:
        Tree:     3
                 / \
                5   1
               / \   \
              6   2   8
                 / \
                7   4
        Target: 5, K: 2
        Nodes at distance 2 from 5: [7, 4, 1]

    Distance can go:
    - Down to descendants
    - Up to ancestors
    - Across to other subtrees (via ancestors)

    Strategy (2-phase BFS):
    1. Build parent pointers with first BFS (treat as undirected graph)
    2. BFS from target, exploring left, right, AND parent directions

    Why it works:
    - Parent pointers let us go "up" the tree
    - From target, can explore in all directions like undirected graph
    - BFS naturally finds all nodes at exact distance

    Time Complexity: O(n) - two BFS passes
    Space Complexity: O(n) - parent map + visited set

    Args:
        root: Root of binary tree
        target: Target node to measure distance from
        distance_k: Distance to search for nodes

    Returns:
        List of values at exactly distance K from target
    """
    # Handle empty tree
    if not root:
        return []

    # Phase 1: Build parent pointers
    parent_map = {}
    node_queue = deque([root])

    while node_queue:
        current_node = node_queue.popleft()

        # Record parent relationship
        if current_node.left:
            parent_map[current_node.left] = current_node
            node_queue.append(current_node.left)

        if current_node.right:
            parent_map[current_node.right] = current_node
            node_queue.append(current_node.right)

    # Phase 2: BFS from target with distance tracking
    distance_queue = deque([(target, 0)])  # (node, current_distance)
    visited_nodes = {target}
    result_values = []

    while distance_queue:
        current_node, current_distance = distance_queue.popleft()

        # Found node at target distance
        if current_distance == distance_k:
            result_values.append(current_node.value)
            continue  # Don't explore further from this node

        # Explore all three directions: left, right, parent
        neighbor_nodes = [
            current_node.left,
            current_node.right,
            parent_map.get(current_node)  # May be None if current is root
        ]

        for neighbor_node in neighbor_nodes:
            # Only visit unvisited neighbors
            if neighbor_node and neighbor_node not in visited_nodes:
                visited_nodes.add(neighbor_node)
                distance_queue.append((neighbor_node, current_distance + 1))

    return result_values


# ============================================================================
# BFS TEMPLATE - MASTER PATTERN
# ============================================================================

def bfs_template_explanation():
    """
    UNIVERSAL BFS TEMPLATE - Master Pattern for Level-Order Problems

    # ================================================================
    # BASIC LEVEL-ORDER BFS
    # ================================================================

    if not root:
        return []

    result = []
    node_queue = deque([root])

    while node_queue:
        # CRITICAL: Get level size at start (queue length changes during loop!)
        level_size = len(node_queue)
        level_data = []

        # Process EXACTLY level_size nodes (entire current level)
        for _ in range(level_size):
            current_node = node_queue.popleft()

            # Process current node (collect value, check condition, etc.)
            level_data.append(current_node.value)

            # Add children for next level
            if current_node.left:
                node_queue.append(current_node.left)
            if current_node.right:
                node_queue.append(current_node.right)

        # Process collected level data
        result.append(level_data)

    return result


    # ================================================================
    # KEY PATTERNS AND VARIATIONS
    # ================================================================

    1. TRACK LEVEL SIZE
       - len(queue) at START of while loop
       - Gives exact count of current level
       - Process that many nodes before moving to next level

    2. PROCESS ENTIRE LEVEL BEFORE NEXT
       - for _ in range(level_size) ensures level completeness
       - Queue grows during loop (adding children)
       - But we only process original level_size nodes

    3. USE DEQUE FOR EFFICIENCY
       - deque provides O(1) append and popleft
       - list.pop(0) would be O(n) - inefficient!
       - from collections import deque

    4. LEVEL STATISTICS
       - Sum/count/max/min while processing level
       - Calculate result after processing all level nodes
       - Example: average = level_sum / level_size

    5. TREE VIEWS (first/last per level)
       - Right view: last node per level (index == level_size - 1)
       - Left view: first node per level (index == 0)
       - Track position within level

    6. SHORTEST PATH / MINIMUM DEPTH
       - Track depth/distance with node in queue
       - queue stores (node, depth) tuples
       - Return immediately when condition met (optimal)

    7. ZIGZAG TRAVERSAL
       - Regular BFS
       - Reverse every other level
       - Toggle boolean flag after each level

    8. VERTICAL/DIAGONAL GROUPING
       - Track extra dimension (column, diagonal)
       - Queue stores (node, dimension) tuples
       - Group by dimension value

    9. CONNECT LEVEL NODES
       - Use queue to find next in level
       - queue[0] is next node at same level
       - Connect if not last in level

   10. SYMMETRIC COMPARISON
       - Queue stores PAIRS of nodes to compare
       - Check mirrored positions
       - Add children in mirrored order


    # ================================================================
    # BFS VS DFS - WHEN TO USE WHICH
    # ================================================================

    Use BFS when:
    - Need level-by-level processing
    - Finding shortest path / minimum depth
    - Level statistics (average, max per level)
    - Tree views (left/right side)
    - Connecting nodes at same level

    Use DFS when:
    - Path finding (root to leaf)
    - Tree height / maximum depth
    - Subtree properties
    - Backtracking needed
    - Less memory (O(h) vs O(w))


    # ================================================================
    # COMMON MISTAKES TO AVOID
    # ================================================================

    1. Don't use list.pop(0) - use deque.popleft()
       - list.pop(0) is O(n) - shifts all elements!
       - deque.popleft() is O(1)

    2. Don't calculate len(queue) inside for loop
       - Queue length changes as you add children
       - Take snapshot BEFORE for loop

    3. Don't forget to check for None before accessing node
       - Always check: if current_node: before using

    4. Don't modify queue while iterating
       - Use for _ in range(level_size) pattern
       - Not: for node in queue (queue changes!)

    5. Don't confuse levels with height
       - Level 0 = root
       - Height = maximum level + 1
    """
    pass


# ============================================================================
# TEST CASES
# ============================================================================

def create_sample_tree():
    """
    Create sample tree for testing:
           3
          / \
         9  20
           /  \
          15   7
    """
    root = TreeNode(3)
    root.left = TreeNode(9)
    root.right = TreeNode(20)
    root.right.left = TreeNode(15)
    root.right.right = TreeNode(7)
    return root


def run_tests():
    """
    Comprehensive test cases demonstrating all BFS patterns.
    """
    print("=" * 70)
    print("BINARY TREE BFS PATTERN - COMPREHENSIVE TEST CASES")
    print("=" * 70)

    sample_tree = create_sample_tree()

    # Test 1: Level-Order Traversal
    print("\n1. LEVEL-ORDER TRAVERSAL")
    print("-" * 70)
    level_order_result = level_order_traversal(sample_tree)
    print(f"   Output: {level_order_result}")
    print(f"   Expected: [[3], [9, 20], [15, 7]]")
    print(f"\n   Why: Process tree level by level using queue")
    print(f"   Level 0: [3], Level 1: [9, 20], Level 2: [15, 7]")

    # Test 2: Bottom-Up Level Order
    print("\n2. LEVEL-ORDER BOTTOM-UP")
    print("-" * 70)
    bottom_up_result = level_order_bottom_up(sample_tree)
    print(f"   Output: {bottom_up_result}")
    print(f"   Expected: [[15, 7], [9, 20], [3]]")
    print(f"\n   Why: Same as regular level-order, but reversed")

    # Test 3: Zigzag Level Order
    print("\n3. ZIGZAG LEVEL-ORDER")
    print("-" * 70)
    zigzag_result = zigzag_level_order(sample_tree)
    print(f"   Output: {zigzag_result}")
    print(f"   Expected: [[3], [20, 9], [15, 7]]")
    print(f"\n   Why: Alternate direction each level")
    print(f"   Level 0 (→): [3], Level 1 (←): [20, 9], Level 2 (→): [15, 7]")

    # Test 4: Level Averages
    print("\n4. AVERAGE AT EACH LEVEL")
    print("-" * 70)
    averages_result = level_averages(sample_tree)
    print(f"   Output: {averages_result}")
    print(f"   Expected: [3.0, 14.5, 11.0]")
    print(f"\n   Why: Level 0: 3/1=3.0, Level 1: (9+20)/2=14.5, Level 2: (15+7)/2=11.0")

    # Test 5: Max at Each Level
    print("\n5. MAXIMUM AT EACH LEVEL")
    print("-" * 70)
    max_result = max_at_each_level(sample_tree)
    print(f"   Output: {max_result}")
    print(f"   Expected: [3, 20, 15]")
    print(f"\n   Why: Track maximum value while processing each level")

    # Test 6: Right Side View
    print("\n6. RIGHT SIDE VIEW")
    print("-" * 70)
    right_view_result = right_side_view(sample_tree)
    print(f"   Output: {right_view_result}")
    print(f"   Expected: [3, 20, 7]")
    print(f"\n   Why: Last (rightmost) node at each level")
    print(f"   Standing on right, you see: 3, then 20, then 7")

    # Test 7: Left Side View
    print("\n7. LEFT SIDE VIEW")
    print("-" * 70)
    left_view_result = left_side_view(sample_tree)
    print(f"   Output: {left_view_result}")
    print(f"   Expected: [3, 9, 15]")
    print(f"\n   Why: First (leftmost) node at each level")
    print(f"   Standing on left, you see: 3, then 9, then 15")

    # Test 8: Minimum Depth
    print("\n8. MINIMUM DEPTH (BFS)")
    print("-" * 70)
    min_depth_result = min_depth_bfs(sample_tree)
    print(f"   Output: {min_depth_result}")
    print(f"   Expected: 2 (path: 3→9)")
    print(f"\n   Why: BFS finds shortest path to leaf (node 9 at level 2)")

    # Test 9: Vertical Order
    print("\n9. VERTICAL ORDER TRAVERSAL")
    print("-" * 70)
    vertical_result = vertical_order_traversal(sample_tree)
    print(f"   Output: {vertical_result}")
    print(f"   Expected: [[9], [3, 15], [20], [7]]")
    print(f"\n   Why: Group by column: 9 (col -1), 3,15 (col 0), 20 (col 1), 7 (col 2)")

    # Test 10: Symmetric Tree
    print("\n10. SYMMETRIC TREE CHECK")
    print("-" * 70)
    symmetric_tree = TreeNode(1)
    symmetric_tree.left = TreeNode(2)
    symmetric_tree.right = TreeNode(2)
    symmetric_tree.left.left = TreeNode(3)
    symmetric_tree.right.right = TreeNode(3)
    symmetric_result = is_symmetric(symmetric_tree)
    print(f"   Output: {symmetric_result}")
    print(f"   Expected: True")
    print(f"\n   Why: Left and right subtrees are mirrors of each other")

    # Summary
    print("\n" + "=" * 70)
    print("KEY INSIGHTS - BINARY TREE BFS PATTERNS")
    print("=" * 70)
    print("1. BFS uses queue (FIFO), DFS uses stack/recursion (LIFO)")
    print("2. Track level size: len(queue) BEFORE processing level")
    print("3. BFS optimal for: shortest path, minimum depth, level problems")
    print("4. Space complexity: O(w) where w = maximum width of tree")
    print("5. Right view: last node per level; Left view: first node per level")
    print("6. Zigzag: alternate direction, reverse every other level")
    print("7. Connect levels: use queue to peek at next node in level")
    print("8. Statistics: process all nodes in level, then calculate")
    print("9. Distance K: build parent map, then BFS in all directions")
    print("10. Use deque, not list - deque.popleft() is O(1), list.pop(0) is O(n)")
    print("=" * 70)


if __name__ == "__main__":
    run_tests()

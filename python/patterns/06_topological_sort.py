"""
TOPOLOGICAL SORT PATTERN - Comprehensive Guide

Topological sort is a linear ordering of vertices in a Directed Acyclic Graph (DAG)
such that for every directed edge u→v, vertex u comes before v in the ordering.

WHEN TO USE:
- Course prerequisites/scheduling
- Build order dependencies
- Task ordering with constraints
- Detecting cycles in directed graphs
- Alien dictionary problems

REQUIREMENTS:
- Graph must be DIRECTED
- Graph must be ACYCLIC (DAG)
- If graph has cycle, topological sort is impossible

TIME COMPLEXITY: O(V + E) - visit all vertices and edges
SPACE COMPLEXITY: O(V + E) - adjacency list and auxiliary structures

TWO MAIN ALGORITHMS:
1. Kahn's Algorithm (BFS-based) - uses in-degree, easier to understand
2. DFS-based - uses recursion and finishing times
"""

from typing import List, Dict, Set
from collections import deque, defaultdict


# ============================================================================
# PATTERN 1: KAHN'S ALGORITHM (BFS-BASED)
# ============================================================================

def topological_sort_kahn(number_of_vertices: int, edges: List[List[int]]) -> List[int]:
    """
    Topological sort using Kahn's algorithm (BFS-based approach).

    Example:
        Graph: 0 → 1 → 3
               ↓   ↗
               2
        Edges: [[0,1], [0,2], [1,3], [2,3]]
        Result: [0, 1, 2, 3] or [0, 2, 1, 3]

    Algorithm Steps:
    1. Calculate in-degree (number of incoming edges) for each vertex
    2. Add all vertices with in-degree 0 to queue (no dependencies)
    3. Process queue:
       - Remove vertex (no more dependencies)
       - Decrease in-degree of all its neighbors
       - If neighbor's in-degree becomes 0, add to queue
    4. If processed all vertices, return order
       If not all processed, cycle exists (impossible to sort)

    Why it works:
    - Vertices with in-degree 0 have no dependencies (can go first)
    - After processing vertex, its dependents have one less dependency
    - Process vertices in order of dependency resolution
    - If cycle exists, some vertices never reach in-degree 0

    Time Complexity: O(V + E) - visit each vertex and edge once
    Space Complexity: O(V + E) - adjacency list + in-degree array + queue

    Args:
        number_of_vertices: Total number of vertices in graph
        edges: List of directed edges [source, destination]

    Returns:
        Topological ordering if possible, empty list if cycle exists
    """
    # Build adjacency list representation of graph
    adjacency_graph = defaultdict(list)

    # Track incoming edges for each vertex
    incoming_edge_count = [0] * number_of_vertices

    # Build graph and count incoming edges
    for source_vertex, destination_vertex in edges:
        adjacency_graph[source_vertex].append(destination_vertex)
        incoming_edge_count[destination_vertex] += 1

    # Initialize queue with vertices that have no dependencies (in-degree = 0)
    processing_queue = deque([
        vertex_index
        for vertex_index in range(number_of_vertices)
        if incoming_edge_count[vertex_index] == 0
    ])

    topological_order = []

    # Process vertices in dependency order
    while processing_queue:
        # Remove vertex with no dependencies
        current_vertex = processing_queue.popleft()
        topological_order.append(current_vertex)

        # Decrease in-degree of neighbors (remove dependency)
        for neighbor_vertex in adjacency_graph[current_vertex]:
            incoming_edge_count[neighbor_vertex] -= 1

            # If neighbor has no more dependencies, can process it
            if incoming_edge_count[neighbor_vertex] == 0:
                processing_queue.append(neighbor_vertex)

    # If topological_order doesn't contain all vertices, cycle exists
    # (some vertices never reached in-degree 0)
    all_vertices_processed = len(topological_order) == number_of_vertices

    return topological_order if all_vertices_processed else []


# ============================================================================
# PATTERN 2: DFS-BASED TOPOLOGICAL SORT
# ============================================================================

def topological_sort_dfs(number_of_vertices: int, edges: List[List[int]]) -> List[int]:
    """
    Topological sort using DFS (recursive approach).

    Example:
        Graph: 0 → 1 → 3
               ↓   ↗
               2
        DFS visits: 0 → 1 → 3 (add 3), then 2 → 3 (already visited)
        Postorder: [3, 1, 2, 0]
        Reversed: [0, 2, 1, 3] ✓

    Algorithm Steps:
    1. Perform DFS from each unvisited vertex
    2. Add vertex to result AFTER visiting all its descendants (postorder)
    3. Reverse the result to get topological order

    Why postorder works:
    - Postorder adds vertex after all descendants processed
    - In topological order, descendants come after vertex
    - Reversing postorder gives correct topological order
    - Example: A → B → C
      - DFS visits: A → B → C
      - Postorder adds: C, then B, then A → [C, B, A]
      - Reverse: [A, B, C] ✓

    Time Complexity: O(V + E)
    Space Complexity: O(V + E) - adjacency list + visited set + recursion stack

    Args:
        number_of_vertices: Total number of vertices
        edges: List of directed edges [source, destination]

    Returns:
        Topological ordering
    """
    # Build adjacency list
    adjacency_graph = defaultdict(list)
    for source_vertex, destination_vertex in edges:
        adjacency_graph[source_vertex].append(destination_vertex)

    # Track visited vertices
    visited_vertices = set()

    # Store vertices in postorder (will reverse later)
    postorder_result = []

    def depth_first_search(current_vertex):
        """Recursive DFS that builds postorder traversal."""
        # Already visited this vertex
        if current_vertex in visited_vertices:
            return

        # Mark as visited
        visited_vertices.add(current_vertex)

        # Visit all neighbors first (descendants must be added before vertex)
        for neighbor_vertex in adjacency_graph[current_vertex]:
            depth_first_search(neighbor_vertex)

        # Add vertex AFTER visiting all descendants (postorder)
        postorder_result.append(current_vertex)

    # Start DFS from each unvisited vertex
    for vertex_index in range(number_of_vertices):
        if vertex_index not in visited_vertices:
            depth_first_search(vertex_index)

    # Reverse postorder to get topological order
    # Postorder: last to first in dependency chain
    # Topological: first to last in dependency chain
    return postorder_result[::-1]


# ============================================================================
# PATTERN 3: COURSE SCHEDULE (CYCLE DETECTION)
# ============================================================================

def can_finish_courses(number_of_courses: int, prerequisites: List[List[int]]) -> bool:
    """
    Determine if all courses can be finished given prerequisites.

    Example:
        Courses: 4
        Prerequisites: [[1,0], [2,0], [3,1], [3,2]]
        Meaning: Course 1 requires 0, Course 2 requires 0, Course 3 requires 1 and 2
        Can finish: True (order: 0 → 1 → 2 → 3 or 0 → 2 → 1 → 3)

        Courses: 2
        Prerequisites: [[1,0], [0,1]]
        Meaning: Course 1 requires 0, Course 0 requires 1 (cycle!)
        Can finish: False

    Prerequisite [course, required] means:
    - To take 'course', must first take 'required'
    - Creates edge: required → course

    Can finish all courses if and only if NO CYCLE exists.

    Why Kahn's algorithm detects cycles:
    - If cycle exists, vertices in cycle never reach in-degree 0
    - They all depend on each other (circular dependency)
    - Can't process all vertices = cycle exists

    Time Complexity: O(V + E)
    Space Complexity: O(V + E)

    Args:
        number_of_courses: Total number of courses
        prerequisites: List of [course, prerequisite] pairs

    Returns:
        True if can finish all courses (no cycle), False otherwise
    """
    # Build graph: prerequisite → course (dependency direction)
    adjacency_graph = defaultdict(list)
    incoming_edge_count = [0] * number_of_courses

    for course, prerequisite_course in prerequisites:
        # Edge: prerequisite must come before course
        adjacency_graph[prerequisite_course].append(course)
        incoming_edge_count[course] += 1

    # Queue with courses having no prerequisites (in-degree 0)
    processing_queue = deque([
        course_index
        for course_index in range(number_of_courses)
        if incoming_edge_count[course_index] == 0
    ])

    completed_courses = 0

    # Process courses in dependency order
    while processing_queue:
        current_course = processing_queue.popleft()
        completed_courses += 1

        # Unlock dependent courses (courses that required this one)
        for next_course in adjacency_graph[current_course]:
            incoming_edge_count[next_course] -= 1

            # If course has no more prerequisites, can take it
            if incoming_edge_count[next_course] == 0:
                processing_queue.append(next_course)

    # Can finish all courses if no cycle (all courses processable)
    return completed_courses == number_of_courses


def find_course_order(number_of_courses: int, prerequisites: List[List[int]]) -> List[int]:
    """
    Find valid course order to complete all courses.

    Example:
        Courses: 4
        Prerequisites: [[1,0], [2,0], [3,1], [3,2]]
        Valid order: [0, 1, 2, 3] or [0, 2, 1, 3]

    Returns topological ordering if possible (no cycle).
    Returns empty list if impossible (cycle exists).

    Time Complexity: O(V + E)
    Space Complexity: O(V + E)

    Args:
        number_of_courses: Total number of courses
        prerequisites: List of [course, prerequisite] pairs

    Returns:
        Valid course order if possible, empty list if cycle exists
    """
    # Build dependency graph
    adjacency_graph = defaultdict(list)
    incoming_edge_count = [0] * number_of_courses

    for course, prerequisite_course in prerequisites:
        adjacency_graph[prerequisite_course].append(course)
        incoming_edge_count[course] += 1

    # Start with courses having no prerequisites
    processing_queue = deque([
        course_index
        for course_index in range(number_of_courses)
        if incoming_edge_count[course_index] == 0
    ])

    course_order = []

    while processing_queue:
        current_course = processing_queue.popleft()
        course_order.append(current_course)

        # Unlock courses that depended on this one
        for next_course in adjacency_graph[current_course]:
            incoming_edge_count[next_course] -= 1

            if incoming_edge_count[next_course] == 0:
                processing_queue.append(next_course)

    # Return order if all courses processable (no cycle)
    # Otherwise return empty (cycle prevents completion)
    all_courses_processable = len(course_order) == number_of_courses

    return course_order if all_courses_processable else []


# ============================================================================
# PATTERN 4: CYCLE DETECTION IN DIRECTED GRAPH
# ============================================================================

def has_cycle_dfs(number_of_vertices: int, edges: List[List[int]]) -> bool:
    """
    Detect cycle in directed graph using DFS with three vertex states.

    Example:
        Graph with cycle: 0 → 1 → 2 → 0 (back to 0)
        Returns: True

        Graph without cycle: 0 → 1 → 2 → 3
        Returns: False

    Vertex States:
    - WHITE (0): Unvisited - haven't explored yet
    - GRAY (1):  Visiting - currently in DFS path (on recursion stack)
    - BLACK (2): Visited - finished exploring this vertex and all descendants

    Cycle Detection:
    - If we encounter a GRAY vertex, we found a back edge
    - Back edge points to ancestor in current DFS path
    - Back edge = cycle exists!

    Why this works:
    - GRAY vertices are ancestors in current DFS path
    - Encountering GRAY vertex means path loops back to ancestor
    - BLACK vertices are finished (can't create cycle with current path)

    Time Complexity: O(V + E)
    Space Complexity: O(V + E)

    Args:
        number_of_vertices: Total number of vertices
        edges: List of directed edges [source, destination]

    Returns:
        True if cycle exists, False otherwise
    """
    # Build adjacency list
    adjacency_graph = defaultdict(list)
    for source_vertex, destination_vertex in edges:
        adjacency_graph[source_vertex].append(destination_vertex)

    # Define three states
    UNVISITED, VISITING, VISITED = 0, 1, 2

    # Track state of each vertex
    vertex_state = [UNVISITED] * number_of_vertices

    def depth_first_search(current_vertex):
        """
        DFS to detect cycle.
        Returns True if cycle found, False otherwise.
        """
        # Found back edge (vertex in current DFS path) - CYCLE!
        if vertex_state[current_vertex] == VISITING:
            return True

        # Already fully explored this vertex - no cycle through here
        if vertex_state[current_vertex] == VISITED:
            return False

        # Mark as visiting (add to current DFS path)
        vertex_state[current_vertex] = VISITING

        # Explore all neighbors
        for neighbor_vertex in adjacency_graph[current_vertex]:
            if depth_first_search(neighbor_vertex):
                return True  # Cycle found in descendant

        # Finished exploring this vertex (remove from DFS path)
        vertex_state[current_vertex] = VISITED

        return False

    # Check from each unvisited vertex (graph might be disconnected)
    for vertex_index in range(number_of_vertices):
        if vertex_state[vertex_index] == UNVISITED:
            if depth_first_search(vertex_index):
                return True  # Cycle found

    # No cycle found in entire graph
    return False


# ============================================================================
# PATTERN 5: ALL POSSIBLE TOPOLOGICAL ORDERS
# ============================================================================

def all_topological_orders(number_of_vertices: int, edges: List[List[int]]) -> List[List[int]]:
    """
    Find ALL possible topological orderings using backtracking.

    Example:
        Graph: 0 → 1
               0 → 2
        All orders: [[0,1,2], [0,2,1]]
        (Both valid: 0 must come before 1 and 2, but 1 and 2 can be in any order)

    Uses backtracking to explore all valid orderings:
    - At each step, try all vertices with in-degree 0
    - Recursively build rest of ordering
    - Backtrack to try other possibilities

    Why backtracking:
    - Multiple vertices might have in-degree 0 simultaneously
    - Each choice leads to different valid ordering
    - Need to explore all possibilities
    - Backtrack by restoring in-degrees

    Time Complexity: O(V! * E) - factorial possibilities, check edges each time
    Space Complexity: O(V + E)

    Args:
        number_of_vertices: Total number of vertices
        edges: List of directed edges

    Returns:
        List of all valid topological orderings
    """
    # Build graph and calculate in-degrees
    adjacency_graph = defaultdict(list)
    incoming_edge_count = [0] * number_of_vertices

    for source_vertex, destination_vertex in edges:
        adjacency_graph[source_vertex].append(destination_vertex)
        incoming_edge_count[destination_vertex] += 1

    all_orderings = []
    current_path = []

    def backtrack_orderings():
        """Backtracking helper to explore all valid orderings."""
        # Base case: found complete ordering
        if len(current_path) == number_of_vertices:
            all_orderings.append(current_path[:])  # Copy current path
            return

        # Try each vertex with in-degree 0 (multiple choices possible)
        for vertex_index in range(number_of_vertices):
            # Can only choose vertices with no dependencies that aren't already in path
            if incoming_edge_count[vertex_index] == 0 and vertex_index not in current_path:
                # Choose this vertex
                current_path.append(vertex_index)

                # Update in-degrees (simulate removing vertex)
                for neighbor_vertex in adjacency_graph[vertex_index]:
                    incoming_edge_count[neighbor_vertex] -= 1

                # Recurse to find rest of ordering
                backtrack_orderings()

                # Backtrack: undo choices
                current_path.pop()
                for neighbor_vertex in adjacency_graph[vertex_index]:
                    incoming_edge_count[neighbor_vertex] += 1

    backtrack_orderings()
    return all_orderings


# ============================================================================
# PATTERN 6: ALIEN DICTIONARY
# ============================================================================

def alien_dictionary(words: List[str]) -> str:
    """
    Determine character ordering from sorted alien dictionary.

    Example:
        words = ["wrt", "wrf", "er", "ett", "rftt"]
        Comparisons:
        - "wrt" < "wrf" → t < f
        - "wrf" < "er"  → w < e
        - "er" < "ett"  → r < t
        - "ett" < "rftt" → e < r
        Character order: w → e → r → t → f = "wertf"

    Strategy:
    1. Compare adjacent words to find character ordering
    2. For each pair, find first differing character
    3. Add directed edge from first char to second char
    4. Perform topological sort on character graph
    5. Result is alien alphabet order

    Why it works:
    - Dictionary is sorted, so earlier words come before later words
    - First differing character determines ordering
    - Build dependency graph of characters
    - Topological sort gives valid character ordering

    Edge Cases:
    - If longer word is prefix of shorter word → invalid (e.g., "abc" before "ab")
    - If cycle exists → invalid ordering

    Time Complexity: O(C) where C = total characters in all words
    Space Complexity: O(1) - alphabet size is constant (at most 26 characters)

    Args:
        words: List of words in sorted alien dictionary order

    Returns:
        String representing alien alphabet order, empty if invalid
    """
    # Build character dependency graph
    character_graph = defaultdict(set)
    incoming_edge_count = defaultdict(int)

    # Initialize in-degree for all characters that appear
    for word in words:
        for character in word:
            incoming_edge_count[character] = incoming_edge_count.get(character, 0)

    # Build edges from adjacent word pairs
    for word_index in range(len(words) - 1):
        first_word = words[word_index]
        second_word = words[word_index + 1]
        minimum_length = min(len(first_word), len(second_word))

        # Check invalid case: longer word is prefix of shorter word
        # e.g., "abc" before "ab" is invalid in dictionary
        words_share_prefix = first_word[:minimum_length] == second_word[:minimum_length]
        if len(first_word) > len(second_word) and words_share_prefix:
            return ""  # Invalid dictionary ordering

        # Find first character that differs
        for char_index in range(minimum_length):
            first_character = first_word[char_index]
            second_character = second_word[char_index]

            # Found differing characters - this determines ordering
            if first_character != second_character:
                # Add edge: first_character must come before second_character
                if second_character not in character_graph[first_character]:
                    character_graph[first_character].add(second_character)
                    incoming_edge_count[second_character] += 1
                break  # Only first difference matters

    # Topological sort on characters
    processing_queue = deque([
        character
        for character in incoming_edge_count
        if incoming_edge_count[character] == 0
    ])

    alphabet_order = []

    while processing_queue:
        current_character = processing_queue.popleft()
        alphabet_order.append(current_character)

        # Process characters that depend on current character
        for neighbor_character in character_graph[current_character]:
            incoming_edge_count[neighbor_character] -= 1

            if incoming_edge_count[neighbor_character] == 0:
                processing_queue.append(neighbor_character)

    # Check if all characters processed (no cycle)
    all_characters_processed = len(alphabet_order) == len(incoming_edge_count)

    return ''.join(alphabet_order) if all_characters_processed else ""


# ============================================================================
# PATTERN 7: MINIMUM HEIGHT TREES
# ============================================================================

def find_min_height_trees(total_nodes: int, edges: List[List[int]]) -> List[int]:
    """
    Find all root nodes that result in minimum height trees (UNDIRECTED graph).

    Example:
        Nodes: 5
        Edges: [[0,1], [1,2], [1,3], [2,4]]
        Tree:    0
                 |
                 1
                / \
               2   3
               |
               4
        If root = 1 or 2: height = 2 (minimum)
        If root = 0, 3, or 4: height = 3
        Result: [1, 2]

    Strategy: Trim leaf nodes layer by layer (similar to topological sort).
    - Start from leaf nodes (degree 1)
    - Remove leaves layer by layer
    - Remaining 1-2 nodes are centroids (minimum height roots)

    Why it works:
    - Centroids are center of tree
    - Farthest from all leaves
    - Removing layers of leaves reveals centroid
    - At most 2 centroids possible in a tree

    Time Complexity: O(V) - visit each node once
    Space Complexity: O(V) - adjacency list + queue

    Args:
        total_nodes: Number of nodes in tree
        edges: List of undirected edges

    Returns:
        List of node indices that result in minimum height trees
    """
    # Edge case: single node tree
    if total_nodes == 1:
        return [0]

    # Build adjacency list for undirected graph (use sets for easy removal)
    adjacency_graph = defaultdict(set)

    for first_node, second_node in edges:
        # Undirected: add edge in both directions
        adjacency_graph[first_node].add(second_node)
        adjacency_graph[second_node].add(first_node)

    # Find all leaf nodes (degree = 1, only one neighbor)
    leaf_nodes = deque([
        node_index
        for node_index in range(total_nodes)
        if len(adjacency_graph[node_index]) == 1
    ])

    remaining_nodes = total_nodes

    # Trim leaves layer by layer until only centroids remain (1 or 2 nodes)
    while remaining_nodes > 2:
        leaf_count = len(leaf_nodes)
        remaining_nodes -= leaf_count

        # Process current layer of leaves
        for _ in range(leaf_count):
            current_leaf = leaf_nodes.popleft()

            # Remove leaf from its only neighbor
            neighbor_node = adjacency_graph[current_leaf].pop()
            adjacency_graph[neighbor_node].remove(current_leaf)

            # If neighbor becomes a leaf (degree becomes 1), add to queue
            if len(adjacency_graph[neighbor_node]) == 1:
                leaf_nodes.append(neighbor_node)

    # Remaining nodes are centroids (minimum height tree roots)
    return list(leaf_nodes)


# ============================================================================
# TOPOLOGICAL SORT TEMPLATE - MASTER PATTERN
# ============================================================================

def topological_sort_template_explanation():
    """
    TOPOLOGICAL SORT TEMPLATE - Master Pattern (Kahn's Algorithm)

    # ================================================================
    # STEP 1: Build graph and calculate in-degrees
    # ================================================================

    adjacency_graph = defaultdict(list)
    incoming_edge_count = [0] * number_of_vertices

    # Build directed graph
    for source_vertex, destination_vertex in edges:
        adjacency_graph[source_vertex].append(destination_vertex)
        incoming_edge_count[destination_vertex] += 1


    # ================================================================
    # STEP 2: Initialize queue with vertices having in-degree 0
    # ================================================================

    processing_queue = deque([
        vertex_index
        for vertex_index in range(number_of_vertices)
        if incoming_edge_count[vertex_index] == 0
    ])

    topological_order = []


    # ================================================================
    # STEP 3: Process queue (BFS-style)
    # ================================================================

    while processing_queue:
        current_vertex = processing_queue.popleft()
        topological_order.append(current_vertex)

        # Decrease in-degree of neighbors (remove dependency)
        for neighbor_vertex in adjacency_graph[current_vertex]:
            incoming_edge_count[neighbor_vertex] -= 1

            # If neighbor has no more dependencies, can process it
            if incoming_edge_count[neighbor_vertex] == 0:
                processing_queue.append(neighbor_vertex)


    # ================================================================
    # STEP 4: Check if all vertices processed (cycle detection)
    # ================================================================

    if len(topological_order) != number_of_vertices:
        # Not all vertices processed = cycle exists!
        return []

    return topological_order


    # ================================================================
    # KEY POINTS
    # ================================================================

    1. In-degree = number of incoming edges to vertex
       - Represents number of dependencies
       - In-degree 0 = no dependencies (can be processed)

    2. Start with vertices having in-degree 0
       - These have no prerequisites
       - Safe to process first

    3. Remove vertex and update neighbors' in-degrees
       - Simulates removing dependencies
       - Unlocks dependent vertices

    4. If can't process all vertices, cycle exists
       - Some vertices never reach in-degree 0
       - Circular dependencies prevent completion

    5. DFS Alternative:
       - Use postorder traversal
       - Reverse result for topological order
       - More intuitive for some problems


    # ================================================================
    # COMMON APPLICATIONS
    # ================================================================

    1. Course Prerequisites:
       - Courses = vertices, prerequisites = edges
       - Find valid course order

    2. Build Dependencies:
       - Files/modules = vertices, dependencies = edges
       - Find build order

    3. Task Scheduling:
       - Tasks = vertices, dependencies = edges
       - Find execution order

    4. Cycle Detection:
       - If topological sort fails, cycle exists
       - Useful for validating DAG property
    """
    pass


# ============================================================================
# TEST CASES
# ============================================================================

def run_tests():
    """
    Comprehensive test cases demonstrating all topological sort patterns.
    """
    print("=" * 70)
    print("TOPOLOGICAL SORT PATTERN - COMPREHENSIVE TEST CASES")
    print("=" * 70)

    # Test 1: Basic Topological Sort
    print("\n1. BASIC TOPOLOGICAL SORT")
    print("-" * 70)
    test_edges_1 = [[0, 1], [0, 2], [1, 3], [2, 3]]
    print(f"   Edges: {test_edges_1}")
    kahn_result = topological_sort_kahn(4, test_edges_1)
    dfs_result = topological_sort_dfs(4, test_edges_1)
    print(f"   Kahn's algorithm: {kahn_result}")
    print(f"   DFS approach:     {dfs_result}")
    print(f"   Expected: [0, 1, 2, 3] or [0, 2, 1, 3]")
    print(f"\n   Why: 0 has no dependencies, must come first")
    print(f"   1 and 2 both depend only on 0 (can be in any order)")
    print(f"   3 depends on both 1 and 2, must come last")

    # Test 2: Course Schedule (Can Finish)
    print("\n2. COURSE SCHEDULE - CAN FINISH?")
    print("-" * 70)
    test_prereqs_2 = [[1, 0], [2, 0], [3, 1], [3, 2]]
    print(f"   Prerequisites: {test_prereqs_2}")
    can_finish = can_finish_courses(4, test_prereqs_2)
    print(f"   Can finish all courses: {can_finish}")
    print(f"   Expected: True")
    print(f"\n   Why: No cycle exists")
    print(f"   Valid order: Course 0 → Courses 1,2 → Course 3")

    # Test 3: Course Schedule with Cycle
    print("\n3. COURSE SCHEDULE - WITH CYCLE")
    print("-" * 70)
    test_prereqs_3 = [[1, 0], [0, 1]]
    print(f"   Prerequisites: {test_prereqs_3}")
    can_finish_cycle = can_finish_courses(2, test_prereqs_3)
    print(f"   Can finish all courses: {can_finish_cycle}")
    print(f"   Expected: False (cycle exists)")
    print(f"\n   Why: Course 1 requires 0, Course 0 requires 1 (circular!)")
    print(f"   Neither course ever reaches in-degree 0")

    # Test 4: Find Course Order
    print("\n4. FIND COURSE ORDER")
    print("-" * 70)
    test_prereqs_4 = [[1, 0], [2, 0], [3, 1], [3, 2]]
    print(f"   Prerequisites: {test_prereqs_4}")
    course_order = find_course_order(4, test_prereqs_4)
    print(f"   Valid order: {course_order}")
    print(f"   Expected: [0, 1, 2, 3] or [0, 2, 1, 3]")
    print(f"\n   Why: Returns actual ordering, not just yes/no")

    # Test 5: Cycle Detection
    print("\n5. CYCLE DETECTION")
    print("-" * 70)
    test_edges_5a = [[0, 1], [1, 2], [2, 0]]
    test_edges_5b = [[0, 1], [1, 2], [2, 3]]
    has_cycle_a = has_cycle_dfs(3, test_edges_5a)
    has_cycle_b = has_cycle_dfs(4, test_edges_5b)
    print(f"   Graph 0→1→2→0 has cycle: {has_cycle_a}")
    print(f"   Graph 0→1→2→3 has cycle: {has_cycle_b}")
    print(f"   Expected: True, False")
    print(f"\n   Why: First graph loops back to 0 (cycle)")
    print(f"   Second graph is linear (no cycle)")

    # Test 6: Alien Dictionary
    print("\n6. ALIEN DICTIONARY")
    print("-" * 70)
    test_words_6 = ["wrt", "wrf", "er", "ett", "rftt"]
    print(f"   Words: {test_words_6}")
    alien_order = alien_dictionary(test_words_6)
    print(f"   Character order: {alien_order}")
    print(f"   Expected: 'wertf' or similar valid ordering")
    print(f"\n   Why: Compare adjacent words to find character ordering")
    print(f"   wrt < wrf → t < f")
    print(f"   wrf < er → w < e")
    print(f"   er < ett → r < t")

    # Test 7: All Topological Orders
    print("\n7. ALL TOPOLOGICAL ORDERS (BACKTRACKING)")
    print("-" * 70)
    test_edges_7 = [[0, 1], [0, 2]]
    all_orders = all_topological_orders(3, test_edges_7)
    print(f"   Edges: {test_edges_7}")
    print(f"   All valid orders: {all_orders}")
    print(f"   Expected: [[0,1,2], [0,2,1]]")
    print(f"\n   Why: 0 must come first")
    print(f"   1 and 2 can be in any order (both only depend on 0)")

    # Test 8: Minimum Height Trees
    print("\n8. MINIMUM HEIGHT TREES")
    print("-" * 70)
    test_edges_8 = [[0, 1], [1, 2], [1, 3], [2, 4]]
    print(f"   Edges: {test_edges_8}")
    mht_roots = find_min_height_trees(5, test_edges_8)
    print(f"   Minimum height tree roots: {mht_roots}")
    print(f"   Expected: [1, 2]")
    print(f"\n   Why: Nodes 1 and 2 are centroids (center of tree)")
    print(f"   They minimize maximum distance to any leaf")

    # Summary
    print("\n" + "=" * 70)
    print("KEY INSIGHTS - TOPOLOGICAL SORT PATTERNS")
    print("=" * 70)
    print("1. Topological sort only works on Directed Acyclic Graphs (DAGs)")
    print("2. Kahn's algorithm: BFS with in-degrees, easy cycle detection")
    print("3. DFS approach: postorder traversal, then reverse result")
    print("4. In-degree 0 = no dependencies (can be processed first)")
    print("5. If can't process all vertices, cycle exists")
    print("6. Course schedule = topological sort + cycle detection")
    print("7. Multiple valid orderings possible (not unique)")
    print("8. Applications: build systems, task scheduling, course prereqs")
    print("9. Alien dictionary: compare adjacent words to build character graph")
    print("10. Minimum height trees: trim leaves to find centroids")
    print("=" * 70)


if __name__ == "__main__":
    run_tests()

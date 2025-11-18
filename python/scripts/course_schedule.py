"""
Course Schedule

There are a total of numCourses courses you have to take, labeled from 0 to numCourses - 1.
You are given an array prerequisites where prerequisites[i] = [ai, bi] indicates that you
must take course bi first if you want to take course ai.

Return true if you can finish all courses. Otherwise, return false.

LeetCode: https://leetcode.com/problems/course-schedule/
Difficulty: Medium
Pattern: Graph / Topological Sort / Cycle Detection
"""

from typing import List
from collections import deque, defaultdict


def can_finish_dfs(num_courses: int, prerequisites: List[List[int]]) -> bool:
    """
    DFS approach with cycle detection using three states.

    Time Complexity: O(V + E) where V = courses, E = prerequisites
    Space Complexity: O(V + E) for adjacency list and recursion

    Why it works:
    - Build directed graph from prerequisites
    - Use DFS to detect cycles
    - Three states: UNVISITED (0), VISITING (1), VISITED (2)
    - If we revisit a VISITING node, we found a cycle
    - Cycle means courses can't be completed
    """
    # Build adjacency list
    graph = defaultdict(list)
    for course, prereq in prerequisites:
        graph[course].append(prereq)

    # Three states for cycle detection
    UNVISITED = 0
    VISITING = 1
    VISITED = 2

    state = [UNVISITED] * num_courses

    def has_cycle(course: int) -> bool:
        """Returns True if cycle detected."""
        if state[course] == VISITING:
            # Found a cycle (back edge)
            return True

        if state[course] == VISITED:
            # Already processed, no cycle from here
            return False

        # Mark as currently visiting
        state[course] = VISITING

        # Check all prerequisites
        for prereq in graph[course]:
            if has_cycle(prereq):
                return True

        # Mark as done visiting
        state[course] = VISITED
        return False

    # Check each course for cycles
    for course in range(num_courses):
        if has_cycle(course):
            return False

    return True


def can_finish(num_courses: int, prerequisites: List[List[int]]) -> bool:
    """
    BFS approach using Kahn's algorithm (topological sort).

    Time Complexity: O(V + E)
    Space Complexity: O(V + E)

    Why it works:
    - Count incoming edges (prerequisites) for each course
    - Start with courses that have no prerequisites
    - Remove courses one by one, updating prerequisites count
    - If we can remove all courses, no cycle exists
    - If courses remain with prerequisites, there's a cycle
    """
    # Build adjacency list and count incoming edges
    graph = defaultdict(list)
    in_degree = [0] * num_courses

    for course, prereq in prerequisites:
        graph[prereq].append(course)  # prereq -> course
        in_degree[course] += 1

    # Start with courses that have no prerequisites
    queue = deque()
    for course in range(num_courses):
        if in_degree[course] == 0:
            queue.append(course)

    # Process courses in topological order
    courses_taken = 0

    while queue:
        course = queue.popleft()
        courses_taken += 1

        # Remove this course and update dependent courses
        for next_course in graph[course]:
            in_degree[next_course] -= 1

            # If all prerequisites met, can take this course
            if in_degree[next_course] == 0:
                queue.append(next_course)

    # If we took all courses, no cycle exists
    return courses_taken == num_courses


def can_finish_with_order(num_courses: int, prerequisites: List[List[int]]) -> dict:
    """
    Kahn's algorithm that also returns the course order.

    Returns dict with 'can_finish' boolean and 'order' list.
    """
    graph = defaultdict(list)
    in_degree = [0] * num_courses

    for course, prereq in prerequisites:
        graph[prereq].append(course)
        in_degree[course] += 1

    queue = deque()
    for course in range(num_courses):
        if in_degree[course] == 0:
            queue.append(course)

    order = []

    while queue:
        course = queue.popleft()
        order.append(course)

        for next_course in graph[course]:
            in_degree[next_course] -= 1
            if in_degree[next_course] == 0:
                queue.append(next_course)

    return {
        'can_finish': len(order) == num_courses,
        'order': order if len(order) == num_courses else [],
        'courses_completed': len(order)
    }


def find_cycle_dfs(num_courses: int, prerequisites: List[List[int]]) -> List[int]:
    """
    Find and return a cycle if one exists.

    Returns empty list if no cycle, otherwise returns cycle path.
    """
    graph = defaultdict(list)
    for course, prereq in prerequisites:
        graph[course].append(prereq)

    UNVISITED = 0
    VISITING = 1
    VISITED = 2

    state = [UNVISITED] * num_courses
    parent = {}
    cycle = []

    def dfs(course: int, path: list) -> bool:
        """Returns True if cycle found."""
        if state[course] == VISITING:
            # Found cycle, extract it
            cycle_start = course
            cycle.append(course)

            # Backtrack to find full cycle
            current = path[-1] if path else -1
            while current != cycle_start and current != -1:
                cycle.append(current)
                current = parent.get(current, -1)

            cycle.reverse()
            return True

        if state[course] == VISITED:
            return False

        state[course] = VISITING
        current_path = path + [course]

        for prereq in graph[course]:
            parent[prereq] = course
            if dfs(prereq, current_path):
                return True

        state[course] = VISITED
        return False

    for course in range(num_courses):
        if dfs(course, []):
            return cycle

    return []


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'num_courses': 2,
            'prerequisites': [[1, 0]],
            'expected': True,
            'description': 'Take course 0, then course 1'
        },
        {
            'num_courses': 2,
            'prerequisites': [[1, 0], [0, 1]],
            'expected': False,
            'description': 'Cycle: 0 requires 1, 1 requires 0'
        },
        {
            'num_courses': 4,
            'prerequisites': [[1, 0], [2, 0], [3, 1], [3, 2]],
            'expected': True,
            'description': 'Valid: 0 -> 1,2 -> 3'
        },
        {
            'num_courses': 3,
            'prerequisites': [[0, 1], [1, 2], [2, 0]],
            'expected': False,
            'description': 'Cycle: 0 -> 1 -> 2 -> 0'
        },
        {
            'num_courses': 1,
            'prerequisites': [],
            'expected': True,
            'description': 'Single course, no prerequisites'
        },
        {
            'num_courses': 5,
            'prerequisites': [[1, 4], [2, 4], [3, 1], [3, 2]],
            'expected': True,
            'description': 'DAG with multiple paths'
        }
    ]

    print("=" * 60)
    print("COURSE SCHEDULE - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = can_finish(test['num_courses'], test['prerequisites'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Courses: {test['num_courses']}")
        print(f"Prerequisites: {test['prerequisites']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")

        # Show course order or cycle
        details = can_finish_with_order(test['num_courses'], test['prerequisites'])
        if details['can_finish']:
            print(f"Valid order: {details['order']}")
        else:
            cycle = find_cycle_dfs(test['num_courses'], test['prerequisites'])
            if cycle:
                print(f"Cycle detected: {cycle}")

        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("DFS: Time O(V + E), Space O(V + E)")
    print("BFS (Kahn's): Time O(V + E), Space O(V + E)")
    print("\nKey Insight: Problem is cycle detection in directed graph")
    print("DFS: Use 3-state coloring (UNVISITED, VISITING, VISITED)")
    print("BFS: Use topological sort, count processed courses")

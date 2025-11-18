"""
10. Linked List Cycle

Given head, the head of a linked list, determine if the linked list has a cycle in it.
There is a cycle in a linked list if there is some node in the list that can be
reached again by continuously following the next pointer.

LeetCode: https://leetcode.com/problems/linked-list-cycle/
Difficulty: Easy
Pattern: Two Pointers (Fast & Slow), Floyd's Cycle Detection
"""

from typing import Optional, List


class ListNode:
    """Definition for singly-linked list."""
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next


def has_cycle_hash_set(head: Optional[ListNode]) -> bool:
    """
    Hash set approach: Track visited nodes.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    visited = set()
    current = head

    while current:
        if current in visited:
            return True
        visited.add(current)
        current = current.next

    return False


def has_cycle(head: Optional[ListNode]) -> bool:
    """
    Floyd's Cycle Detection (Tortoise and Hare).
    Two pointers: slow moves 1 step, fast moves 2 steps.
    If there's a cycle, they will eventually meet.

    Time Complexity: O(n)
    Space Complexity: O(1)
    """
    if not head or not head.next:
        return False

    slow = head
    fast = head.next

    while slow != fast:
        if not fast or not fast.next:
            return False
        slow = slow.next
        fast = fast.next.next

    return True


def detect_cycle_start(head: Optional[ListNode]) -> Optional[ListNode]:
    """
    Detect where the cycle begins (if it exists).
    Extension of Floyd's algorithm.

    Time Complexity: O(n)
    Space Complexity: O(1)
    """
    if not head or not head.next:
        return None

    # Find meeting point
    slow = fast = head
    has_cycle = False

    while fast and fast.next:
        slow = slow.next
        fast = fast.next.next
        if slow == fast:
            has_cycle = True
            break

    if not has_cycle:
        return None

    # Find cycle start
    slow = head
    while slow != fast:
        slow = slow.next
        fast = fast.next

    return slow


# Helper Functions
def create_linked_list_with_cycle(values: List[int], pos: int) -> Optional[ListNode]:
    """
    Create linked list with cycle.
    pos: index where cycle starts (-1 for no cycle)
    """
    if not values:
        return None

    nodes = [ListNode(val) for val in values]

    for i in range(len(nodes) - 1):
        nodes[i].next = nodes[i + 1]

    if pos >= 0:
        nodes[-1].next = nodes[pos]

    return nodes[0]


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'values': [3, 2, 0, -4],
            'pos': 1,
            'expected': True,
            'description': 'Cycle at position 1'
        },
        {
            'values': [1, 2],
            'pos': 0,
            'expected': True,
            'description': 'Cycle at position 0'
        },
        {
            'values': [1],
            'pos': -1,
            'expected': False,
            'description': 'Single node, no cycle'
        },
        {
            'values': [1, 2, 3, 4, 5],
            'pos': -1,
            'expected': False,
            'description': 'No cycle'
        },
        {
            'values': [1, 2, 3, 4, 5],
            'pos': 2,
            'expected': True,
            'description': 'Cycle in middle'
        }
    ]

    print("=" * 60)
    print("LINKED LIST CYCLE - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        head = create_linked_list_with_cycle(test['values'], test['pos'])

        result_floyd = has_cycle(head)
        passed = result_floyd == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Values: {test['values']}")
        print(f"Cycle Position: {test['pos']}")
        print(f"Expected: {test['expected']}")
        print(f"Got (Floyd's): {result_floyd}")

        if test['expected']:
            cycle_start = detect_cycle_start(head)
            if cycle_start and test['pos'] >= 0:
                print(f"Cycle starts at node with value: {cycle_start.val}")
                print(f"Expected value: {test['values'][test['pos']]}")

        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("FLOYD'S CYCLE DETECTION ALGORITHM")
    print("=" * 60)
    print("""
Algorithm (Tortoise and Hare):
1. Use two pointers: slow and fast
2. slow moves 1 step at a time
3. fast moves 2 steps at a time
4. If they meet, there's a cycle
5. If fast reaches null, no cycle

Why it works:
- In a cycle, fast will eventually catch up to slow
- The distance between them decreases by 1 each iteration
- They will meet within the cycle

To find cycle start:
1. After detecting cycle, reset slow to head
2. Move both slow and fast one step at a time
3. Where they meet is the cycle start
""")

    print("\n" + "=" * 60)
    print("COMPLEXITY COMPARISON")
    print("=" * 60)
    print("Hash Set: Time O(n), Space O(n)")
    print("Floyd's: Time O(n), Space O(1) - OPTIMAL")

"""
5. Reverse Linked List

Given the head of a singly linked list, reverse the list, and return the reversed list.

LeetCode: https://leetcode.com/problems/reverse-linked-list/
Difficulty: Easy
Pattern: Linked List
"""

from typing import Optional, List


class ListNode:
    """Definition for singly-linked list."""
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next


def reverse_list_iterative(head: Optional[ListNode]) -> Optional[ListNode]:
    """
    Iterative approach with three pointers.

    Time Complexity: O(n)
    Space Complexity: O(1)
    """
    prev = None
    current = head

    while current:
        next_temp = current.next  # Save next
        current.next = prev       # Reverse link
        prev = current            # Move prev forward
        current = next_temp       # Move current forward

    return prev


def reverse_list_recursive(head: Optional[ListNode]) -> Optional[ListNode]:
    """
    Recursive approach.

    Time Complexity: O(n)
    Space Complexity: O(n) due to recursion stack
    """
    # Base case
    if not head or not head.next:
        return head

    # Reverse the rest of the list
    reversed_head = reverse_list_recursive(head.next)

    # Put head at the end
    head.next.next = head
    head.next = None

    return reversed_head


# Alias for optimal solution
reverse_list = reverse_list_iterative


# Helper Functions
def create_linked_list(values: List[int]) -> Optional[ListNode]:
    """Create a linked list from a list of values."""
    if not values:
        return None

    head = ListNode(values[0])
    current = head

    for val in values[1:]:
        current.next = ListNode(val)
        current = current.next

    return head


def linked_list_to_list(head: Optional[ListNode]) -> List[int]:
    """Convert linked list to Python list."""
    result = []
    current = head

    while current:
        result.append(current.val)
        current = current.next

    return result


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'input': [1, 2, 3, 4, 5],
            'expected': [5, 4, 3, 2, 1],
            'description': 'Normal case'
        },
        {
            'input': [1, 2],
            'expected': [2, 1],
            'description': 'Two nodes'
        },
        {
            'input': [],
            'expected': [],
            'description': 'Empty list'
        },
        {
            'input': [1],
            'expected': [1],
            'description': 'Single node'
        },
        {
            'input': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
            'expected': [10, 9, 8, 7, 6, 5, 4, 3, 2, 1],
            'description': 'Longer list'
        }
    ]

    print("=" * 60)
    print("REVERSE LINKED LIST - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        # Test iterative
        head = create_linked_list(test['input'])
        reversed_head = reverse_list_iterative(head)
        result = linked_list_to_list(reversed_head)
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['input']}")
        print(f"Expected: {test['expected']}")
        print(f"Got (Iterative): {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

        # Test recursive
        head = create_linked_list(test['input'])
        reversed_head = reverse_list_recursive(head)
        result = linked_list_to_list(reversed_head)
        print(f"Got (Recursive): {result}")

    print("\n" + "=" * 60)
    print("APPROACH COMPARISON")
    print("=" * 60)
    print("Iterative: Time O(n), Space O(1)")
    print("Recursive: Time O(n), Space O(n)")
    print("\nIterative Approach Steps:")
    print("1. Use three pointers: prev, current, next")
    print("2. While current exists:")
    print("   - Save next node")
    print("   - Reverse current's pointer")
    print("   - Move pointers forward")
    print("3. Return prev (new head)")

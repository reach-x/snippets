"""
4. Merge Two Sorted Lists

Merge two sorted linked lists and return it as a sorted list. The list should be
made by splicing together the nodes of the first two lists.

LeetCode: https://leetcode.com/problems/merge-two-sorted-lists/
Difficulty: Easy
Pattern: Linked List, Two Pointers
"""

from typing import Optional, List


class ListNode:
    """Definition for singly-linked list."""
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next


def merge_two_lists_iterative(list1: Optional[ListNode], list2: Optional[ListNode]) -> Optional[ListNode]:
    """
    Iterative approach with dummy node.

    Time Complexity: O(n + m)
    Space Complexity: O(1)
    """
    dummy = ListNode(0)
    current = dummy

    while list1 and list2:
        if list1.val <= list2.val:
            current.next = list1
            list1 = list1.next
        else:
            current.next = list2
            list2 = list2.next
        current = current.next

    # Append remaining nodes
    current.next = list1 if list1 else list2

    return dummy.next


def merge_two_lists_recursive(list1: Optional[ListNode], list2: Optional[ListNode]) -> Optional[ListNode]:
    """
    Recursive approach.

    Time Complexity: O(n + m)
    Space Complexity: O(n + m) due to recursion stack
    """
    if not list1:
        return list2
    if not list2:
        return list1

    if list1.val <= list2.val:
        list1.next = merge_two_lists_recursive(list1.next, list2)
        return list1
    else:
        list2.next = merge_two_lists_recursive(list1, list2.next)
        return list2


# Alias for the optimal solution
merge_two_lists = merge_two_lists_iterative


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


def print_linked_list(head: Optional[ListNode]) -> str:
    """Convert linked list to string representation."""
    if not head:
        return "[]"
    return str(linked_list_to_list(head))


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'list1': [1, 2, 4],
            'list2': [1, 3, 4],
            'expected': [1, 1, 2, 3, 4, 4],
            'description': 'Basic merge'
        },
        {
            'list1': [],
            'list2': [],
            'expected': [],
            'description': 'Both empty'
        },
        {
            'list1': [],
            'list2': [0],
            'expected': [0],
            'description': 'First empty'
        },
        {
            'list1': [1, 2, 3],
            'list2': [4, 5, 6],
            'expected': [1, 2, 3, 4, 5, 6],
            'description': 'No overlap'
        },
        {
            'list1': [5],
            'list2': [1, 2, 4],
            'expected': [1, 2, 4, 5],
            'description': 'Single node vs multiple'
        },
        {
            'list1': [-10, -5, 0],
            'list2': [-8, -3, 2],
            'expected': [-10, -8, -5, -3, 0, 2],
            'description': 'Negative numbers'
        }
    ]

    print("=" * 60)
    print("MERGE TWO SORTED LISTS - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        l1 = create_linked_list(test['list1'])
        l2 = create_linked_list(test['list2'])
        merged = merge_two_lists(l1, l2)
        result = linked_list_to_list(merged)
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"List 1: {test['list1']}")
        print(f"List 2: {test['list2']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("APPROACH COMPARISON")
    print("=" * 60)
    print("Iterative: Time O(n+m), Space O(1)")
    print("Recursive: Time O(n+m), Space O(n+m)")
    print("Iterative is preferred for space efficiency")

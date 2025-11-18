# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next

class Solution:
    def addTwoNumbers(self, l1: ListNode, l2: ListNode) -> ListNode:
        """
        Add two numbers represented by linked lists.
        Digits are stored in reverse order.

        Example:
        342 is stored as 2 -> 4 -> 3
        465 is stored as 5 -> 6 -> 4
        Result (807) is returned as 7 -> 0 -> 8
        """

        # Dummy head makes building the result list easier.
        # We return dummy_head.next at the end.
        dummy_head = ListNode()
        current = dummy_head

        carry = 0  # Stores any carry-over to the next digit

        # Continue while digits remain in l1 or l2, or a carry exists
        while l1 is not None or l2 is not None or carry != 0:

            # Get values (0 if we've run past the end of the list)
            value1 = l1.val if l1 is not None else 0
            value2 = l2.val if l2 is not None else 0

            # Compute total and carry
            total = value1 + value2 + carry
            new_digit = total % 10
            carry = total // 10

            # Create and advance through the result list
            current.next = ListNode(new_digit)
            current = current.next

            # Advance each input list if possible
            if l1 is not None:
                l1 = l1.next

            if l2 is not None:
                l2 = l2.next

        return dummy_head.next


#!/usr/bin/env python3

class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

class LinkedList:
    def __init__(self):
        self.head = None

    def append(self, val):
        """Add node to end of list"""
        new_node = ListNode(val)
        if not self.head:
            self.head = new_node
            return

        current = self.head
        while current.next:
            current = current.next
        current.next = new_node

    def prepend(self, val):
        """Add node to beginning of list"""
        new_node = ListNode(val)
        new_node.next = self.head
        self.head = new_node

    def delete(self, val):
        """Delete first node with given value"""
        if not self.head:
            return

        if self.head.val == val:
            self.head = self.head.next
            return

        current = self.head
        while current.next:
            if current.next.val == val:
                current.next = current.next.next
                return
            current = current.next

    def reverse(self):
        """Reverse the linked list"""
        prev = None
        current = self.head

        while current:
            next_node = current.next
            current.next = prev
            prev = current
            current = next_node

        self.head = prev

    def to_list(self):
        """Convert to Python list for display"""
        result = []
        current = self.head
        while current:
            result.append(current.val)
            current = current.next
        return result

if __name__ == '__main__':
    ll = LinkedList()

    print("Appending values 1-5:")
    for i in range(1, 6):
        ll.append(i)
    print(ll.to_list())

    print("\nPrepending 0:")
    ll.prepend(0)
    print(ll.to_list())

    print("\nDeleting 3:")
    ll.delete(3)
    print(ll.to_list())

    print("\nReversing list:")
    ll.reverse()
    print(ll.to_list())

"""
Min Stack

Design a stack that supports push, pop, top, and retrieving the minimum element
in constant time.

Implement the MinStack class:
- MinStack() initializes the stack object.
- void push(int val) pushes the element val onto the stack.
- void pop() removes the element on the top of the stack.
- int top() gets the top element of the stack.
- int getMin() retrieves the minimum element in the stack.

You must implement a solution with O(1) time complexity for each function.

LeetCode: https://leetcode.com/problems/min-stack/
Difficulty: Medium
Pattern: Stack / Design
"""


class MinStackTwoStacks:
    """
    Two stacks approach: One for values, one for minimums.

    Space Complexity: O(n) - in worst case, min_stack has same size as main stack
    """

    def __init__(self):
        self.stack = []  # Main stack
        self.min_stack = []  # Stack to track minimums

    def push(self, val: int) -> None:
        """Push value onto stack. Time: O(1)"""
        self.stack.append(val)

        # Push to min_stack if it's empty or val is new minimum
        if not self.min_stack or val <= self.min_stack[-1]:
            self.min_stack.append(val)

    def pop(self) -> None:
        """Remove top element. Time: O(1)"""
        if not self.stack:
            return

        val = self.stack.pop()

        # If popped value was the minimum, remove from min_stack too
        if val == self.min_stack[-1]:
            self.min_stack.pop()

    def top(self) -> int:
        """Get top element. Time: O(1)"""
        return self.stack[-1] if self.stack else None

    def get_min(self) -> int:
        """Get minimum element. Time: O(1)"""
        return self.min_stack[-1] if self.min_stack else None


class MinStack:
    """
    Single stack approach: Store (value, current_min) tuples.

    Space Complexity: O(n) - but more space efficient than two stacks

    Why it works:
    - Each element stores the minimum value at the time it was pushed
    - When we pop, we automatically have the previous minimum
    - All operations are O(1)
    """

    def __init__(self):
        self.stack = []  # Stack of (value, min_at_this_point) tuples

    def push(self, val: int) -> None:
        """Push value onto stack. Time: O(1)"""
        # Calculate minimum at this point
        if not self.stack:
            current_min = val
        else:
            current_min = min(val, self.stack[-1][1])

        self.stack.append((val, current_min))

    def pop(self) -> None:
        """Remove top element. Time: O(1)"""
        if self.stack:
            self.stack.pop()

    def top(self) -> int:
        """Get top element. Time: O(1)"""
        return self.stack[-1][0] if self.stack else None

    def get_min(self) -> int:
        """Get minimum element. Time: O(1)"""
        return self.stack[-1][1] if self.stack else None


class MinStackLinkedList:
    """
    Linked list approach: Each node stores value and minimum.

    Useful for environments where arrays/lists have size limitations.
    """

    class Node:
        def __init__(self, val: int, min_val: int, next_node=None):
            self.val = val
            self.min = min_val
            self.next = next_node

    def __init__(self):
        self.head = None

    def push(self, val: int) -> None:
        """Push value onto stack. Time: O(1)"""
        if not self.head:
            self.head = self.Node(val, val)
        else:
            current_min = min(val, self.head.min)
            self.head = self.Node(val, current_min, self.head)

    def pop(self) -> None:
        """Remove top element. Time: O(1)"""
        if self.head:
            self.head = self.head.next

    def top(self) -> int:
        """Get top element. Time: O(1)"""
        return self.head.val if self.head else None

    def get_min(self) -> int:
        """Get minimum element. Time: O(1)"""
        return self.head.min if self.head else None


# Test Cases
if __name__ == "__main__":
    print("=" * 60)
    print("MIN STACK - TEST RESULTS")
    print("=" * 60)

    # Test Case 1: Basic operations
    print("\nTest 1: Basic operations")
    min_stack = MinStack()
    min_stack.push(-2)
    min_stack.push(0)
    min_stack.push(-3)
    print(f"After push(-2), push(0), push(-3)")
    print(f"getMin() = {min_stack.get_min()}, expected: -3")
    print(f"Status: {'PASS' if min_stack.get_min() == -3 else 'FAIL'}")

    min_stack.pop()
    print(f"\nAfter pop()")
    print(f"top() = {min_stack.top()}, expected: 0")
    print(f"getMin() = {min_stack.get_min()}, expected: -2")
    print(f"Status: {'PASS' if min_stack.top() == 0 and min_stack.get_min() == -2 else 'FAIL'}")

    # Test Case 2: Minimum changes
    print("\n" + "-" * 60)
    print("Test 2: Minimum changes over time")
    min_stack = MinStack()
    operations = [
        ('push', 5),
        ('push', 3),
        ('push', 7),
        ('push', 1),
    ]

    expected_mins = [5, 3, 3, 1]

    for i, (op, val) in enumerate(operations):
        min_stack.push(val)
        current_min = min_stack.get_min()
        expected = expected_mins[i]
        print(f"After push({val}): getMin() = {current_min}, expected: {expected}")
        print(f"Status: {'PASS' if current_min == expected else 'FAIL'}")

    # Test Case 3: All same values
    print("\n" + "-" * 60)
    print("Test 3: All same values")
    min_stack = MinStack()
    for _ in range(3):
        min_stack.push(5)

    print(f"After push(5) × 3: getMin() = {min_stack.get_min()}, expected: 5")
    print(f"Status: {'PASS' if min_stack.get_min() == 5 else 'FAIL'}")

    min_stack.pop()
    print(f"After pop(): getMin() = {min_stack.get_min()}, expected: 5")
    print(f"Status: {'PASS' if min_stack.get_min() == 5 else 'FAIL'}")

    # Test Case 4: Negative numbers
    print("\n" + "-" * 60)
    print("Test 4: Negative numbers")
    min_stack = MinStack()
    min_stack.push(-1)
    min_stack.push(-5)
    min_stack.push(-2)

    print(f"After push(-1), push(-5), push(-2)")
    print(f"getMin() = {min_stack.get_min()}, expected: -5")
    print(f"Status: {'PASS' if min_stack.get_min() == -5 else 'FAIL'}")

    min_stack.pop()
    print(f"After pop(): getMin() = {min_stack.get_min()}, expected: -5")
    print(f"Status: {'PASS' if min_stack.get_min() == -5 else 'FAIL'}")

    min_stack.pop()
    print(f"After pop(): getMin() = {min_stack.get_min()}, expected: -1")
    print(f"Status: {'PASS' if min_stack.get_min() == -1 else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("All operations: Time O(1), Space O(n)")
    print("\nKey Insight: Store minimum along with each element")
    print("to avoid recalculating when minimum is popped")

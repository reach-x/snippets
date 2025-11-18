"""
3. Valid Parentheses

Given a string s containing just the characters '(', ')', '{', '}', '[' and ']',
determine if the input string is valid. An input string is valid if:
1. Open brackets must be closed by the same type of brackets.
2. Open brackets must be closed in the correct order.
3. Every close bracket has a corresponding open bracket of the same type.

LeetCode: https://leetcode.com/problems/valid-parentheses/
Difficulty: Easy
Pattern: Stack
"""


def is_valid(s: str) -> bool:
    """
    Use stack to match opening and closing brackets.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    stack = []
    matching = {')': '(', '}': '{', ']': '['}

    for char in s:
        if char in matching:
            # Closing bracket
            if not stack or stack[-1] != matching[char]:
                return False
            stack.pop()
        else:
            # Opening bracket
            stack.append(char)

    return len(stack) == 0


def is_valid_with_details(s: str) -> dict:
    """
    Returns detailed validation information.
    """
    stack = []
    matching = {')': '(', '}': '{', ']': '['}
    steps = []

    for i, char in enumerate(s):
        if char in matching:
            if not stack:
                return {
                    'valid': False,
                    'error': f'Closing bracket "{char}" at position {i} has no matching opening bracket',
                    'steps': steps
                }
            if stack[-1] != matching[char]:
                return {
                    'valid': False,
                    'error': f'Mismatched brackets: found "{char}" at position {i}, expected closing for "{stack[-1]}"',
                    'steps': steps
                }
            popped = stack.pop()
            steps.append(f'Position {i}: Matched "{char}" with "{popped}"')
        else:
            stack.append(char)
            steps.append(f'Position {i}: Pushed "{char}"')

    if stack:
        return {
            'valid': False,
            'error': f'Unclosed brackets: {stack}',
            'steps': steps
        }

    return {
        'valid': True,
        'error': None,
        'steps': steps
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'input': "()",
            'expected': True,
            'description': 'Simple valid case'
        },
        {
            'input': "()[]{}",
            'expected': True,
            'description': 'Multiple types valid'
        },
        {
            'input': "(]",
            'expected': False,
            'description': 'Mismatched brackets'
        },
        {
            'input': "([)]",
            'expected': False,
            'description': 'Wrong order'
        },
        {
            'input': "{[]}",
            'expected': True,
            'description': 'Nested brackets valid'
        },
        {
            'input': "",
            'expected': True,
            'description': 'Empty string'
        },
        {
            'input': "(((",
            'expected': False,
            'description': 'Only opening brackets'
        },
        {
            'input': ")))",
            'expected': False,
            'description': 'Only closing brackets'
        },
        {
            'input': "(){}}{",
            'expected': False,
            'description': 'Extra brackets'
        }
    ]

    print("=" * 60)
    print("VALID PARENTHESES - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = is_valid(test['input'])
        details = is_valid_with_details(test['input'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: \"{test['input']}\"")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

        if not details['valid']:
            print(f"Error: {details['error']}")
        else:
            print(f"Valid: All brackets matched correctly")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Time Complexity: O(n) - single pass through string")
    print("Space Complexity: O(n) - stack storage in worst case")

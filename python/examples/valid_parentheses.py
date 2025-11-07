#!/usr/bin/env python3

def is_valid_parentheses(s):
    """Check if string has valid parentheses/brackets"""
    stack = []
    mapping = {')': '(', '}': '{', ']': '['}

    for char in s:
        if char in mapping:
            top_element = stack.pop() if stack else '#'
            if mapping[char] != top_element:
                return False
        else:
            stack.append(char)

    return not stack

if __name__ == '__main__':
    test_cases = [
        "()",
        "()[]{}",
        "(]",
        "([)]",
        "{[]}",
        "((()))",
        "({[()]})"
    ]

    for test in test_cases:
        result = is_valid_parentheses(test)
        print(f"'{test}' is valid: {result}")

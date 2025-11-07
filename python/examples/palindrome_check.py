#!/usr/bin/env python3

def is_palindrome(s):
    """Check if string is a palindrome"""
    cleaned = ''.join(c.lower() for c in s if c.isalnum())
    return cleaned == cleaned[::-1]

def is_palindrome_two_pointer(s):
    """Check palindrome using two pointers"""
    cleaned = ''.join(c.lower() for c in s if c.isalnum())
    left, right = 0, len(cleaned) - 1

    while left < right:
        if cleaned[left] != cleaned[right]:
            return False
        left += 1
        right -= 1

    return True

if __name__ == '__main__':
    test_cases = [
        "racecar",
        "A man, a plan, a canal: Panama",
        "race a car",
        "Was it a car or a cat I saw?",
        "hello"
    ]

    for test in test_cases:
        result = is_palindrome(test)
        print(f"'{test}' is palindrome: {result}")

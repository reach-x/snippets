"""
2. Longest Substring Without Repeating Characters

Given a string s, find the length of the longest substring without repeating characters.

LeetCode: https://leetcode.com/problems/longest-substring-without-repeating-characters/
Difficulty: Medium
Pattern: Sliding Window
"""


def length_of_longest_substring_brute_force(s: str) -> int:
    """
    Brute force: Check all substrings.

    Time Complexity: O(n^3)
    Space Complexity: O(min(n, m)) where m is charset size
    """
    max_length = 0

    for i in range(len(s)):
        for j in range(i + 1, len(s) + 1):
            substring = s[i:j]
            if len(substring) == len(set(substring)):
                max_length = max(max_length, len(substring))

    return max_length


def length_of_longest_substring(s: str) -> int:
    """
    Optimal: Sliding window with hash map.

    Time Complexity: O(n)
    Space Complexity: O(min(n, m)) where m is charset size
    """
    char_index = {}
    max_length = 0
    left = 0

    for right, char in enumerate(s):
        # If char is in window, move left pointer
        if char in char_index and char_index[char] >= left:
            left = char_index[char] + 1

        char_index[char] = right
        max_length = max(max_length, right - left + 1)

    return max_length


def longest_substring_with_details(s: str) -> dict:
    """
    Returns the longest substring and its details.
    """
    char_index = {}
    max_length = 0
    left = 0
    max_left = 0
    max_right = 0

    for right, char in enumerate(s):
        if char in char_index and char_index[char] >= left:
            left = char_index[char] + 1

        char_index[char] = right
        current_length = right - left + 1

        if current_length > max_length:
            max_length = current_length
            max_left = left
            max_right = right

    return {
        'length': max_length,
        'substring': s[max_left:max_right + 1] if max_length > 0 else '',
        'start_index': max_left,
        'end_index': max_right
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'input': "abcabcbb",
            'expected': 3,
            'expected_substring': "abc",
            'description': 'Repeating pattern'
        },
        {
            'input': "bbbbb",
            'expected': 1,
            'expected_substring': "b",
            'description': 'All same characters'
        },
        {
            'input': "pwwkew",
            'expected': 3,
            'expected_substring': "wke",
            'description': 'Mixed pattern'
        },
        {
            'input': "",
            'expected': 0,
            'expected_substring': "",
            'description': 'Empty string'
        },
        {
            'input': "abcdef",
            'expected': 6,
            'expected_substring': "abcdef",
            'description': 'All unique characters'
        },
        {
            'input': "dvdf",
            'expected': 3,
            'expected_substring': "vdf",
            'description': 'Tricky case'
        },
        {
            'input': "tmmzuxt",
            'expected': 5,
            'expected_substring': "mzuxt",
            'description': 'Another tricky case'
        }
    ]

    print("=" * 70)
    print("LONGEST SUBSTRING WITHOUT REPEATING CHARACTERS - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        result = length_of_longest_substring(test['input'])
        details = longest_substring_with_details(test['input'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: \"{test['input']}\"")
        print(f"Expected Length: {test['expected']}")
        print(f"Got Length: {result}")
        print(f"Longest Substring: \"{details['substring']}\"")
        print(f"Position: [{details['start_index']}:{details['end_index']}]")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("COMPLEXITY ANALYSIS")
    print("=" * 70)
    print("Brute Force: Time O(n^3), Space O(min(n, m))")
    print("Sliding Window (Optimal): Time O(n), Space O(min(n, m))")
    print("where n is string length, m is charset size")

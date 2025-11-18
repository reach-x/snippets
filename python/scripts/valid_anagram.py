"""
Valid Anagram

Given two strings s and t, return true if t is an anagram of s, and false otherwise.
An anagram is a word or phrase formed by rearranging the letters of a different word
or phrase, typically using all the original letters exactly once.

LeetCode: https://leetcode.com/problems/valid-anagram/
Difficulty: Easy
Pattern: Hash Map / Sorting
"""

from collections import Counter


def is_anagram_sorting(s: str, t: str) -> bool:
    """
    Sorting approach: Sort both strings and compare.

    Time Complexity: O(n log n) where n is length of string
    Space Complexity: O(1) or O(n) depending on sorting algorithm
    """
    # Different lengths can't be anagrams
    if len(s) != len(t):
        return False

    return sorted(s) == sorted(t)


def is_anagram_hash_map(s: str, t: str) -> bool:
    """
    Hash map approach: Count frequency of each character.

    Time Complexity: O(n)
    Space Complexity: O(1) - at most 26 letters in English alphabet
    """
    if len(s) != len(t):
        return False

    # Count character frequencies
    char_count = {}

    # Increment for s
    for char in s:
        char_count[char] = char_count.get(char, 0) + 1

    # Decrement for t
    for char in t:
        char_count[char] = char_count.get(char, 0) - 1

    # All counts should be zero
    return all(count == 0 for count in char_count.values())


def is_anagram(s: str, t: str) -> bool:
    """
    Optimal approach using Counter from collections.

    Time Complexity: O(n)
    Space Complexity: O(1) - bounded by alphabet size
    """
    if len(s) != len(t):
        return False

    return Counter(s) == Counter(t)


def is_anagram_array(s: str, t: str) -> bool:
    """
    Array approach: Use fixed-size array for lowercase English letters.
    Only works if input is guaranteed to be lowercase a-z.

    Time Complexity: O(n)
    Space Complexity: O(1) - fixed array of 26 elements
    """
    if len(s) != len(t):
        return False

    # Array to count each letter (a-z)
    counts = [0] * 26

    for char in s:
        counts[ord(char) - ord('a')] += 1

    for char in t:
        counts[ord(char) - ord('a')] -= 1

    return all(count == 0 for count in counts)


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            's': 'anagram',
            't': 'nagaram',
            'expected': True,
            'description': 'Valid anagram'
        },
        {
            's': 'rat',
            't': 'car',
            'expected': False,
            'description': 'Not an anagram'
        },
        {
            's': 'listen',
            't': 'silent',
            'expected': True,
            'description': 'Valid anagram (different arrangement)'
        },
        {
            's': 'hello',
            't': 'world',
            'expected': False,
            'description': 'Completely different letters'
        },
        {
            's': 'a',
            't': 'a',
            'expected': True,
            'description': 'Single character match'
        },
        {
            's': '',
            't': '',
            'expected': True,
            'description': 'Empty strings'
        },
        {
            's': 'abc',
            't': 'abcd',
            'expected': False,
            'description': 'Different lengths'
        },
        {
            's': 'aabbcc',
            't': 'abcabc',
            'expected': True,
            'description': 'Repeated characters'
        }
    ]

    print("=" * 60)
    print("VALID ANAGRAM - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = is_anagram(test['s'], test['t'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: s = '{test['s']}', t = '{test['t']}'")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Sorting: Time O(n log n), Space O(1) to O(n)")
    print("Hash Map: Time O(n), Space O(1) - max 26 chars")
    print("Counter (Optimal): Time O(n), Space O(1)")
    print("Array: Time O(n), Space O(1) - only for lowercase a-z")

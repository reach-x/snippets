"""
Group Anagrams

Given an array of strings strs, group the anagrams together. You can return the
answer in any order.

An anagram is a word or phrase formed by rearranging the letters of a different
word or phrase, typically using all the original letters exactly once.

LeetCode: https://leetcode.com/problems/group-anagrams/
Difficulty: Medium
Pattern: Hash Map
"""

from typing import List
from collections import defaultdict


def group_anagrams_sorting(strs: List[str]) -> List[List[str]]:
    """
    Sorting approach: Use sorted string as key.

    Time Complexity: O(n * k log k) where n = number of strings, k = max length
    Space Complexity: O(n * k) to store the result
    """
    anagram_groups = defaultdict(list)

    for string in strs:
        # Sort the string to use as key
        # Anagrams will have the same sorted representation
        sorted_string = ''.join(sorted(string))
        anagram_groups[sorted_string].append(string)

    return list(anagram_groups.values())


def group_anagrams(strs: List[str]) -> List[List[str]]:
    """
    Optimal approach: Use character count as key (tuple of counts).

    Time Complexity: O(n * k) where n = number of strings, k = max length
    Space Complexity: O(n * k)

    Why it works:
    - Anagrams have identical character frequencies
    - We create a tuple of 26 counts (a-z) as the hash key
    - Tuples are hashable, lists are not
    - This avoids the O(k log k) sorting cost
    """
    anagram_groups = defaultdict(list)

    for string in strs:
        # Count frequency of each character (a-z)
        char_count = [0] * 26

        for char in string:
            char_count[ord(char) - ord('a')] += 1

        # Use tuple as dictionary key (lists aren't hashable)
        key = tuple(char_count)
        anagram_groups[key].append(string)

    return list(anagram_groups.values())


def group_anagrams_prime(strs: List[str]) -> List[List[str]]:
    """
    Prime number approach: Assign each letter a unique prime, multiply them.

    Time Complexity: O(n * k)
    Space Complexity: O(n * k)

    Note: Can overflow with long strings, but elegant mathematical approach
    """
    # Assign first 26 primes to letters a-z
    primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59,
              61, 67, 71, 73, 79, 83, 89, 97, 101]

    anagram_groups = defaultdict(list)

    for string in strs:
        # Calculate product of primes
        key = 1
        for char in string:
            key *= primes[ord(char) - ord('a')]

        anagram_groups[key].append(string)

    return list(anagram_groups.values())


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'strs': ['eat', 'tea', 'tan', 'ate', 'nat', 'bat'],
            'expected': [['eat', 'tea', 'ate'], ['tan', 'nat'], ['bat']],
            'description': 'Multiple anagram groups'
        },
        {
            'strs': [''],
            'expected': [['']],
            'description': 'Single empty string'
        },
        {
            'strs': ['a'],
            'expected': [['a']],
            'description': 'Single character'
        },
        {
            'strs': ['abc', 'def', 'ghi'],
            'expected': [['abc'], ['def'], ['ghi']],
            'description': 'No anagrams'
        },
        {
            'strs': ['listen', 'silent', 'enlist'],
            'expected': [['listen', 'silent', 'enlist']],
            'description': 'All are anagrams'
        }
    ]

    print("=" * 60)
    print("GROUP ANAGRAMS - TEST RESULTS")
    print("=" * 60)

    def normalize_result(result):
        """Sort result for comparison (order doesn't matter)"""
        return sorted([sorted(group) for group in result])

    for i, test in enumerate(test_cases, 1):
        result = group_anagrams(test['strs'])
        normalized_result = normalize_result(result)
        normalized_expected = normalize_result(test['expected'])
        passed = normalized_result == normalized_expected

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: {test['strs']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Sorting: Time O(n * k log k), Space O(n * k)")
    print("Character Count (Optimal): Time O(n * k), Space O(n * k)")
    print("Prime Numbers: Time O(n * k), Space O(n * k)")
    print("\nwhere n = number of strings, k = max string length")

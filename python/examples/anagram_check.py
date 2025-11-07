#!/usr/bin/env python3

from collections import Counter

def is_anagram(s1, s2):
    """Check if two strings are anagrams"""
    return Counter(s1.lower()) == Counter(s2.lower())

def is_anagram_sorted(s1, s2):
    """Check using sorting"""
    return sorted(s1.lower()) == sorted(s2.lower())

def group_anagrams(words):
    """Group words that are anagrams of each other"""
    anagram_groups = {}

    for word in words:
        sorted_word = ''.join(sorted(word.lower()))
        if sorted_word not in anagram_groups:
            anagram_groups[sorted_word] = []
        anagram_groups[sorted_word].append(word)

    return list(anagram_groups.values())

if __name__ == '__main__':
    print("Anagram check:")
    pairs = [
        ("listen", "silent"),
        ("hello", "world"),
        ("triangle", "integral"),
        ("apple", "ppale")
    ]

    for s1, s2 in pairs:
        result = is_anagram(s1, s2)
        print(f"'{s1}' and '{s2}' are anagrams: {result}")

    print("\nGroup anagrams:")
    words = ["eat", "tea", "tan", "ate", "nat", "bat"]
    groups = group_anagrams(words)
    for group in groups:
        print(group)

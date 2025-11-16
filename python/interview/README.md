# Python Interview Preparation Guide

Complete Python interview preparation materials organized by topic. All files are designed to be greppable for quick reference during interview prep.

## Files Overview

### 1. data_structures.py
**Topics**: Lists, Tuples, Dictionaries, Sets, Strings

Common questions and operations for Python's core data structures:
- List operations (append, extend, insert, remove, slice, comprehensions)
- Tuple basics and unpacking
- Dictionary methods (get, items, keys, values, comprehensions)
- Set operations (union, intersection, difference)
- String manipulation (split, join, format, strip, replace)

**Grep examples**:
```bash
grep -n "How do you reverse a list" data_structures.py
grep -n "dictionary comprehension" data_structures.py
grep -n "set operations" data_structures.py
```

### 2. algorithms.py
**Topics**: Sorting, Searching, Recursion, Two Pointers, Sliding Window

Common algorithmic patterns and implementations:
- Sorting algorithms (bubble, selection, insertion, merge, quick)
- Binary search and linear search
- Recursive patterns (fibonacci, factorial, power)
- Two pointer technique
- Sliding window problems
- Time and space complexity cheat sheet

**Grep examples**:
```bash
grep -n "merge sort" algorithms.py
grep -n "binary search" algorithms.py
grep -n "two pointers" algorithms.py
grep -n "Time Complexity" algorithms.py
```

### 3. oop.py
**Topics**: Classes, Inheritance, Polymorphism, Encapsulation, Magic Methods

Object-oriented programming concepts:
- Class and instance variables
- Inheritance and multiple inheritance
- Method overriding and polymorphism
- Private/protected attributes
- Property decorators
- Magic methods (__init__, __str__, __repr__, etc.)
- Dataclasses
- Design patterns (Singleton, Factory)

**Grep examples**:
```bash
grep -n "What is inheritance" oop.py
grep -n "magic methods" oop.py
grep -n "property decorator" oop.py
grep -n "dataclass" oop.py
```

### 4. advanced_concepts.py
**Topics**: Decorators, Generators, Iterators, Closures, Context Managers

Advanced Python features:
- Decorators (simple, with arguments, caching)
- Generators and yield
- Iterators and iterables
- List/dict/set comprehensions
- Lambda functions
- Closures and nonlocal
- Context managers (with statement)
- *args and **kwargs
- GIL and memory management
- Exception handling
- Type hints

**Grep examples**:
```bash
grep -n "What is a decorator" advanced_concepts.py
grep -n "generator" advanced_concepts.py
grep -n "context manager" advanced_concepts.py
grep -n "GIL" advanced_concepts.py
```

### 5. patterns_and_best_practices.py
**Topics**: Pythonic Code, Idioms, Best Practices, Common Patterns

Writing clean, pythonic code:
- PEP 8 conventions
- EAFP vs LBYL
- String formatting (f-strings, format, %)
- List/dict manipulation patterns
- File I/O (reading, writing, JSON, CSV)
- Performance optimization tips
- Common anti-patterns to avoid
- Useful tricks (walrus operator, unpacking, ternary)

**Grep examples**:
```bash
grep -n "PEP 8" patterns_and_best_practices.py
grep -n "EAFP" patterns_and_best_practices.py
grep -n "anti-pattern" patterns_and_best_practices.py
grep -n "f-string" patterns_and_best_practices.py
```

### 6. coding_challenges.py
**Topics**: Common Interview Coding Problems

Real interview coding challenges with solutions:
- String problems (palindrome, anagram, longest substring)
- Array problems (two sum, max subarray, rotate array)
- Linked list problems (reverse, cycle detection, merge)
- Tree problems (traversal, depth, balanced)
- Mathematical problems (prime, GCD, factorial)
- Dynamic programming (coin change, climbing stairs, LCS)
- Miscellaneous (LRU cache, valid parentheses, kth largest)

**Grep examples**:
```bash
grep -n "two sum" coding_challenges.py
grep -n "palindrome" coding_challenges.py
grep -n "reverse linked list" coding_challenges.py
grep -n "dynamic programming" coding_challenges.py
```

## Quick Search Guide

### By Topic
```bash
# Find all questions about a specific topic
grep -n "Q: .*list" *.py
grep -n "Q: .*decorator" *.py
grep -n "Q: .*class" *.py

# Find all answers
grep -n "A: " data_structures.py

# Find complexity information
grep -n "O(n)" algorithms.py
grep -n "Time Complexity" algorithms.py
```

### By Keyword
```bash
# Search across all files
grep -rn "lambda" .
grep -rn "generator" .
grep -rn "inheritance" .

# Case-insensitive search
grep -rin "fibonacci" .

# Show context (2 lines before and after)
grep -A 2 -B 2 "two sum" coding_challenges.py
```

## Interview Day Workflow

### Day Before (Monday)
1. Review each file completely
2. Practice coding challenges without looking at solutions
3. Create flashcards for concepts you find difficult
4. Focus on time/space complexity analysis

### Interview Day (Tuesday)
Quick reference during breaks:
```bash
# Quick lookup for specific question
grep -n "How do you.*reverse.*list" data_structures.py

# Find solution pattern
grep -A 10 "palindrome" coding_challenges.py

# Review complexity
grep "Time Complexity" algorithms.py
```

## Study Tips

1. **Active Reading**: Don't just read - type out examples and run them
2. **Spaced Repetition**: Review files multiple times over several days
3. **Practice Speaking**: Explain concepts out loud as if in an interview
4. **Test Edge Cases**: For each solution, think about edge cases
5. **Optimize**: For each problem, think about time/space trade-offs

## Common Interview Question Categories

### Easy (30% of questions)
- Basic data structure operations
- Simple string manipulation
- Array traversal
- Basic recursion

### Medium (50% of questions)
- Two pointers / sliding window
- Hash maps for optimization
- Tree/graph traversal
- Dynamic programming basics
- OOP design

### Hard (20% of questions)
- Advanced DP
- Complex tree/graph algorithms
- System design considerations
- Optimization problems

## Resources Used

All solutions follow:
- PEP 8 style guidelines
- Pythonic idioms and best practices
- Clear variable naming (no single letters)
- Comprehensive comments
- Example usage where applicable

## Good Luck!

Remember:
- Clarify requirements before coding
- Think out loud during the interview
- Start with a brute force solution, then optimize
- Test your code with examples
- Discuss trade-offs (time vs space complexity)

You've got this! 🐍

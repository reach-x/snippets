# Pattern Index - Quick Navigation Guide

**Use this index to quickly find the right pattern file during interviews**

---

## Quick Find: I Need To...

| I Need To... | Open This File | Jump to Section |
|--------------|----------------|-----------------|
| Find max/min sum of k consecutive elements | `01_sliding_window.py` | TEMPLATE 1: Fixed Window |
| Find longest substring with k distinct chars | `01_sliding_window.py` | TEMPLATE 3: HashMap |
| Generate all subsets/power set | `02_subset.py` | TEMPLATE 1: Subsets |
| Generate all permutations | `02_subset.py` | TEMPLATE 2: Permutations |
| Find combinations (choose k from n) | `02_subset.py` | TEMPLATE 3: Combinations |
| Find elements in sorted array | `03_modified_binary_search.py` | TEMPLATE 1: Classic Search |
| Find first/last occurrence | `03_modified_binary_search.py` | TEMPLATE 2-3: Boundaries |
| Find k largest elements | `04_k_largest_elements.py` | TEMPLATE 1: K Largest |
| Find k smallest elements | `04_k_largest_elements.py` | TEMPLATE 2: K Smallest |
| Find kth largest element | `04_k_largest_elements.py` | TEMPLATE 3: Kth Largest |
| Find top k frequent elements | `04_k_largest_elements.py` | TEMPLATE 4: Top K Frequent |
| Merge k sorted lists | `04_k_largest_elements.py` | TEMPLATE 5: Merge K Sorted |
| Traverse tree (preorder/inorder/postorder) | `05_binary_tree_dfs.py` | TEMPLATE 1-3: Traversals |
| Find tree depth/height | `05_binary_tree_dfs.py` | TEMPLATE 4: Max Depth |
| Find path sum in tree | `05_binary_tree_dfs.py` | TEMPLATE 5: Path Sum |
| Validate binary search tree | `05_binary_tree_dfs.py` | TEMPLATE 6: Validate BST |
| Find lowest common ancestor | `05_binary_tree_dfs.py` | TEMPLATE 7: LCA |
| Solve course schedule problem | `06_topological_sort.py` | TEMPLATE 2: Can Finish |
| Detect cycle in directed graph | `06_topological_sort.py` | TEMPLATE 3: Has Cycle |
| Find valid task/course ordering | `06_topological_sort.py` | TEMPLATE 4: Find Order |
| Process tree level by level | `07_binary_tree_bfs.py` | TEMPLATE 1: Level Order |
| Zigzag tree traversal | `07_binary_tree_bfs.py` | TEMPLATE 2: Zigzag |
| Find right/left side view of tree | `07_binary_tree_bfs.py` | TEMPLATE 3: Right View |
| Find minimum tree depth | `07_binary_tree_bfs.py` | TEMPLATE 4: Min Depth |
| Find two numbers that sum to target | `08_two_pointers.py` | TEMPLATE 1: Two Sum |
| Find three numbers that sum to target | `08_two_pointers.py` | TEMPLATE 2: Three Sum |
| Remove duplicates from sorted array | `08_two_pointers.py` | TEMPLATE 3: Remove Duplicates |
| Check if string is palindrome | `08_two_pointers.py` | TEMPLATE 4: Palindrome |
| Find container with most water | `08_two_pointers.py` | TEMPLATE 5: Max Area |
| Sort array of 0s, 1s, 2s | `08_two_pointers.py` | TEMPLATE 6: Sort Colors |

---

## Pattern Files Overview

### 01_sliding_window.py
**When**: Contiguous subarray/substring problems
**Solves**: Max/min sum of k elements, longest substring with conditions, fruits into baskets
**Key Templates**: 3 templates (fixed, variable, hashmap)

### 02_subset.py
**When**: Need ALL combinations/permutations/subsets
**Solves**: Power set, permutations, combinations, combination sum, N-Queens
**Key Templates**: 4 templates (subsets, permutations, combinations, combination sum)

### 03_modified_binary_search.py
**When**: Searching in sorted array/space
**Solves**: Find element, find boundaries, rotated arrays, search insert position
**Key Templates**: 5 templates (classic, first, last, insert, bisect)

### 04_k_largest_elements.py
**When**: Finding top-k, kth element, or merging sorted lists
**Solves**: K largest/smallest, kth largest, top k frequent, merge k sorted
**Key Templates**: 5 templates (k largest, k smallest, kth, top k frequent, merge)

### 05_binary_tree_dfs.py
**When**: Tree paths, depth, validation, properties
**Solves**: Traversals, max depth, path sum, BST validation, LCA, diameter
**Key Templates**: 7 templates (preorder, inorder, postorder, depth, path sum, BST, LCA)

### 06_topological_sort.py
**When**: Dependencies, task ordering, prerequisites
**Solves**: Course schedule, build order, cycle detection, valid ordering
**Key Templates**: 4 templates (Kahn's, can finish, has cycle, find order)

### 07_binary_tree_bfs.py
**When**: Level-by-level tree processing
**Solves**: Level order, zigzag, tree views, min depth, level statistics
**Key Templates**: 5 templates (level order, zigzag, views, min depth, averages)

### 08_two_pointers.py
**When**: Sorted array pairs/triplets, palindromes, partitioning
**Solves**: Two/three sum, remove duplicates, palindrome, container, sorting
**Key Templates**: 6 templates (two sum, three sum, duplicates, palindrome, area, colors)

---

## File Structure

Each pattern file contains:

```
├─ FILE HEADER (Docstring)
│  ├─ Pattern description
│  ├─ When to use
│  ├─ Time/space complexity
│  └─ Core concepts
│
├─ QUICK REFERENCE (Copy-paste templates)
│  ├─ TEMPLATE 1: Most common use case
│  ├─ TEMPLATE 2: Second common use case
│  ├─ TEMPLATE 3: Variant
│  └─ ... (3-7 templates per file)
│
├─ DETAILED IMPLEMENTATIONS
│  ├─ Pattern 1: Full explanation + code
│  ├─ Pattern 2: Full explanation + code
│  └─ ...
│
└─ TEST CASES
   ├─ Comprehensive examples
   ├─ Expected outputs
   └─ Key insights
```

---

## How to Use This Index During Interview

### Scenario 1: You Know The Problem Type
```
Problem: "Find the k largest elements in an array"
         ↓
1. Look in "I Need To..." table
2. Find: "Find k largest elements"
3. Open: 04_k_largest_elements.py
4. Go to: TEMPLATE 1
5. Copy and adapt template
```

### Scenario 2: You're Not Sure Which Pattern
```
Problem: "Find longest substring with at most k distinct characters"
         ↓
1. Identify keywords: "substring" (contiguous), "longest"
2. Check CHEATSHEET.md pattern matcher
3. Match: "contiguous" → Sliding Window
4. Open: 01_sliding_window.py
5. Go to: TEMPLATE 3 (HashMap variant)
6. Copy and adapt template
```

### Scenario 3: Quick Template Lookup
```
During interview, need quick code for "remove duplicates":
         ↓
1. Open this index
2. Search "remove duplicates"
3. See: 08_two_pointers.py → TEMPLATE 3
4. Open file, jump to TEMPLATE 3
5. Copy, paste, done in 30 seconds
```

---

## Problem Type → Pattern Mapping

### Array Problems
| Problem Type | Pattern | File |
|--------------|---------|------|
| Contiguous elements (subarray/substring) | Sliding Window | 01 |
| All combinations/subsets | Backtracking | 02 |
| Search in sorted array | Binary Search | 03 |
| Top k elements | Heap | 04 |
| Pair/triplet sum (sorted) | Two Pointers | 08 |

### Tree Problems
| Problem Type | Pattern | File |
|--------------|---------|------|
| Paths, depth, validation | Tree DFS | 05 |
| Level-by-level processing | Tree BFS | 07 |
| Shortest path to leaf | Tree BFS | 07 |

### Graph Problems
| Problem Type | Pattern | File |
|--------------|---------|------|
| Dependencies, prerequisites | Topological Sort | 06 |
| Course schedule | Topological Sort | 06 |
| Cycle detection (directed) | Topological Sort | 06 |

---

## LeetCode Problem → Pattern File

Quick reference for common LeetCode problems:

| LC# | Problem | Pattern | File |
|-----|---------|---------|------|
| 3 | Longest Substring Without Repeating | Sliding Window | 01 |
| 15 | 3Sum | Two Pointers | 08 |
| 23 | Merge k Sorted Lists | Heap | 04 |
| 46 | Permutations | Backtracking | 02 |
| 78 | Subsets | Backtracking | 02 |
| 102 | Binary Tree Level Order | Tree BFS | 07 |
| 104 | Maximum Depth of Binary Tree | Tree DFS | 05 |
| 167 | Two Sum II (sorted) | Two Pointers | 08 |
| 207 | Course Schedule | Topological Sort | 06 |
| 215 | Kth Largest Element | Heap | 04 |
| 236 | Lowest Common Ancestor | Tree DFS | 05 |
| 347 | Top K Frequent Elements | Heap | 04 |
| 704 | Binary Search | Binary Search | 03 |

---

## Files in This Directory

| File | Purpose | Use When |
|------|---------|----------|
| `README.md` | Master guide with detailed explanations | Learning patterns, understanding concepts |
| `CHEATSHEET.md` | One-page quick reference | During interview, need quick lookup |
| `PATTERN_INDEX.md` | This file - navigation guide | Finding right file/template fast |
| `01_sliding_window.py` | Sliding window patterns | Contiguous subarrays/substrings |
| `02_subset.py` | Backtracking patterns | All combinations/permutations |
| `03_modified_binary_search.py` | Binary search patterns | Sorted array search |
| `04_k_largest_elements.py` | Heap patterns | Top-k, kth element problems |
| `05_binary_tree_dfs.py` | Tree DFS patterns | Tree paths, depth, validation |
| `06_topological_sort.py` | Topological sort patterns | Dependencies, ordering |
| `07_binary_tree_bfs.py` | Tree BFS patterns | Level-order processing |
| `08_two_pointers.py` | Two pointers patterns | Pair/triplet sum, palindromes |

---

## Recommended Workflow

### Before Interview
1. Print or save `CHEATSHEET.md` for quick reference
2. Bookmark this `PATTERN_INDEX.md` for navigation
3. Practice identifying patterns using `README.md` decision tree

### During Interview
1. Read problem → identify keywords
2. Check `CHEATSHEET.md` pattern matcher table
3. Use this index to find right file + template
4. Copy template and adapt to problem
5. Explain approach and code solution

### After Interview
1. Review patterns used
2. Practice similar problems
3. Update your understanding of when to use each pattern

---

**Pro Tip**: Keep `CHEATSHEET.md` and `PATTERN_INDEX.md` open in separate tabs during interviews for fastest access!

# Coding Patterns - Interview Quick Reference

**Last Updated**: 2025-11-17
**Purpose**: Master reference guide for selecting and implementing coding patterns during technical interviews

---

## Pattern Selection Guide

### How to Use This Guide During Interviews
1. **Read the problem** - Identify key characteristics
2. **Match to pattern** - Use the decision tree below
3. **Copy template** - Each file has copy-paste ready templates
4. **Adapt solution** - Modify template for specific problem

---

## Quick Pattern Matcher

### Question Clues → Pattern Selection

| Problem Contains... | Use This Pattern | File |
|---------------------|------------------|------|
| "subarray", "substring", "contiguous" | **Sliding Window** | 01_sliding_window.py |
| "all combinations", "all subsets", "permutations" | **Backtracking/Subset** | 02_subset.py |
| "sorted array", "find in log(n) time" | **Binary Search** | 03_modified_binary_search.py |
| "top k", "k largest", "k smallest", "kth element" | **Heap/Quickselect** | 04_k_largest_elements.py |
| "binary tree" + path/traversal/depth | **Tree DFS** | 05_binary_tree_dfs.py |
| "dependencies", "course schedule", "build order" | **Topological Sort** | 06_topological_sort.py |
| "binary tree" + level/breadth | **Tree BFS** | 07_binary_tree_bfs.py |
| "sorted array" + two elements, "pair sum" | **Two Pointers** | 08_two_pointers.py |

---

## Pattern Decision Tree

```
START: What type of problem is this?

├─ ARRAY/STRING PROBLEM?
│  ├─ Need contiguous elements? → SLIDING WINDOW (01)
│  ├─ Need all combinations? → BACKTRACKING (02)
│  ├─ Array is sorted?
│  │  ├─ Find element/position? → BINARY SEARCH (03)
│  │  └─ Find pair/triplet sum? → TWO POINTERS (08)
│  └─ Need top K elements? → HEAP (04)
│
├─ TREE PROBLEM?
│  ├─ Need paths/depth/validation? → TREE DFS (05)
│  └─ Need level-by-level processing? → TREE BFS (07)
│
└─ GRAPH PROBLEM?
   └─ Has dependencies/ordering? → TOPOLOGICAL SORT (06)
```

---

## Pattern Details & Complexity

### 1. Sliding Window (01_sliding_window.py)
**When**: Contiguous subarray/substring problems
**Time**: O(n)
**Space**: O(1) to O(k)
**Key**: Expand window (add right), contract window (remove left)

**Templates Available**:
- Fixed window size
- Variable window size (expand/contract)
- Window with HashMap tracking

**Common Problems**:
- Maximum sum subarray of size K
- Longest substring with K distinct characters
- Minimum window substring
- Fruits into baskets

---

### 2. Backtracking/Subset (02_subset.py)
**When**: Need ALL combinations/permutations/subsets
**Time**: O(2^n) for subsets, O(n!) for permutations
**Space**: O(n) recursion depth
**Key**: Make choice → Explore → Undo choice

**Templates Available**:
- Subsets (with/without duplicates)
- Permutations
- Combinations
- Combination sum
- N-Queens

**Common Problems**:
- Generate all subsets
- Letter combinations
- Palindrome partitioning
- Word search in grid

---

### 3. Binary Search (03_modified_binary_search.py)
**When**: Searching in sorted array in O(log n)
**Time**: O(log n)
**Space**: O(1) iterative, O(log n) recursive
**Key**: Eliminate half the search space each iteration

**Templates Available**:
- Classic binary search
- Find first/last occurrence (boundaries)
- Search in rotated sorted array
- Search in 2D matrix
- Peak element finding

**Common Problems**:
- Find element in sorted array
- Find insertion position
- Search in rotated array
- Find min/max in rotated array

---

### 4. Heap/K Largest Elements (04_k_largest_elements.py)
**When**: Finding top K, Kth largest/smallest, frequent elements
**Time**: O(n log k) heap, O(n) average quickselect
**Space**: O(k) heap
**Key**: Min-heap for K largest, Max-heap for K smallest

**Templates Available**:
- K largest/smallest with heap
- Kth largest element (heap + quickselect)
- Top K frequent elements
- K closest points
- Merge K sorted arrays/lists

**Common Problems**:
- Kth largest element in array
- Top K frequent words
- K closest points to origin
- Merge K sorted lists

---

### 5. Tree DFS (05_binary_tree_dfs.py)
**When**: Tree traversal, paths, depth, validation
**Time**: O(n) visits each node once
**Space**: O(h) recursion stack height
**Key**: Process in order - Preorder/Inorder/Postorder

**Templates Available**:
- Preorder/Inorder/Postorder (recursive + iterative)
- Max/Min depth
- Path sum variations
- Tree validation (BST, balanced, symmetric)
- Lowest Common Ancestor

**Common Problems**:
- Maximum depth of tree
- Path sum II (all paths)
- Validate BST
- Diameter of tree
- Lowest common ancestor

---

### 6. Topological Sort (06_topological_sort.py)
**When**: Dependency resolution, ordering with constraints
**Time**: O(V + E) vertices + edges
**Space**: O(V)
**Key**: Process nodes with no dependencies first

**Templates Available**:
- Kahn's algorithm (BFS with in-degrees)
- DFS-based topological sort
- Cycle detection
- Course schedule variations

**Common Problems**:
- Course schedule (can finish all?)
- Course schedule II (find valid order)
- Alien dictionary
- Minimum height trees

---

### 7. Tree BFS (07_binary_tree_bfs.py)
**When**: Level-order processing, shortest path in tree
**Time**: O(n) visits each node once
**Space**: O(w) maximum width of tree
**Key**: Use queue, process level by level

**Templates Available**:
- Level-order traversal
- Zigzag level-order
- Level statistics (average, max per level)
- Right/Left side view
- Vertical order traversal

**Common Problems**:
- Level order traversal
- Zigzag traversal
- Right side view of tree
- Minimum depth (BFS finds shortest)
- Connect nodes at same level

---

### 8. Two Pointers (08_two_pointers.py)
**When**: Sorted array pair/triplet sum, palindrome, partitioning
**Time**: O(n) or O(n²) for 3-sum
**Space**: O(1)
**Key**: Move pointers based on comparison

**Templates Available**:
- Opposite ends (for sorted arrays)
- Same direction (fast/slow pointers)
- Three pointers (Dutch National Flag)
- Two sum, three sum, four sum
- Remove duplicates, reverse, partition

**Common Problems**:
- Two sum (sorted array)
- Three sum / Four sum
- Container with most water
- Trapping rain water
- Remove duplicates in-place

---

## Time Complexity Cheatsheet

| Pattern | Best Case | Average | Worst Case | Space |
|---------|-----------|---------|------------|-------|
| Sliding Window | O(n) | O(n) | O(n) | O(1)-O(k) |
| Backtracking | - | - | O(2^n) to O(n!) | O(n) |
| Binary Search | O(1) | O(log n) | O(log n) | O(1) |
| Heap (K elements) | O(n) | O(n log k) | O(n log k) | O(k) |
| Tree DFS | O(n) | O(n) | O(n) | O(h) |
| Topological Sort | O(V+E) | O(V+E) | O(V+E) | O(V) |
| Tree BFS | O(n) | O(n) | O(n) | O(w) |
| Two Pointers | O(n) | O(n) | O(n²)* | O(1) |

*O(n²) for 3-sum, O(n³) for 4-sum due to nested iterations

---

## Interview Strategy

### Step 1: Identify Pattern (2-3 minutes)
- Read problem carefully
- Identify keywords (sorted, contiguous, tree, K largest, etc.)
- Match to pattern using guide above

### Step 2: Clarify Constraints (1-2 minutes)
- Ask about input size
- Ask about edge cases
- Confirm expected time/space complexity

### Step 3: Explain Approach (3-5 minutes)
- State which pattern you'll use
- Explain why it fits
- Walk through high-level algorithm
- Mention time/space complexity

### Step 4: Code Solution (10-15 minutes)
- Open appropriate pattern file
- Copy relevant template
- Adapt to specific problem
- Use descriptive variable names

### Step 5: Test & Optimize (5 minutes)
- Walk through example
- Check edge cases
- Discuss optimizations

---

## File Organization

Each pattern file contains:

1. **QUICK REFERENCE** - Copy-paste ready templates
2. **PATTERN OVERVIEW** - When to use, complexity, key concepts
3. **DETAILED IMPLEMENTATIONS** - Full solutions with explanations
4. **TEST CASES** - Comprehensive examples
5. **KEY INSIGHTS** - Pattern-specific tips and tricks

---

## Common Interview Mistakes to Avoid

### Sliding Window
❌ Recalculating window sum from scratch each time
✅ Subtract left, add right (O(1) per move)

### Backtracking
❌ Forgetting to make a copy when adding to result
✅ Always use `current_path[:]` or `list(current_path)`

### Binary Search
❌ Infinite loop with wrong mid calculation
✅ Use `mid = left + (right - left) // 2`

### Heap
❌ Using min-heap for K smallest (wrong!)
✅ Min-heap for K largest, Max-heap for K smallest

### Tree DFS
❌ Modifying tree structure during traversal
✅ Use helper functions, avoid side effects

### Topological Sort
❌ Not checking for cycles
✅ If can't order all vertices, cycle exists

### Tree BFS
❌ Using `list.pop(0)` which is O(n)
✅ Use `collections.deque` with `popleft()` which is O(1)

### Two Pointers
❌ Using on unsorted array when sorted is required
✅ Check if array needs sorting first

---

## Quick Access by Problem Type

### Array Problems
- **Contiguous elements**: Sliding Window (01)
- **All combinations**: Backtracking (02)
- **Sorted + search**: Binary Search (03)
- **Sorted + pair sum**: Two Pointers (08)
- **Top K**: Heap (04)

### String Problems
- **Substring**: Sliding Window (01)
- **All combinations**: Backtracking (02)
- **Pattern matching**: Usually custom logic

### Tree Problems
- **Paths/Depth/Validation**: Tree DFS (05)
- **Level-by-level**: Tree BFS (07)
- **Shortest path**: Tree BFS (07)

### Graph Problems
- **Dependencies**: Topological Sort (06)
- **Shortest path**: BFS (not covered here, use BFS template)
- **Connected components**: DFS (not covered here, use DFS template)

---

## Additional Resources

### LeetCode Problem Lists by Pattern
1. **Sliding Window**: LC 3, 76, 438, 567, 209
2. **Backtracking**: LC 78, 90, 46, 47, 39, 40
3. **Binary Search**: LC 704, 35, 33, 81, 153, 162
4. **Heap**: LC 215, 347, 973, 23, 295
5. **Tree DFS**: LC 104, 110, 112, 113, 98, 236
6. **Topological Sort**: LC 207, 210, 269, 310
7. **Tree BFS**: LC 102, 103, 107, 199, 314
8. **Two Pointers**: LC 167, 15, 18, 11, 42

### Practice Recommendations
- Start with one pattern at a time
- Solve 5-10 problems per pattern
- Focus on understanding WHY pattern works
- Practice coding without looking at solution
- Time yourself (aim for 15-20 minutes per problem)

---

## Version History
- **v1.0** (2025-11-17): Initial comprehensive pattern collection
- All 8 patterns enhanced with full variable names
- Beginner-friendly documentation
- Interview-ready templates

---

**Remember**: Pattern recognition is a skill that improves with practice. The more problems you solve, the faster you'll recognize which pattern to apply!

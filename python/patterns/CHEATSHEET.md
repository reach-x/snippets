# Coding Patterns - One-Page Cheatsheet

**Print this page for quick reference during interviews**

---

## Pattern Selection: Problem Keywords → Pattern

| If Problem Says... | Use This Pattern | Time | Space | Key Insight |
|-------------------|------------------|------|-------|-------------|
| "contiguous", "subarray", "substring" | **Sliding Window** | O(n) | O(1) | Maintain window, slide it |
| "all subsets", "all permutations", "all combinations" | **Backtracking** | O(2^n) to O(n!) | O(n) | Choose → Explore → Undo |
| "sorted array" + search | **Binary Search** | O(log n) | O(1) | Eliminate half each time |
| "k largest", "k smallest", "top k", "kth element" | **Heap** | O(n log k) | O(k) | Min-heap for k largest |
| "binary tree" + "path", "depth", "validate" | **Tree DFS** | O(n) | O(h) | Recursion, go deep |
| "binary tree" + "level", "breadth", "width" | **Tree BFS** | O(n) | O(w) | Queue, go wide |
| "prerequisites", "course schedule", "dependencies" | **Topological Sort** | O(V+E) | O(V) | Process zero in-degree |
| "sorted array" + "pair sum", "triplet sum" | **Two Pointers** | O(n) to O(n²) | O(1) | Move from both ends |

---

## Quick Templates

### 1. SLIDING WINDOW - Fixed Size
```python
def sliding_window_fixed(arr, k):
    window_sum = sum(arr[:k])
    result = window_sum
    for i in range(k, len(arr)):
        window_sum = window_sum - arr[i-k] + arr[i]
        result = max(result, window_sum)
    return result
```

### 2. SLIDING WINDOW - Variable Size
```python
def sliding_window_variable(arr, target):
    left = 0
    current_sum = 0
    result = 0
    for right in range(len(arr)):
        current_sum += arr[right]
        while current_sum > target:
            current_sum -= arr[left]
            left += 1
        result = max(result, right - left + 1)
    return result
```

### 3. BACKTRACKING - Subsets
```python
def subsets(nums):
    result = []
    def backtrack(start, path):
        result.append(path[:])  # COPY!
        for i in range(start, len(nums)):
            path.append(nums[i])
            backtrack(i + 1, path)
            path.pop()
    backtrack(0, [])
    return result
```

### 4. BACKTRACKING - Permutations
```python
def permutations(nums):
    result = []
    def backtrack(path):
        if len(path) == len(nums):
            result.append(path[:])  # COPY!
            return
        for num in nums:
            if num in path: continue
            path.append(num)
            backtrack(path)
            path.pop()
    backtrack([])
    return result
```

### 5. BINARY SEARCH - Classic
```python
def binary_search(arr, target):
    left, right = 0, len(arr) - 1
    while left <= right:
        mid = left + (right - left) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return -1
```

### 6. BINARY SEARCH - Find First
```python
def find_first(arr, target):
    left, right = 0, len(arr) - 1
    result = -1
    while left <= right:
        mid = left + (right - left) // 2
        if arr[mid] == target:
            result = mid
            right = mid - 1  # Continue LEFT
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return result
```

### 7. HEAP - K Largest (Min-Heap)
```python
import heapq
def k_largest(nums, k):
    min_heap = nums[:k]
    heapq.heapify(min_heap)
    for i in range(k, len(nums)):
        if nums[i] > min_heap[0]:
            heapq.heapreplace(min_heap, nums[i])
    return min_heap
```

### 8. HEAP - K Smallest (Max-Heap)
```python
def k_smallest(nums, k):
    max_heap = [-x for x in nums[:k]]
    heapq.heapify(max_heap)
    for i in range(k, len(nums)):
        if nums[i] < -max_heap[0]:
            heapq.heapreplace(max_heap, -nums[i])
    return [-x for x in max_heap]
```

### 9. TREE DFS - Preorder
```python
def preorder(root):
    result = []
    def dfs(node):
        if not node: return
        result.append(node.value)
        dfs(node.left)
        dfs(node.right)
    dfs(root)
    return result
```

### 10. TREE DFS - Inorder (BST → Sorted)
```python
def inorder(root):
    result = []
    def dfs(node):
        if not node: return
        dfs(node.left)
        result.append(node.value)
        dfs(node.right)
    dfs(root)
    return result
```

### 11. TREE DFS - Max Depth
```python
def max_depth(root):
    if not root: return 0
    return 1 + max(max_depth(root.left), max_depth(root.right))
```

### 12. TREE BFS - Level Order
```python
from collections import deque
def level_order(root):
    if not root: return []
    result = []
    queue = deque([root])
    while queue:
        level_size = len(queue)
        level = []
        for _ in range(level_size):
            node = queue.popleft()
            level.append(node.value)
            if node.left: queue.append(node.left)
            if node.right: queue.append(node.right)
        result.append(level)
    return result
```

### 13. TOPOLOGICAL SORT - Kahn's Algorithm
```python
from collections import deque, defaultdict
def topological_sort(n, edges):
    graph = defaultdict(list)
    in_degree = [0] * n
    for src, dst in edges:
        graph[src].append(dst)
        in_degree[dst] += 1
    queue = deque([i for i in range(n) if in_degree[i] == 0])
    result = []
    while queue:
        vertex = queue.popleft()
        result.append(vertex)
        for neighbor in graph[vertex]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)
    return result if len(result) == n else []
```

### 14. TWO POINTERS - Two Sum (Sorted)
```python
def two_sum(nums, target):
    left, right = 0, len(nums) - 1
    while left < right:
        current_sum = nums[left] + nums[right]
        if current_sum == target:
            return [left, right]
        elif current_sum < target:
            left += 1
        else:
            right -= 1
    return []
```

### 15. TWO POINTERS - Three Sum
```python
def three_sum(nums):
    nums.sort()
    result = []
    for i in range(len(nums) - 2):
        if i > 0 and nums[i] == nums[i-1]: continue
        left, right = i + 1, len(nums) - 1
        while left < right:
            total = nums[i] + nums[left] + nums[right]
            if total == 0:
                result.append([nums[i], nums[left], nums[right]])
                while left < right and nums[left] == nums[left+1]: left += 1
                while left < right and nums[right] == nums[right-1]: right -= 1
                left += 1
                right -= 1
            elif total < 0:
                left += 1
            else:
                right -= 1
    return result
```

---

## Critical Reminders

| Pattern | DON'T Forget! |
|---------|---------------|
| Sliding Window | Subtract left, add right (don't recalculate!) |
| Backtracking | `result.append(path[:])` - ALWAYS copy! |
| Binary Search | `mid = left + (right - left) // 2` - avoid overflow |
| Heap | Min-heap for k LARGEST, max-heap for k SMALLEST |
| Tree DFS | Base case: `if not node: return` |
| Tree BFS | `deque.popleft()` is O(1), `list.pop(0)` is O(n)! |
| Topological | Check cycle: `len(result) == n` |
| Two Pointers | Array must be SORTED for sum problems |

---

## Pattern Selection Flowchart

```
START
│
├─ Array/String Problem?
│  ├─ Contiguous elements? → SLIDING WINDOW
│  ├─ All combinations? → BACKTRACKING
│  ├─ Is sorted?
│  │  ├─ Find element? → BINARY SEARCH
│  │  └─ Pair/triplet sum? → TWO POINTERS
│  └─ Top K elements? → HEAP
│
├─ Tree Problem?
│  ├─ Paths/depth/validation? → TREE DFS
│  └─ Level-by-level? → TREE BFS
│
└─ Graph Problem?
   └─ Dependencies/ordering? → TOPOLOGICAL SORT
```

---

## Time Complexity Quick Reference

| Pattern | Best | Average | Worst | When to Use |
|---------|------|---------|-------|-------------|
| Sliding Window | O(n) | O(n) | O(n) | Better than O(n*k) brute force |
| Backtracking | - | - | O(2^n) to O(n!) | Only when you need ALL solutions |
| Binary Search | O(1) | O(log n) | O(log n) | Sorted data, huge improvement over O(n) |
| Heap | O(n) | O(n log k) | O(n log k) | K << n, or streaming data |
| Tree DFS | O(n) | O(n) | O(n) | Paths, validation, tree properties |
| Topological Sort | O(V+E) | O(V+E) | O(V+E) | Directed acyclic graphs only |
| Tree BFS | O(n) | O(n) | O(n) | Shortest path, level problems |
| Two Pointers | O(n) | O(n) | O(n²)* | Sorted arrays, O(1) space needed |

*O(n²) for 3-sum, O(n³) for 4-sum

---

## Common LeetCode Problems by Pattern

**Sliding Window**: 3, 76, 209, 438, 567, 904, 1004
**Backtracking**: 78, 90, 46, 47, 39, 40, 77, 51
**Binary Search**: 704, 35, 33, 81, 153, 162, 74
**Heap**: 215, 347, 973, 23, 295, 703
**Tree DFS**: 104, 110, 112, 113, 98, 236, 543
**Topological Sort**: 207, 210, 269, 310, 444
**Tree BFS**: 102, 103, 107, 199, 314, 515, 116
**Two Pointers**: 167, 15, 18, 11, 42, 26, 27

---

## Interview Strategy (20-25 min problem)

**1. Understand (2-3 min)**
- Read problem carefully
- Ask clarifying questions
- Confirm inputs/outputs

**2. Pattern Recognition (1-2 min)**
- Identify keywords
- Match to pattern using this cheatsheet
- State which pattern you'll use

**3. Explain Approach (2-3 min)**
- High-level algorithm
- Why this pattern fits
- Time/space complexity

**4. Code (10-15 min)**
- Copy template from pattern file
- Adapt to problem
- Use descriptive variable names

**5. Test & Optimize (3-5 min)**
- Walk through example
- Check edge cases
- Discuss improvements

---

**File Locations:**
- Detailed patterns: `patterns/01_*.py` through `patterns/08_*.py`
- Full guide: `patterns/README.md`
- This cheatsheet: `patterns/CHEATSHEET.md`

**Quick Access:** Open this cheatsheet during interviews for instant pattern lookup!

# SortingLib - Comprehensive Python Sorting Library

A complete sorting algorithms library with 13 different sorting algorithms, performance analysis, and detailed documentation.

## Features

- ✅ **13 Sorting Algorithms** - All major sorting algorithms implemented
- ✅ **Performance Tracking** - Track comparisons, swaps, and execution time
- ✅ **Flexible API** - Custom key functions, reverse sorting
- ✅ **Benchmarking Tools** - Compare algorithm performance
- ✅ **Complete Documentation** - Complexity analysis and use cases
- ✅ **Type Hints** - Full type annotations for IDE support
- ✅ **Zero Dependencies** - Pure Python standard library

## Quick Start

```python
from sortinglib import sort, SortAlgorithm

# Simple sorting
result = sort([64, 34, 25, 12, 22, 11, 90])
# [11, 12, 22, 25, 34, 64, 90]

# Choose specific algorithm
result = sort([5, 2, 8, 1], algorithm=SortAlgorithm.MERGE)

# Custom key function
words = ['apple', 'pie', 'zoo', 'at']
result = sort(words, algorithm=SortAlgorithm.QUICK, key=len)
# ['at', 'pie', 'zoo', 'apple']

# Reverse sorting
result = sort([3, 1, 4, 1, 5], reverse=True)
# [5, 4, 3, 1, 1]
```

## Available Algorithms

### Comparison-Based Sorts

| Algorithm | Best | Average | Worst | Space | Stable | Best For |
|-----------|------|---------|-------|-------|--------|----------|
| **Bubble Sort** | O(n) | O(n²) | O(n²) | O(1) | ✓ | Small/educational |
| **Selection Sort** | O(n²) | O(n²) | O(n²) | O(1) | ✗ | Minimal writes |
| **Insertion Sort** | O(n) | O(n²) | O(n²) | O(1) | ✓ | Nearly sorted |
| **Merge Sort** | O(n log n) | O(n log n) | O(n log n) | O(n) | ✓ | Large datasets |
| **Quick Sort** | O(n log n) | O(n log n) | O(n²) | O(log n) | ✗ | General purpose |
| **Heap Sort** | O(n log n) | O(n log n) | O(n log n) | O(1) | ✗ | Guaranteed O(n log n) |
| **Shell Sort** | O(n log n) | O(n^1.5) | O(n²) | O(1) | ✗ | Medium datasets |
| **Cocktail Sort** | O(n) | O(n²) | O(n²) | O(1) | ✓ | Nearly sorted |
| **Comb Sort** | O(n log n) | O(n²) | O(n²) | O(1) | ✗ | Better bubble |
| **Tim Sort** | O(n) | O(n log n) | O(n log n) | O(n) | ✓ | Real-world data |

### Non-Comparison Sorts

| Algorithm | Time | Space | Stable | Constraints | Best For |
|-----------|------|-------|--------|-------------|----------|
| **Counting Sort** | O(n + k) | O(k) | ✓ | Non-negative integers | Small range integers |
| **Radix Sort** | O(d(n + k)) | O(n + k) | ✓ | Non-negative integers | Fixed-width integers |
| **Bucket Sort** | O(n + k) | O(n + k) | ✓ | Numeric data | Uniform distribution |

## API Reference

### Main Functions

#### sort()

```python
def sort(arr: List[Any], 
         algorithm: SortAlgorithm = SortAlgorithm.QUICK,
         key: Optional[Callable] = None, 
         reverse: bool = False,
         stats: Optional[SortStats] = None) -> List[Any]
```

Universal sorting function.

**Parameters:**
- `arr`: List to sort
- `algorithm`: Algorithm to use (default: QuickSort)
- `key`: Function to extract comparison key
- `reverse`: Sort in descending order
- `stats`: Optional SortStats object to track operations

**Returns:** Sorted list

**Example:**
```python
# Basic usage
sorted_list = sort([3, 1, 4, 1, 5, 9])

# With algorithm choice
sorted_list = sort(data, algorithm=SortAlgorithm.MERGE)

# With key function
people = [{'name': 'Alice', 'age': 30}, {'name': 'Bob', 'age': 25}]
sorted_people = sort(people, key=lambda x: x['age'])

# Track statistics
stats = SortStats()
result = sort([5, 2, 8, 1], stats=stats)
print(f"Comparisons: {stats.comparisons}, Swaps: {stats.swaps}")
```

### Individual Algorithm Functions

All algorithms can be called directly:

```python
from sortinglib import bubble_sort, quick_sort, merge_sort

result = bubble_sort([5, 2, 8, 1])
result = quick_sort([5, 2, 8, 1], reverse=True)
result = merge_sort([5, 2, 8, 1], key=lambda x: x % 3)
```

### Performance Analysis

#### benchmark()

```python
def benchmark(arr: List[Any], 
              algorithms: Optional[List[SortAlgorithm]] = None,
              trials: int = 1) -> Dict[str, Dict[str, Any]]
```

Benchmark multiple sorting algorithms.

**Example:**
```python
from sortinglib import benchmark, SortAlgorithm

data = [random.randint(0, 1000) for _ in range(100)]
results = benchmark(data, trials=5)

for algo, stats in results.items():
    print(f"{algo}: {stats['time']:.6f}s, {stats['comparisons']} comparisons")
```

#### compare_algorithms()

```python
def compare_algorithms(size: int = 100, 
                      algorithms: Optional[List[SortAlgorithm]] = None) -> None
```

Compare algorithms and print formatted results.

**Example:**
```python
from sortinglib import compare_algorithms

compare_algorithms(size=500)
```

Output:
```
Comparing sorting algorithms on 500 elements

Algorithm       Time (s)     Comparisons     Swaps     
-------------------------------------------------------
tim             0.000084     0               0         
quick           0.000421     3247            2013      
merge           0.000678     2486            0         
...
```

#### get_algorithm_info()

```python
def get_algorithm_info(algorithm: SortAlgorithm) -> Dict[str, str]
```

Get complexity and stability information.

**Example:**
```python
from sortinglib import get_algorithm_info, SortAlgorithm

info = get_algorithm_info(SortAlgorithm.MERGE)
print(f"Time complexity (avg): {info['time_avg']}")
print(f"Space complexity: {info['space']}")
print(f"Stable: {info['stable']}")
print(f"Use case: {info['use_case']}")
```

### Statistics Tracking

```python
from sortinglib import SortStats, sort

stats = SortStats()
result = sort([5, 2, 8, 1, 9, 3, 7], stats=stats)

print(f"Comparisons: {stats.comparisons}")
print(f"Swaps: {stats.swaps}")
print(f"Time: {stats.time_taken}")
```

## Detailed Examples

### Example 1: Sorting Different Data Types

```python
from sortinglib import sort, SortAlgorithm

# Integers
numbers = [64, 34, 25, 12, 22, 11, 90]
sorted_nums = sort(numbers, algorithm=SortAlgorithm.QUICK)
print(sorted_nums)  # [11, 12, 22, 25, 34, 64, 90]

# Strings
words = ['banana', 'apple', 'cherry', 'date']
sorted_words = sort(words, algorithm=SortAlgorithm.MERGE)
print(sorted_words)  # ['apple', 'banana', 'cherry', 'date']

# Floats
decimals = [3.14, 2.71, 1.41, 1.73]
sorted_decimals = sort(decimals)
print(sorted_decimals)  # [1.41, 1.73, 2.71, 3.14]
```

### Example 2: Custom Key Functions

```python
from sortinglib import sort

# Sort by string length
words = ['python', 'is', 'awesome', 'and', 'powerful']
by_length = sort(words, key=len)
print(by_length)  # ['is', 'and', 'python', 'awesome', 'powerful']

# Sort tuples by second element
points = [(1, 5), (3, 2), (2, 8), (4, 1)]
by_y = sort(points, key=lambda p: p[1])
print(by_y)  # [(4, 1), (3, 2), (1, 5), (2, 8)]

# Sort dictionaries
students = [
    {'name': 'Alice', 'grade': 85},
    {'name': 'Bob', 'grade': 92},
    {'name': 'Charlie', 'grade': 78}
]
by_grade = sort(students, key=lambda s: s['grade'], reverse=True)
print([s['name'] for s in by_grade])  # ['Bob', 'Alice', 'Charlie']
```

### Example 3: Comparing Algorithm Performance

```python
from sortinglib import sort, SortAlgorithm, SortStats
import random

# Generate test data
data = [random.randint(0, 1000) for _ in range(1000)]

algorithms = [
    SortAlgorithm.QUICK,
    SortAlgorithm.MERGE,
    SortAlgorithm.HEAP,
    SortAlgorithm.TIM
]

print(f"{'Algorithm':<15} {'Time (s)':<12} {'Comparisons':<15}")
print("-" * 42)

for algo in algorithms:
    stats = SortStats()
    import time
    start = time.perf_counter()
    result = sort(data.copy(), algorithm=algo, stats=stats)
    elapsed = time.perf_counter() - start
    
    print(f"{algo.value:<15} {elapsed:<12.6f} {stats.comparisons:<15}")
```

### Example 4: Stability Demonstration

```python
from sortinglib import merge_sort, quick_sort

# Data with duplicate keys
students = [
    ('Alice', 85),
    ('Bob', 92),
    ('Charlie', 85),
    ('David', 92),
    ('Eve', 85)
]

# Stable sort (Merge Sort) - preserves order of equal elements
stable_result = merge_sort(students, key=lambda x: x[1])
print("Stable (Merge):", [s[0] for s in stable_result])
# ['Alice', 'Charlie', 'Eve', 'Bob', 'David']
# Note: Alice, Charlie, Eve maintain their relative order

# Unstable sort (Quick Sort) - may not preserve order
unstable_result = quick_sort(students, key=lambda x: x[1])
print("Unstable (Quick):", [s[0] for s in unstable_result])
# Order of equal elements may vary
```

### Example 5: Specialized Integer Sorting

```python
from sortinglib import counting_sort, radix_sort, bucket_sort

# Counting sort - very fast for small range
small_range = [4, 2, 2, 8, 3, 3, 1]
result = counting_sort(small_range)
print(f"Counting: {result}")

# Radix sort - good for large integers
large_ints = [170, 45, 75, 90, 802, 24, 2, 66]
result = radix_sort(large_ints)
print(f"Radix: {result}")

# Bucket sort - good for floats
floats = [0.897, 0.565, 0.656, 0.1234, 0.665, 0.3434]
result = bucket_sort(floats)
print(f"Bucket: {result}")
```

### Example 6: Processing Large Datasets

```python
from sortinglib import sort, SortAlgorithm, benchmark
import random

# Generate large dataset
large_data = [random.randint(0, 10000) for _ in range(10000)]

# Find fastest algorithm for this data
results = benchmark(large_data[:1000], trials=3)
fastest = min(results.items(), key=lambda x: x[1]['time'])
print(f"Fastest algorithm: {fastest[0]} ({fastest[1]['time']:.6f}s)")

# Use fastest algorithm for full dataset
best_algo = SortAlgorithm(fastest[0])
sorted_data = sort(large_data, algorithm=best_algo)
print(f"Sorted {len(sorted_data)} elements")
```

### Example 7: Real-World Use Case - Log Processing

```python
from sortinglib import sort
from datetime import datetime

# Log entries with timestamps
logs = [
    {'time': '2024-01-15 10:30:45', 'level': 'INFO', 'msg': 'Server started'},
    {'time': '2024-01-15 10:25:12', 'level': 'ERROR', 'msg': 'Connection failed'},
    {'time': '2024-01-15 10:28:33', 'level': 'WARN', 'msg': 'Slow query'},
    {'time': '2024-01-15 10:32:01', 'level': 'INFO', 'msg': 'Request processed'},
]

# Sort by timestamp
def parse_time(log):
    return datetime.strptime(log['time'], '%Y-%m-%d %H:%M:%S')

sorted_logs = sort(logs, key=parse_time)

for log in sorted_logs:
    print(f"{log['time']} [{log['level']}] {log['msg']}")

# Sort by severity (custom order)
severity = {'ERROR': 0, 'WARN': 1, 'INFO': 2}
by_severity = sort(logs, key=lambda x: severity[x['level']])
```

### Example 8: Sorting with Multiple Criteria

```python
from sortinglib import sort

# Sort by multiple fields
employees = [
    {'name': 'Alice', 'dept': 'Engineering', 'salary': 120000},
    {'name': 'Bob', 'dept': 'Sales', 'salary': 90000},
    {'name': 'Charlie', 'dept': 'Engineering', 'salary': 110000},
    {'name': 'David', 'dept': 'Sales', 'salary': 95000},
]

# Sort by department, then by salary (descending)
sorted_emp = sort(employees, key=lambda e: (e['dept'], -e['salary']))

for emp in sorted_emp:
    print(f"{emp['name']:<10} {emp['dept']:<15} ${emp['salary']:,}")
```

## Algorithm Selection Guide

### When to Use Each Algorithm

**Quick Sort** (Default)
- General purpose sorting
- Good average-case performance
- When stability doesn't matter
```python
sort(data, algorithm=SortAlgorithm.QUICK)
```

**Merge Sort**
- Need guaranteed O(n log n)
- Stability is important
- Large datasets
- Sorting linked lists
```python
sort(data, algorithm=SortAlgorithm.MERGE)
```

**Heap Sort**
- Need O(n log n) with O(1) space
- Memory is limited
- Don't need stability
```python
sort(data, algorithm=SortAlgorithm.HEAP)
```

**Insertion Sort**
- Small datasets (< 50 elements)
- Data is nearly sorted
- Online sorting (streaming data)
```python
sort(data, algorithm=SortAlgorithm.INSERTION)
```

**Counting Sort**
- Non-negative integers
- Small range of values
- Need O(n) time
```python
counting_sort([4, 2, 2, 8, 3, 3, 1])
```

**Radix Sort**
- Large integers
- Fixed-width data
- Very fast for right data
```python
radix_sort([170, 45, 75, 90, 802, 24, 2, 66])
```

**Tim Sort** (Python's built-in)
- Real-world data with patterns
- Mix of sorted runs
- Production use
```python
sort(data, algorithm=SortAlgorithm.TIM)
```

## Performance Tips

1. **For small arrays (< 50)**: Use Insertion Sort
2. **For nearly sorted data**: Use Insertion Sort or Bubble Sort
3. **For general purpose**: Use Quick Sort or Tim Sort
4. **For guaranteed O(n log n)**: Use Merge Sort or Heap Sort
5. **For integers with small range**: Use Counting Sort
6. **For large integers**: Use Radix Sort
7. **When stability matters**: Use Merge Sort, Insertion Sort, or Tim Sort
8. **When memory is limited**: Use Heap Sort or Quick Sort

## Complexity Cheat Sheet

### Time Complexity Summary

```
Best Case:
O(n):     Bubble (sorted), Insertion (sorted), Counting, Bucket, Tim (sorted)
O(n log n): Merge, Quick, Heap, Shell, Tim

Average Case:
O(n²):    Bubble, Selection, Insertion, Shell, Cocktail, Comb
O(n log n): Merge, Quick, Heap, Tim

Worst Case:
O(n²):    Bubble, Selection, Insertion, Quick, Shell, Cocktail, Comb, Bucket
O(n log n): Merge, Heap, Tim
```

### Space Complexity Summary

```
O(1):     Bubble, Selection, Insertion, Heap, Shell, Cocktail, Comb
O(log n): Quick
O(n):     Merge, Counting, Radix, Bucket, Tim
```

## Testing

Run the built-in tests:

```bash
python sortinglib.py
```

This will:
- Test all algorithms on various data types
- Run performance comparisons
- Display algorithm information

## Requirements

- Python 3.6+
- No external dependencies

## License

MIT License - Feel free to use in your projects!

## Contributing

This library includes all major sorting algorithms. Feel free to extend with additional variants or optimizations!

## References

- Introduction to Algorithms (CLRS)
- The Art of Computer Programming (Knuth)
- Python's Timsort implementation

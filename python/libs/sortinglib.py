"""
SortingLib - A comprehensive sorting algorithms library for Python

This library provides implementations of all common sorting algorithms
with performance analysis, visualization support, and detailed documentation.

Author: JB
License: MIT
"""

import time
import random
from typing import List, Callable, Optional, Tuple, Dict, Any
from enum import Enum


class SortAlgorithm(Enum):
    """Enumeration of available sorting algorithms."""
    BUBBLE = "bubble"
    SELECTION = "selection"
    INSERTION = "insertion"
    MERGE = "merge"
    QUICK = "quick"
    HEAP = "heap"
    COUNTING = "counting"
    RADIX = "radix"
    BUCKET = "bucket"
    SHELL = "shell"
    COCKTAIL = "cocktail"
    COMB = "comb"
    TIM = "tim"


class SortStats:
    """Statistics collector for sorting operations."""
    
    def __init__(self):
        self.comparisons = 0
        self.swaps = 0
        self.time_taken = 0.0
    
    def reset(self):
        self.comparisons = 0
        self.swaps = 0
        self.time_taken = 0.0
    
    def __str__(self):
        return (f"Comparisons: {self.comparisons}, "
                f"Swaps: {self.swaps}, "
                f"Time: {self.time_taken:.6f}s")


# ============================================================================
# COMPARISON-BASED SORTS
# ============================================================================

def bubble_sort(arr: List[Any], key: Optional[Callable] = None, 
                reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Bubble Sort - Repeatedly swap adjacent elements if they're in wrong order.
    
    Time Complexity: O(n²) average and worst case, O(n) best case
    Space Complexity: O(1)
    Stable: Yes
    
    Best for: Small datasets, nearly sorted data, educational purposes
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
        
    Example:
        >>> bubble_sort([64, 34, 25, 12, 22, 11, 90])
        [11, 12, 22, 25, 34, 64, 90]
    """
    arr = arr.copy()
    n = len(arr)
    stats = stats or SortStats()
    
    for i in range(n):
        swapped = False
        for j in range(0, n - i - 1):
            stats.comparisons += 1
            
            val_j = key(arr[j]) if key else arr[j]
            val_j1 = key(arr[j + 1]) if key else arr[j + 1]
            
            if (val_j > val_j1) if not reverse else (val_j < val_j1):
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
                stats.swaps += 1
                swapped = True
        
        # If no swaps, array is sorted
        if not swapped:
            break
    
    return arr


def selection_sort(arr: List[Any], key: Optional[Callable] = None,
                   reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Selection Sort - Find minimum element and place it at beginning.
    
    Time Complexity: O(n²) all cases
    Space Complexity: O(1)
    Stable: No
    
    Best for: Small datasets, when memory writes are expensive
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    arr = arr.copy()
    n = len(arr)
    stats = stats or SortStats()
    
    for i in range(n):
        extreme_idx = i
        
        for j in range(i + 1, n):
            stats.comparisons += 1
            
            val_extreme = key(arr[extreme_idx]) if key else arr[extreme_idx]
            val_j = key(arr[j]) if key else arr[j]
            
            if (val_j < val_extreme) if not reverse else (val_j > val_extreme):
                extreme_idx = j
        
        if extreme_idx != i:
            arr[i], arr[extreme_idx] = arr[extreme_idx], arr[i]
            stats.swaps += 1
    
    return arr


def insertion_sort(arr: List[Any], key: Optional[Callable] = None,
                   reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Insertion Sort - Build sorted array one element at a time.
    
    Time Complexity: O(n²) average and worst case, O(n) best case
    Space Complexity: O(1)
    Stable: Yes
    
    Best for: Small datasets, nearly sorted data, online sorting
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    arr = arr.copy()
    stats = stats or SortStats()
    
    for i in range(1, len(arr)):
        key_val = arr[i]
        key_comp = key(key_val) if key else key_val
        j = i - 1
        
        while j >= 0:
            stats.comparisons += 1
            val_j = key(arr[j]) if key else arr[j]
            
            if (val_j > key_comp) if not reverse else (val_j < key_comp):
                arr[j + 1] = arr[j]
                stats.swaps += 1
                j -= 1
            else:
                break
        
        arr[j + 1] = key_val
    
    return arr


def merge_sort(arr: List[Any], key: Optional[Callable] = None,
               reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Merge Sort - Divide and conquer algorithm that splits and merges.
    
    Time Complexity: O(n log n) all cases
    Space Complexity: O(n)
    Stable: Yes
    
    Best for: Large datasets, linked lists, when stability matters
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    stats = stats or SortStats()
    
    def merge(left: List[Any], right: List[Any]) -> List[Any]:
        result = []
        i = j = 0
        
        while i < len(left) and j < len(right):
            stats.comparisons += 1
            
            val_left = key(left[i]) if key else left[i]
            val_right = key(right[j]) if key else right[j]
            
            if (val_left <= val_right) if not reverse else (val_left >= val_right):
                result.append(left[i])
                i += 1
            else:
                result.append(right[j])
                j += 1
        
        result.extend(left[i:])
        result.extend(right[j:])
        return result
    
    def merge_sort_recursive(arr: List[Any]) -> List[Any]:
        if len(arr) <= 1:
            return arr
        
        mid = len(arr) // 2
        left = merge_sort_recursive(arr[:mid])
        right = merge_sort_recursive(arr[mid:])
        
        return merge(left, right)
    
    return merge_sort_recursive(arr)


def quick_sort(arr: List[Any], key: Optional[Callable] = None,
               reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Quick Sort - Pick pivot and partition around it.
    
    Time Complexity: O(n log n) average, O(n²) worst case
    Space Complexity: O(log n) due to recursion
    Stable: No (this implementation)
    
    Best for: General purpose, large datasets, average case performance
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    arr = arr.copy()
    stats = stats or SortStats()
    
    def partition(low: int, high: int) -> int:
        pivot = arr[high]
        pivot_val = key(pivot) if key else pivot
        i = low - 1
        
        for j in range(low, high):
            stats.comparisons += 1
            val_j = key(arr[j]) if key else arr[j]
            
            if (val_j <= pivot_val) if not reverse else (val_j >= pivot_val):
                i += 1
                arr[i], arr[j] = arr[j], arr[i]
                stats.swaps += 1
        
        arr[i + 1], arr[high] = arr[high], arr[i + 1]
        stats.swaps += 1
        return i + 1
    
    def quick_sort_recursive(low: int, high: int):
        if low < high:
            pi = partition(low, high)
            quick_sort_recursive(low, pi - 1)
            quick_sort_recursive(pi + 1, high)
    
    quick_sort_recursive(0, len(arr) - 1)
    return arr


def heap_sort(arr: List[Any], key: Optional[Callable] = None,
              reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Heap Sort - Build max heap and extract elements.
    
    Time Complexity: O(n log n) all cases
    Space Complexity: O(1)
    Stable: No
    
    Best for: When consistent O(n log n) is needed with O(1) space
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    arr = arr.copy()
    n = len(arr)
    stats = stats or SortStats()
    
    def heapify(n: int, i: int):
        largest = i
        left = 2 * i + 1
        right = 2 * i + 2
        
        if left < n:
            stats.comparisons += 1
            val_left = key(arr[left]) if key else arr[left]
            val_largest = key(arr[largest]) if key else arr[largest]
            
            if (val_left > val_largest) if not reverse else (val_left < val_largest):
                largest = left
        
        if right < n:
            stats.comparisons += 1
            val_right = key(arr[right]) if key else arr[right]
            val_largest = key(arr[largest]) if key else arr[largest]
            
            if (val_right > val_largest) if not reverse else (val_right < val_largest):
                largest = right
        
        if largest != i:
            arr[i], arr[largest] = arr[largest], arr[i]
            stats.swaps += 1
            heapify(n, largest)
    
    # Build max heap
    for i in range(n // 2 - 1, -1, -1):
        heapify(n, i)
    
    # Extract elements
    for i in range(n - 1, 0, -1):
        arr[i], arr[0] = arr[0], arr[i]
        stats.swaps += 1
        heapify(i, 0)
    
    return arr


def shell_sort(arr: List[Any], key: Optional[Callable] = None,
               reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Shell Sort - Generalization of insertion sort with gap sequence.
    
    Time Complexity: O(n log²n) to O(n^1.5) depending on gap sequence
    Space Complexity: O(1)
    Stable: No
    
    Best for: Medium-sized datasets, better than simple O(n²) sorts
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    arr = arr.copy()
    n = len(arr)
    stats = stats or SortStats()
    
    # Start with large gap, then reduce
    gap = n // 2
    
    while gap > 0:
        for i in range(gap, n):
            temp = arr[i]
            temp_val = key(temp) if key else temp
            j = i
            
            while j >= gap:
                stats.comparisons += 1
                val_j = key(arr[j - gap]) if key else arr[j - gap]
                
                if (val_j > temp_val) if not reverse else (val_j < temp_val):
                    arr[j] = arr[j - gap]
                    stats.swaps += 1
                    j -= gap
                else:
                    break
            
            arr[j] = temp
        
        gap //= 2
    
    return arr


def cocktail_sort(arr: List[Any], key: Optional[Callable] = None,
                  reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Cocktail Sort (Bidirectional Bubble Sort) - Bubble sort in both directions.
    
    Time Complexity: O(n²) average and worst case
    Space Complexity: O(1)
    Stable: Yes
    
    Best for: Nearly sorted data, better than bubble sort in practice
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    arr = arr.copy()
    n = len(arr)
    stats = stats or SortStats()
    swapped = True
    start = 0
    end = n - 1
    
    while swapped:
        swapped = False
        
        # Forward pass
        for i in range(start, end):
            stats.comparisons += 1
            val_i = key(arr[i]) if key else arr[i]
            val_i1 = key(arr[i + 1]) if key else arr[i + 1]
            
            if (val_i > val_i1) if not reverse else (val_i < val_i1):
                arr[i], arr[i + 1] = arr[i + 1], arr[i]
                stats.swaps += 1
                swapped = True
        
        if not swapped:
            break
        
        swapped = False
        end -= 1
        
        # Backward pass
        for i in range(end - 1, start - 1, -1):
            stats.comparisons += 1
            val_i = key(arr[i]) if key else arr[i]
            val_i1 = key(arr[i + 1]) if key else arr[i + 1]
            
            if (val_i > val_i1) if not reverse else (val_i < val_i1):
                arr[i], arr[i + 1] = arr[i + 1], arr[i]
                stats.swaps += 1
                swapped = True
        
        start += 1
    
    return arr


def comb_sort(arr: List[Any], key: Optional[Callable] = None,
              reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Comb Sort - Improves bubble sort by eliminating turtles.
    
    Time Complexity: O(n²) worst case, O(n log n) average case
    Space Complexity: O(1)
    Stable: No
    
    Best for: Better than bubble sort for random data
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    arr = arr.copy()
    n = len(arr)
    stats = stats or SortStats()
    gap = n
    shrink = 1.3
    sorted_flag = False
    
    while not sorted_flag:
        gap = int(gap / shrink)
        if gap <= 1:
            gap = 1
            sorted_flag = True
        
        i = 0
        while i + gap < n:
            stats.comparisons += 1
            val_i = key(arr[i]) if key else arr[i]
            val_gap = key(arr[i + gap]) if key else arr[i + gap]
            
            if (val_i > val_gap) if not reverse else (val_i < val_gap):
                arr[i], arr[i + gap] = arr[i + gap], arr[i]
                stats.swaps += 1
                sorted_flag = False
            
            i += 1
    
    return arr


# ============================================================================
# NON-COMPARISON SORTS
# ============================================================================

def counting_sort(arr: List[int], reverse: bool = False, 
                  stats: Optional[SortStats] = None) -> List[int]:
    """
    Counting Sort - Count occurrences of each value.
    
    Time Complexity: O(n + k) where k is range of input
    Space Complexity: O(k)
    Stable: Yes
    
    Best for: Integer sorting with small range, very fast for right data
    Note: Only works with non-negative integers
    
    Args:
        arr: List of non-negative integers to sort
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    if not arr:
        return []
    
    stats = stats or SortStats()
    
    # Find range
    max_val = max(arr)
    min_val = min(arr)
    range_size = max_val - min_val + 1
    
    # Count occurrences
    count = [0] * range_size
    for num in arr:
        count[num - min_val] += 1
    
    # Build output
    result = []
    if reverse:
        for i in range(range_size - 1, -1, -1):
            result.extend([i + min_val] * count[i])
    else:
        for i in range(range_size):
            result.extend([i + min_val] * count[i])
    
    return result


def radix_sort(arr: List[int], reverse: bool = False,
               stats: Optional[SortStats] = None) -> List[int]:
    """
    Radix Sort - Sort by each digit position.
    
    Time Complexity: O(d * (n + k)) where d is number of digits
    Space Complexity: O(n + k)
    Stable: Yes
    
    Best for: Large numbers of integers, fixed-width data
    Note: Only works with non-negative integers
    
    Args:
        arr: List of non-negative integers to sort
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    if not arr:
        return []
    
    stats = stats or SortStats()
    
    # Find maximum number to know number of digits
    max_val = max(arr)
    exp = 1
    
    while max_val // exp > 0:
        # Counting sort by current digit
        output = [0] * len(arr)
        count = [0] * 10
        
        # Count occurrences
        for num in arr:
            index = (num // exp) % 10
            count[index] += 1
        
        # Cumulative count
        for i in range(1, 10):
            count[i] += count[i - 1]
        
        # Build output
        for i in range(len(arr) - 1, -1, -1):
            index = (arr[i] // exp) % 10
            output[count[index] - 1] = arr[i]
            count[index] -= 1
        
        arr = output
        exp *= 10
    
    return arr[::-1] if reverse else arr


def bucket_sort(arr: List[float], num_buckets: int = 10, 
                reverse: bool = False, stats: Optional[SortStats] = None) -> List[float]:
    """
    Bucket Sort - Distribute elements into buckets, sort each bucket.
    
    Time Complexity: O(n + k) average case, O(n²) worst case
    Space Complexity: O(n + k)
    Stable: Yes (if underlying sort is stable)
    
    Best for: Uniformly distributed data, floating point numbers
    
    Args:
        arr: List of numbers to sort
        num_buckets: Number of buckets to use
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    if not arr:
        return []
    
    stats = stats or SortStats()
    
    # Find range
    min_val = min(arr)
    max_val = max(arr)
    bucket_range = (max_val - min_val) / num_buckets
    
    # Create buckets
    buckets = [[] for _ in range(num_buckets)]
    
    # Distribute elements
    for num in arr:
        if num == max_val:
            buckets[-1].append(num)
        else:
            index = int((num - min_val) / bucket_range)
            buckets[index].append(num)
    
    # Sort individual buckets and concatenate
    result = []
    for bucket in buckets:
        if bucket:
            result.extend(insertion_sort(bucket, stats=stats))
    
    return result[::-1] if reverse else result


# ============================================================================
# PYTHON'S BUILT-IN SORT (TIMSORT)
# ============================================================================

def tim_sort(arr: List[Any], key: Optional[Callable] = None,
             reverse: bool = False, stats: Optional[SortStats] = None) -> List[Any]:
    """
    Tim Sort - Python's built-in sorting algorithm (hybrid of merge and insertion).
    
    Time Complexity: O(n log n) worst case, O(n) best case
    Space Complexity: O(n)
    Stable: Yes
    
    Best for: General purpose, real-world data with patterns
    Note: This uses Python's built-in sorted() which implements Timsort
    
    Args:
        arr: List to sort
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
    """
    stats = stats or SortStats()
    return sorted(arr, key=key, reverse=reverse)


# ============================================================================
# MAIN SORTING INTERFACE
# ============================================================================

def sort(arr: List[Any], algorithm: SortAlgorithm = SortAlgorithm.QUICK,
         key: Optional[Callable] = None, reverse: bool = False,
         stats: Optional[SortStats] = None) -> List[Any]:
    """
    Universal sorting function - choose algorithm by enum.
    
    Args:
        arr: List to sort
        algorithm: Sorting algorithm to use
        key: Function to extract comparison key
        reverse: Sort in descending order
        stats: Optional SortStats object to track operations
        
    Returns:
        Sorted list
        
    Example:
        >>> sort([3, 1, 4, 1, 5], SortAlgorithm.MERGE)
        [1, 1, 3, 4, 5]
    """
    algorithms = {
        SortAlgorithm.BUBBLE: bubble_sort,
        SortAlgorithm.SELECTION: selection_sort,
        SortAlgorithm.INSERTION: insertion_sort,
        SortAlgorithm.MERGE: merge_sort,
        SortAlgorithm.QUICK: quick_sort,
        SortAlgorithm.HEAP: heap_sort,
        SortAlgorithm.SHELL: shell_sort,
        SortAlgorithm.COCKTAIL: cocktail_sort,
        SortAlgorithm.COMB: comb_sort,
        SortAlgorithm.TIM: tim_sort,
    }
    
    # Special handling for non-comparison sorts
    if algorithm == SortAlgorithm.COUNTING:
        if key is None and all(isinstance(x, int) and x >= 0 for x in arr):
            return counting_sort(arr, reverse, stats)
        else:
            raise ValueError("Counting sort requires non-negative integers")
    
    if algorithm == SortAlgorithm.RADIX:
        if key is None and all(isinstance(x, int) and x >= 0 for x in arr):
            return radix_sort(arr, reverse, stats)
        else:
            raise ValueError("Radix sort requires non-negative integers")
    
    if algorithm == SortAlgorithm.BUCKET:
        if key is None:
            return bucket_sort(arr, reverse=reverse, stats=stats)
        else:
            raise ValueError("Bucket sort doesn't support key function")
    
    sort_func = algorithms.get(algorithm)
    if not sort_func:
        raise ValueError(f"Unknown algorithm: {algorithm}")
    
    return sort_func(arr, key=key, reverse=reverse, stats=stats)


# ============================================================================
# PERFORMANCE ANALYSIS
# ============================================================================

def benchmark(arr: List[Any], algorithms: Optional[List[SortAlgorithm]] = None,
              trials: int = 1) -> Dict[str, Dict[str, Any]]:
    """
    Benchmark multiple sorting algorithms.
    
    Args:
        arr: List to sort
        algorithms: List of algorithms to test (None = all applicable)
        trials: Number of trials to average
        
    Returns:
        Dictionary with algorithm names as keys and performance data as values
        
    Example:
        >>> results = benchmark([5, 2, 8, 1, 9], trials=3)
        >>> print(results['quick']['time'])
    """
    if algorithms is None:
        algorithms = list(SortAlgorithm)
    
    results = {}
    
    for algo in algorithms:
        stats = SortStats()
        times = []
        
        for _ in range(trials):
            test_arr = arr.copy()
            start = time.perf_counter()
            
            try:
                sort(test_arr, algorithm=algo, stats=stats)
                end = time.perf_counter()
                times.append(end - start)
            except (ValueError, TypeError):
                # Algorithm not applicable to this data
                continue
        
        if times:
            results[algo.value] = {
                'time': sum(times) / len(times),
                'comparisons': stats.comparisons // trials,
                'swaps': stats.swaps // trials,
            }
    
    return results


def compare_algorithms(size: int = 100, algorithms: Optional[List[SortAlgorithm]] = None) -> None:
    """
    Compare sorting algorithms on random data and print results.
    
    Args:
        size: Size of array to test
        algorithms: List of algorithms to compare (None = all)
        
    Example:
        >>> compare_algorithms(1000)
    """
    # Generate random data
    arr = [random.randint(0, 1000) for _ in range(size)]
    
    print(f"\nComparing sorting algorithms on {size} elements\n")
    print(f"{'Algorithm':<15} {'Time (s)':<12} {'Comparisons':<15} {'Swaps':<10}")
    print("-" * 55)
    
    results = benchmark(arr, algorithms)
    
    # Sort by time
    sorted_results = sorted(results.items(), key=lambda x: x[1]['time'])
    
    for algo_name, data in sorted_results:
        print(f"{algo_name:<15} {data['time']:<12.6f} "
              f"{data['comparisons']:<15} {data['swaps']:<10}")


def get_algorithm_info(algorithm: SortAlgorithm) -> Dict[str, str]:
    """
    Get information about a sorting algorithm.
    
    Args:
        algorithm: Algorithm to get info about
        
    Returns:
        Dictionary with time complexity, space complexity, and stability info
    """
    info = {
        SortAlgorithm.BUBBLE: {
            'time_best': 'O(n)',
            'time_avg': 'O(n²)',
            'time_worst': 'O(n²)',
            'space': 'O(1)',
            'stable': 'Yes',
            'use_case': 'Small datasets, educational'
        },
        SortAlgorithm.SELECTION: {
            'time_best': 'O(n²)',
            'time_avg': 'O(n²)',
            'time_worst': 'O(n²)',
            'space': 'O(1)',
            'stable': 'No',
            'use_case': 'Small datasets, minimal memory writes'
        },
        SortAlgorithm.INSERTION: {
            'time_best': 'O(n)',
            'time_avg': 'O(n²)',
            'time_worst': 'O(n²)',
            'space': 'O(1)',
            'stable': 'Yes',
            'use_case': 'Small or nearly sorted data'
        },
        SortAlgorithm.MERGE: {
            'time_best': 'O(n log n)',
            'time_avg': 'O(n log n)',
            'time_worst': 'O(n log n)',
            'space': 'O(n)',
            'stable': 'Yes',
            'use_case': 'Large datasets, stability needed'
        },
        SortAlgorithm.QUICK: {
            'time_best': 'O(n log n)',
            'time_avg': 'O(n log n)',
            'time_worst': 'O(n²)',
            'space': 'O(log n)',
            'stable': 'No',
            'use_case': 'General purpose, average case'
        },
        SortAlgorithm.HEAP: {
            'time_best': 'O(n log n)',
            'time_avg': 'O(n log n)',
            'time_worst': 'O(n log n)',
            'space': 'O(1)',
            'stable': 'No',
            'use_case': 'Guaranteed O(n log n), O(1) space'
        },
        SortAlgorithm.COUNTING: {
            'time_best': 'O(n + k)',
            'time_avg': 'O(n + k)',
            'time_worst': 'O(n + k)',
            'space': 'O(k)',
            'stable': 'Yes',
            'use_case': 'Integers with small range'
        },
        SortAlgorithm.RADIX: {
            'time_best': 'O(d(n + k))',
            'time_avg': 'O(d(n + k))',
            'time_worst': 'O(d(n + k))',
            'space': 'O(n + k)',
            'stable': 'Yes',
            'use_case': 'Fixed-width integers'
        },
        SortAlgorithm.BUCKET: {
            'time_best': 'O(n + k)',
            'time_avg': 'O(n + k)',
            'time_worst': 'O(n²)',
            'space': 'O(n + k)',
            'stable': 'Yes',
            'use_case': 'Uniformly distributed data'
        },
        SortAlgorithm.SHELL: {
            'time_best': 'O(n log n)',
            'time_avg': 'O(n^1.5)',
            'time_worst': 'O(n²)',
            'space': 'O(1)',
            'stable': 'No',
            'use_case': 'Medium datasets'
        },
        SortAlgorithm.COCKTAIL: {
            'time_best': 'O(n)',
            'time_avg': 'O(n²)',
            'time_worst': 'O(n²)',
            'space': 'O(1)',
            'stable': 'Yes',
            'use_case': 'Nearly sorted data'
        },
        SortAlgorithm.COMB: {
            'time_best': 'O(n log n)',
            'time_avg': 'O(n²/2ᵖ)',
            'time_worst': 'O(n²)',
            'space': 'O(1)',
            'stable': 'No',
            'use_case': 'Better than bubble sort'
        },
        SortAlgorithm.TIM: {
            'time_best': 'O(n)',
            'time_avg': 'O(n log n)',
            'time_worst': 'O(n log n)',
            'space': 'O(n)',
            'stable': 'Yes',
            'use_case': 'General purpose, real-world data'
        },
    }
    
    return info.get(algorithm, {})


if __name__ == '__main__':
    print("SortingLib - Comprehensive Sorting Algorithms Library\n")
    
    # Test data
    test_arrays = {
        'random': [64, 34, 25, 12, 22, 11, 90, 88, 45, 50],
        'sorted': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        'reversed': [10, 9, 8, 7, 6, 5, 4, 3, 2, 1],
        'duplicates': [5, 2, 8, 2, 9, 1, 5, 5],
    }
    
    print("=" * 70)
    print("Testing all sorting algorithms")
    print("=" * 70)
    
    for data_type, arr in test_arrays.items():
        print(f"\n{data_type.upper()} DATA: {arr}")
        print("-" * 70)
        
        for algo in [SortAlgorithm.BUBBLE, SortAlgorithm.QUICK, 
                     SortAlgorithm.MERGE, SortAlgorithm.HEAP]:
            stats = SortStats()
            result = sort(arr, algorithm=algo, stats=stats)
            print(f"{algo.value:<12} -> {result}")
    
    # Performance comparison
    print("\n" + "=" * 70)
    print("Performance Comparison")
    print("=" * 70)
    compare_algorithms(size=100)
    
    # Algorithm info
    print("\n" + "=" * 70)
    print("Quick Sort Details")
    print("=" * 70)
    info = get_algorithm_info(SortAlgorithm.QUICK)
    for key, value in info.items():
        print(f"{key}: {value}")

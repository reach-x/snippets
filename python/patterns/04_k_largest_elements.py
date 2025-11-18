"""
K LARGEST/SMALLEST ELEMENTS PATTERN - Comprehensive Guide

This pattern uses heaps or quickselect to efficiently find the k largest/smallest
elements in an array. It's fundamental for top-k problems, priority queues, and
selection algorithms.

WHEN TO USE:
- Finding k largest/smallest elements
- Top k frequent elements
- K closest points
- Kth largest element
- Merging k sorted lists/arrays
- Median finding in stream

TIME COMPLEXITY:
- Heap approach: O(n log k) - maintain heap of size k
- Quickselect: O(n) average, O(n²) worst case
- Sorting: O(n log n) - simple but not optimal

SPACE COMPLEXITY:
- Heap: O(k)
- Quickselect: O(1) with in-place partitioning

Python's heapq module provides min-heap by default.
For max-heap, negate values or use custom comparison.
"""

import heapq
from typing import List, Tuple
from collections import Counter
import random


# ============================================================================
# QUICK REFERENCE - COPY-PASTE TEMPLATES
# ============================================================================
"""
Use these minimal templates during interviews. Copy and adapt as needed.
CRITICAL: Min-heap for k largest, Max-heap (negated) for k smallest!
"""

# TEMPLATE 1: K Largest Elements (using min-heap)
def k_largest_template(nums: List[int], k: int) -> List[int]:
    """Find k largest elements using min-heap of size k."""
    if k <= 0 or k > len(nums):
        return []

    min_heap = nums[:k]
    heapq.heapify(min_heap)

    for i in range(k, len(nums)):
        if nums[i] > min_heap[0]:
            heapq.heapreplace(min_heap, nums[i])

    return min_heap


# TEMPLATE 2: K Smallest Elements (using max-heap with negation)
def k_smallest_template(nums: List[int], k: int) -> List[int]:
    """Find k smallest elements using max-heap (negated values)."""
    if k <= 0 or k > len(nums):
        return []

    max_heap = [-x for x in nums[:k]]
    heapq.heapify(max_heap)

    for i in range(k, len(nums)):
        if nums[i] < -max_heap[0]:
            heapq.heapreplace(max_heap, -nums[i])

    return [-x for x in max_heap]


# TEMPLATE 3: Kth Largest Element (single value)
def kth_largest_template(nums: List[int], k: int) -> int:
    """Find the kth largest element."""
    min_heap = nums[:k]
    heapq.heapify(min_heap)

    for i in range(k, len(nums)):
        if nums[i] > min_heap[0]:
            heapq.heapreplace(min_heap, nums[i])

    return min_heap[0]  # Top of min-heap is kth largest


# TEMPLATE 4: Top K Frequent Elements
def top_k_frequent_template(nums: List[int], k: int) -> List[int]:
    """Find k most frequent elements."""
    counter = Counter(nums)

    # Min-heap of (frequency, element) pairs of size k
    min_heap = []
    for num, freq in counter.items():
        heapq.heappush(min_heap, (freq, num))
        if len(min_heap) > k:
            heapq.heappop(min_heap)

    return [num for freq, num in min_heap]


# TEMPLATE 5: Merge K Sorted Lists
def merge_k_sorted_template(lists: List[List[int]]) -> List[int]:
    """Merge k sorted lists into one sorted list."""
    min_heap = []
    result = []

    # Initialize heap with first element from each list
    for list_index, lst in enumerate(lists):
        if lst:
            heapq.heappush(min_heap, (lst[0], list_index, 0))

    while min_heap:
        value, list_index, element_index = heapq.heappop(min_heap)
        result.append(value)

        # Add next element from same list
        if element_index + 1 < len(lists[list_index]):
            next_value = lists[list_index][element_index + 1]
            heapq.heappush(min_heap, (next_value, list_index, element_index + 1))

    return result


# ============================================================================
# PATTERN 1: K LARGEST ELEMENTS USING MIN-HEAP
# ============================================================================

def k_largest_elements_heap(numbers: List[int], k_count: int) -> List[int]:
    """
    Find k largest elements using min-heap.

    Example:
        numbers = [3, 2, 1, 5, 6, 4]
        k_largest_elements_heap(numbers, 3)  # Returns [4, 5, 6] (any order)

    Strategy: Maintain min-heap of size k
    - Heap keeps k largest elements seen so far
    - Smallest of k largest is at top (can be removed if we find larger)

    Why min-heap for k largest (seems counterintuitive!):
    - We want to keep track of k largest elements
    - When we see a new element, we compare it to the SMALLEST of our k largest
    - If new element is larger than the smallest, replace it
    - Min-heap lets us access and remove the smallest in O(log k) time

    Time Complexity: O(n log k) - n elements, each push/pop is O(log k)
    Space Complexity: O(k) - heap stores k elements

    Args:
        numbers: List of integers
        k_count: Number of largest elements to find

    Returns:
        List containing k largest elements (in any order)
    """
    # Handle edge cases
    if k_count <= 0 or not numbers:
        return []

    # Build min-heap with first k elements
    min_heap = numbers[:k_count]
    heapq.heapify(min_heap)  # O(k) to heapify

    # Process remaining elements
    for element_index in range(k_count, len(numbers)):
        current_element = numbers[element_index]

        # If current element is larger than smallest in heap, replace it
        if current_element > min_heap[0]:
            # heapreplace: pop smallest, push new element (more efficient than separate pop/push)
            heapq.heapreplace(min_heap, current_element)

    return min_heap


def kth_largest_element_heap(numbers: List[int], k_position: int) -> int:
    """
    Find kth largest element using min-heap.

    Example:
        numbers = [3, 2, 3, 1, 2, 4, 5, 5, 6]
        kth_largest_element_heap(numbers, 2)  # Returns 5 (2nd largest)

    The kth largest is the minimum in a heap of k largest elements.

    Why it works:
    - Min-heap of k largest elements has the kth largest at the top
    - The top is the smallest of the k largest = kth largest overall
    - Example: k=3, numbers=[1,2,3,4,5,6]
      Heap of 3 largest: [4,5,6], min is 4 = 3rd largest ✓

    Time Complexity: O(n log k)
    Space Complexity: O(k)

    Args:
        numbers: List of integers
        k_position: Position of element to find (1 = largest, 2 = 2nd largest, etc.)

    Returns:
        The kth largest element, or -1 if invalid k
    """
    # Validate k
    if k_position <= 0 or k_position > len(numbers):
        return -1

    # Build min-heap with first k elements
    min_heap = numbers[:k_position]
    heapq.heapify(min_heap)

    # Process remaining elements
    for element_index in range(k_position, len(numbers)):
        current_element = numbers[element_index]

        # If current element larger than smallest in heap, replace it
        if current_element > min_heap[0]:
            heapq.heapreplace(min_heap, current_element)

    # Top of heap is kth largest
    return min_heap[0]


# ============================================================================
# PATTERN 2: K SMALLEST ELEMENTS USING MAX-HEAP
# ============================================================================

def k_smallest_elements_heap(numbers: List[int], k_count: int) -> List[int]:
    """
    Find k smallest elements using max-heap.

    Example:
        numbers = [7, 10, 4, 3, 20, 15]
        k_smallest_elements_heap(numbers, 3)  # Returns [3, 4, 7] (any order)

    Strategy: Use max-heap (negate values) to keep k smallest elements.

    Why max-heap for k smallest:
    - Mirror of k largest problem
    - We want to keep k smallest elements
    - When we see a new element, compare it to the LARGEST of our k smallest
    - If new element is smaller than the largest, replace it
    - Max-heap lets us access and remove the largest in O(log k) time

    Python trick for max-heap:
    - heapq only provides min-heap
    - Negate all values: max-heap of negatives = min-heap behavior
    - Example: max-heap [5,3,1] → min-heap [-5,-3,-1]

    Time Complexity: O(n log k)
    Space Complexity: O(k)

    Args:
        numbers: List of integers
        k_count: Number of smallest elements to find

    Returns:
        List containing k smallest elements (in any order)
    """
    # Handle edge cases
    if k_count <= 0 or not numbers:
        return []

    # Python heapq is min-heap, so negate values to simulate max-heap
    max_heap = [-value for value in numbers[:k_count]]
    heapq.heapify(max_heap)

    # Process remaining elements
    for element_index in range(k_count, len(numbers)):
        current_element = numbers[element_index]

        # If current element is smaller than largest in heap (compare negated values)
        if current_element < -max_heap[0]:
            heapq.heapreplace(max_heap, -current_element)

    # Negate back to get original values
    return [-value for value in max_heap]


def kth_smallest_element_heap(numbers: List[int], k_position: int) -> int:
    """
    Find kth smallest element using max-heap.

    Example:
        numbers = [7, 10, 4, 3, 20, 15]
        kth_smallest_element_heap(numbers, 2)  # Returns 4 (2nd smallest)

    Time Complexity: O(n log k)
    Space Complexity: O(k)

    Args:
        numbers: List of integers
        k_position: Position of element to find (1 = smallest, 2 = 2nd smallest, etc.)

    Returns:
        The kth smallest element, or -1 if invalid k
    """
    # Validate k
    if k_position <= 0 or k_position > len(numbers):
        return -1

    # Build max-heap (negated) with first k elements
    max_heap = [-value for value in numbers[:k_position]]
    heapq.heapify(max_heap)

    # Process remaining elements
    for element_index in range(k_position, len(numbers)):
        current_element = numbers[element_index]

        # If current smaller than largest in heap
        if current_element < -max_heap[0]:
            heapq.heapreplace(max_heap, -current_element)

    # Negate back to get original value
    return -max_heap[0]


# ============================================================================
# PATTERN 3: QUICKSELECT ALGORITHM
# ============================================================================

def quickselect(numbers: List[int], k_position: int) -> int:
    """
    Find kth largest element using quickselect (Hoare's selection algorithm).

    Example:
        numbers = [3, 2, 3, 1, 2, 4, 5, 5, 6]
        quickselect(numbers, 4)  # Returns 4 (4th largest)

    Strategy: Similar to quicksort, but only recurse on one partition.
    - Partition array around pivot (elements > pivot go left for kth largest)
    - If pivot is at position k-1, we found kth largest
    - Otherwise, recurse on appropriate partition (left or right)

    Why it works:
    - Partitioning puts pivot in its final sorted position
    - Elements to the left are larger (for kth largest variant)
    - Elements to the right are smaller
    - If pivot lands at k-1, it's the kth largest element
    - Otherwise, we know which side to search

    Why faster than sorting:
    - Sorting: O(n log n) - sorts entire array
    - Quickselect: O(n) average - only partitions one side each time
    - Average case: n + n/2 + n/4 + ... ≈ 2n = O(n)

    Time Complexity: O(n) average, O(n²) worst case (random pivot helps avoid worst case)
    Space Complexity: O(1) with in-place partitioning

    Args:
        numbers: List of integers (will be modified)
        k_position: Position of element to find (1 = largest, 2 = 2nd largest, etc.)

    Returns:
        The kth largest element
    """
    def partition(left_index: int, right_index: int, pivot_index: int) -> int:
        """
        Partition array around pivot for kth largest.
        Returns final pivot position after partitioning.
        """
        pivot_value = numbers[pivot_index]

        # Move pivot to end temporarily
        numbers[pivot_index], numbers[right_index] = numbers[right_index], numbers[pivot_index]

        # Partition: move elements larger than pivot to left
        store_index = left_index
        for current_index in range(left_index, right_index):
            # For kth largest: put larger elements on left
            if numbers[current_index] > pivot_value:
                numbers[store_index], numbers[current_index] = numbers[current_index], numbers[store_index]
                store_index += 1

        # Move pivot to its final position
        numbers[right_index], numbers[store_index] = numbers[store_index], numbers[right_index]

        return store_index

    def select(left_index: int, right_index: int, k_smallest_position: int) -> int:
        """
        Recursively select kth smallest element from numbers[left:right+1].
        """
        # Base case: only one element
        if left_index == right_index:
            return numbers[left_index]

        # Choose random pivot to avoid worst case O(n²)
        pivot_index = random.randint(left_index, right_index)

        # Partition and get pivot's final position
        pivot_index = partition(left_index, right_index, pivot_index)

        # Check if we found the kth element
        if k_smallest_position == pivot_index:
            # Pivot is at the position we want!
            return numbers[k_smallest_position]
        elif k_smallest_position < pivot_index:
            # kth element is in left partition (larger elements)
            return select(left_index, pivot_index - 1, k_smallest_position)
        else:
            # kth element is in right partition (smaller elements)
            return select(pivot_index + 1, right_index, k_smallest_position)

    # Find kth largest = (k-1)th index in descending order
    return select(0, len(numbers) - 1, k_position - 1)


# ============================================================================
# PATTERN 4: TOP K FREQUENT ELEMENTS
# ============================================================================

def top_k_frequent_heap(numbers: List[int], k_count: int) -> List[int]:
    """
    Find k most frequent elements using min-heap.

    Example:
        numbers = [1, 1, 1, 2, 2, 3]
        top_k_frequent_heap(numbers, 2)  # Returns [1, 2]
        (1 appears 3 times, 2 appears 2 times, 3 appears 1 time)

    Strategy:
    1. Count frequencies using Counter
    2. Maintain min-heap of size k with (frequency, element) pairs
    3. Heap maintains k most frequent elements
    4. Extract elements from final heap

    Why min-heap for k most frequent:
    - Same logic as k largest elements
    - Want to keep k highest frequencies
    - Compare new frequency to smallest frequency in heap
    - If larger, replace it

    Time Complexity: O(n log k) - n unique elements, k-size heap
    Space Complexity: O(n) for counter + O(k) for heap

    Args:
        numbers: List of integers with duplicates
        k_count: Number of most frequent elements to find

    Returns:
        List of k most frequent elements
    """
    # Count frequencies of each number
    frequency_map = Counter(numbers)

    # Use min-heap to keep k most frequent elements
    min_heap = []

    # Process each unique number and its frequency
    for number, frequency in frequency_map.items():
        # Push (frequency, number) tuple to heap
        heapq.heappush(min_heap, (frequency, number))

        # If heap exceeds size k, remove element with smallest frequency
        if len(min_heap) > k_count:
            heapq.heappop(min_heap)

    # Extract just the numbers (not frequencies) from heap
    return [number for frequency, number in min_heap]


def top_k_frequent_bucket_sort(numbers: List[int], k_count: int) -> List[int]:
    """
    Find k most frequent elements using bucket sort.

    Example:
        numbers = [1, 1, 1, 2, 2, 3]
        top_k_frequent_bucket_sort(numbers, 2)  # Returns [1, 2]

    Strategy: Use frequency as bucket index.
    - Bucket i contains all elements with frequency i
    - Maximum possible frequency is len(numbers)
    - Iterate buckets from high to low frequency
    - Collect first k elements

    Why bucket sort works here:
    - Frequencies are bounded: 1 to len(numbers)
    - Can use frequency as direct index
    - O(n) to count, O(n) to bucket, O(n) to collect = O(n) total!

    When to use vs heap:
    - Bucket sort: O(n) but uses O(n) space
    - Heap: O(n log k) but uses O(k) space
    - If k << n, bucket sort might use more space but is faster

    Time Complexity: O(n) - linear time!
    Space Complexity: O(n) - buckets array + counter

    Args:
        numbers: List of integers with duplicates
        k_count: Number of most frequent elements to find

    Returns:
        List of k most frequent elements
    """
    # Count frequencies
    frequency_map = Counter(numbers)

    # Create buckets: bucket[i] = list of elements with frequency i
    # Index represents frequency, value is list of numbers with that frequency
    buckets = [[] for _ in range(len(numbers) + 1)]

    # Place each number in appropriate frequency bucket
    for number, frequency in frequency_map.items():
        buckets[frequency].append(number)

    # Collect k most frequent from high to low frequency
    result = []
    for frequency_index in range(len(buckets) - 1, 0, -1):  # Iterate from high to low
        for number in buckets[frequency_index]:
            result.append(number)
            # Stop when we have k elements
            if len(result) == k_count:
                return result

    return result


# ============================================================================
# PATTERN 5: K CLOSEST POINTS TO ORIGIN
# ============================================================================

def k_closest_points(points: List[List[int]], k_count: int) -> List[List[int]]:
    """
    Find k closest points to origin (0, 0) using max-heap.

    Example:
        points = [[1, 3], [-2, 2], [5, 8], [0, 1]]
        k_closest_points(points, 2)  # Returns [[0, 1], [-2, 2]] (any order)

    Distance from origin: sqrt(x² + y²)
    Since we're only comparing distances, we can use x² + y² (avoid expensive sqrt)

    Strategy: Maintain max-heap of k closest points.
    - Keep k smallest distances
    - Use max-heap to track largest distance in our k smallest
    - If new point closer than largest, replace it

    Why max-heap:
    - Finding k smallest distances = same as k smallest elements
    - Need to compare to largest of k smallest
    - Max-heap gives us largest in O(1), remove it in O(log k)

    Time Complexity: O(n log k) - n points, k-size heap
    Space Complexity: O(k) - heap stores k points

    Args:
        points: List of [x, y] coordinate pairs
        k_count: Number of closest points to find

    Returns:
        List of k points closest to origin
    """
    def distance_squared(point: List[int]) -> int:
        """Calculate squared distance from origin (avoids sqrt for efficiency)."""
        x_coordinate, y_coordinate = point[0], point[1]
        return x_coordinate ** 2 + y_coordinate ** 2

    # Max-heap: store (-distance, point) to simulate max-heap with min-heap
    max_heap = []

    for point in points:
        squared_distance = distance_squared(point)

        # If heap not full yet, add point
        if len(max_heap) < k_count:
            # Negate distance for max-heap behavior
            heapq.heappush(max_heap, (-squared_distance, point))

        # If this point is closer than the farthest point in heap
        elif squared_distance < -max_heap[0][0]:
            # Replace farthest point with this closer point
            heapq.heapreplace(max_heap, (-squared_distance, point))

    # Extract just the points (not distances) from heap
    return [point for distance, point in max_heap]


# ============================================================================
# PATTERN 6: MERGE K SORTED LISTS
# ============================================================================

class ListNode:
    """Simple linked list node for merge k sorted lists problem."""
    def __init__(self, value=0, next_node=None):
        self.value = value
        self.next = next_node


def merge_k_sorted_lists(lists: List[ListNode]) -> ListNode:
    """
    Merge k sorted linked lists using min-heap.

    Example:
        lists = [
            1 -> 4 -> 5,
            1 -> 3 -> 4,
            2 -> 6
        ]
        Result: 1 -> 1 -> 2 -> 3 -> 4 -> 4 -> 5 -> 6

    Strategy:
    1. Add first node from each list to min-heap
    2. Pop minimum node, add to result
    3. Add next node from same list to heap
    4. Repeat until heap empty

    Why heap:
    - Need to repeatedly find minimum among k elements
    - Each list's current node is a candidate for next minimum
    - Heap finds minimum in O(log k)

    Time Complexity: O(n log k) where n = total nodes across all lists
    Space Complexity: O(k) - heap stores at most k nodes

    Args:
        lists: List of ListNode heads (each list is sorted)

    Returns:
        Head of merged sorted list
    """
    min_heap = []

    # Add first node from each list to heap
    for list_index, node in enumerate(lists):
        if node:
            # Tuple: (node_value, list_index, node)
            # list_index used to break ties when values equal
            heapq.heappush(min_heap, (node.value, list_index, node))

    # Create dummy head for result list
    dummy_head = ListNode(0)
    current_node = dummy_head

    # Process nodes in sorted order
    while min_heap:
        # Pop node with minimum value
        node_value, list_index, node = heapq.heappop(min_heap)

        # Add to result list
        current_node.next = node
        current_node = current_node.next

        # Add next node from same list to heap
        if node.next:
            heapq.heappush(min_heap, (node.next.value, list_index, node.next))

    return dummy_head.next


def merge_k_sorted_arrays(arrays: List[List[int]]) -> List[int]:
    """
    Merge k sorted arrays using min-heap.

    Example:
        arrays = [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
        Result: [1, 2, 3, 4, 5, 6, 7, 8, 9]

    Strategy: Same as merge k sorted lists, but with arrays.
    - Track: (value, array_index, element_index)
    - Pop minimum, add next element from same array

    Time Complexity: O(n log k) where n = total elements across all arrays
    Space Complexity: O(k) - heap stores at most k elements + O(n) for result

    Args:
        arrays: List of sorted arrays

    Returns:
        Single merged sorted array
    """
    min_heap = []

    # Add first element from each array: (value, array_index, element_index)
    for array_index, array in enumerate(arrays):
        if array:
            # Tuple: (first_value, which_array, position_in_array)
            heapq.heappush(min_heap, (array[0], array_index, 0))

    result = []

    # Process elements in sorted order
    while min_heap:
        # Pop minimum element
        current_value, array_index, element_index = heapq.heappop(min_heap)
        result.append(current_value)

        # Add next element from same array
        if element_index + 1 < len(arrays[array_index]):
            next_value = arrays[array_index][element_index + 1]
            heapq.heappush(min_heap, (next_value, array_index, element_index + 1))

    return result


# ============================================================================
# PATTERN 7: KTH LARGEST IN STREAM
# ============================================================================

class KthLargest:
    """
    Find kth largest element in a stream using min-heap.

    Example:
        kth_largest = KthLargest(3, [4, 5, 8, 2])
        kth_largest.add(3)   # Returns 4 (3rd largest from [4,5,8,2,3])
        kth_largest.add(5)   # Returns 5 (3rd largest from [4,5,8,2,3,5])
        kth_largest.add(10)  # Returns 5 (3rd largest from [4,5,8,2,3,5,10])

    Strategy: Maintain min-heap of size k with k largest elements.
    - The top of heap is always the kth largest
    - When adding new element, if larger than top, replace top
    - Heap automatically maintains k largest elements

    Why it works for streaming:
    - Don't need to store all elements, just k largest
    - Each add is O(log k)
    - Top of heap is always current answer in O(1)

    Time Complexity: O(log k) per add operation
    Space Complexity: O(k) - heap stores k elements
    """

    def __init__(self, k_position: int, initial_numbers: List[int]):
        """
        Initialize with k and initial array of numbers.

        Args:
            k_position: Position to track (1 = largest, 2 = 2nd largest, etc.)
            initial_numbers: Initial stream of numbers
        """
        self.k_position = k_position
        self.min_heap = []

        # Add all initial numbers to heap
        for number in initial_numbers:
            self.add(number)

    def add(self, value: int) -> int:
        """
        Add new value to stream and return current kth largest.

        Args:
            value: New number to add to stream

        Returns:
            Current kth largest element
        """
        # If heap not full yet, just add the value
        if len(self.min_heap) < self.k_position:
            heapq.heappush(self.min_heap, value)

        # If new value larger than smallest in heap (kth largest), replace it
        elif value > self.min_heap[0]:
            heapq.heapreplace(self.min_heap, value)

        # Top of heap is kth largest
        return self.min_heap[0]


# ============================================================================
# COMPARISON: HEAP VS QUICKSELECT VS SORTING
# ============================================================================

def comparison_demo(numbers: List[int], k_position: int):
    """
    Compare different approaches for finding kth largest element.

    Demonstrates when to use each approach with complexity analysis.
    """
    print(f"\nFinding {k_position}th largest in {numbers}")
    print("-" * 60)

    # Approach 1: Sorting (simple but not optimal)
    sorted_numbers = sorted(numbers, reverse=True)
    result_sorting = sorted_numbers[k_position-1] if k_position <= len(numbers) else None
    print(f"1. Sorting:        {result_sorting}")
    print(f"   Time: O(n log n), Space: O(n)")
    print(f"   When to use: Need all elements sorted anyway")

    # Approach 2: Min-Heap (good for small k)
    result_heap = kth_largest_element_heap(numbers[:], k_position)
    print(f"\n2. Min-Heap:       {result_heap}")
    print(f"   Time: O(n log k), Space: O(k)")
    print(f"   When to use: k << n, streaming data, or finding top-k elements")

    # Approach 3: Quickselect (fastest average case)
    result_quickselect = quickselect(numbers[:], k_position)
    print(f"\n3. Quickselect:    {result_quickselect}")
    print(f"   Time: O(n) average, Space: O(1)")
    print(f"   When to use: Finding single kth element, can modify array")

    print("\n" + "-" * 60)
    print("RECOMMENDATION:")
    if k_position <= 5:
        print(f"  For k={k_position} (small): Heap or Quickselect")
    elif k_position >= len(numbers) - 5:
        print(f"  For k={k_position} (large): Quickselect or Sorting")
    else:
        print(f"  For k={k_position} (medium): Quickselect (fastest)")


# ============================================================================
# TEST CASES
# ============================================================================

def run_tests():
    """
    Comprehensive test cases demonstrating all k largest/smallest patterns.
    """
    print("=" * 70)
    print("K LARGEST/SMALLEST ELEMENTS PATTERN - COMPREHENSIVE TEST CASES")
    print("=" * 70)

    # Test 1: K Largest Elements
    print("\n1. K LARGEST ELEMENTS (k=3)")
    print("-" * 70)
    test_array_1 = [3, 2, 1, 5, 6, 4]
    print(f"   Input: {test_array_1}")
    result_1 = sorted(k_largest_elements_heap(test_array_1, 3), reverse=True)
    print(f"   Output: {result_1}")
    print(f"   Expected: [6, 5, 4]")
    print(f"\n   Why: Using min-heap of size 3 keeps 3 largest elements")
    print(f"   Heap maintains: smallest of 3 largest at top for easy comparison")

    # Test 2: Kth Largest Element
    print("\n2. KTH LARGEST ELEMENT (k=2)")
    print("-" * 70)
    test_array_2 = [3, 2, 3, 1, 2, 4, 5, 5, 6]
    print(f"   Input: {test_array_2}")
    heap_result = kth_largest_element_heap(test_array_2, 2)
    quickselect_result = quickselect(test_array_2[:], 2)
    print(f"   Heap approach: {heap_result}")
    print(f"   Quickselect approach: {quickselect_result}")
    print(f"   Expected: 5 (sorted desc: [6,5,5,4,3,3,2,2,1], 2nd is 5)")
    print(f"\n   Why: 6 is largest, 5 is second largest")

    # Test 3: K Smallest Elements
    print("\n3. K SMALLEST ELEMENTS (k=3)")
    print("-" * 70)
    test_array_3 = [7, 10, 4, 3, 20, 15]
    print(f"   Input: {test_array_3}")
    result_3 = sorted(k_smallest_elements_heap(test_array_3, 3))
    print(f"   Output: {result_3}")
    print(f"   Expected: [3, 4, 7]")
    print(f"\n   Why: Using max-heap (negated values) keeps 3 smallest")
    print(f"   Largest of 3 smallest at top for comparison")

    # Test 4: Top K Frequent
    print("\n4. TOP K FREQUENT ELEMENTS (k=2)")
    print("-" * 70)
    test_array_4 = [1, 1, 1, 2, 2, 3]
    print(f"   Input: {test_array_4}")
    heap_result_4 = sorted(top_k_frequent_heap(test_array_4, 2))
    bucket_result_4 = sorted(top_k_frequent_bucket_sort(test_array_4, 2))
    print(f"   Heap approach: {heap_result_4}")
    print(f"   Bucket sort approach: {bucket_result_4}")
    print(f"   Expected: [1, 2]")
    print(f"\n   Why: 1 appears 3 times, 2 appears 2 times, 3 appears 1 time")
    print(f"   Frequencies: {Counter(test_array_4)}")

    # Test 5: K Closest Points
    print("\n5. K CLOSEST POINTS TO ORIGIN (k=2)")
    print("-" * 70)
    test_points_5 = [[1, 3], [-2, 2], [5, 8], [0, 1]]
    print(f"   Input: {test_points_5}")
    result_5 = k_closest_points(test_points_5, 2)
    print(f"   Output: {result_5}")
    print(f"   Expected: [[0,1], [-2,2]] in any order")
    print(f"\n   Distances: [1,3]→10, [-2,2]→8, [5,8]→89, [0,1]→1")
    print(f"   Closest 2: [0,1] (distance 1) and [-2,2] (distance 8)")

    # Test 6: Merge K Sorted Arrays
    print("\n6. MERGE K SORTED ARRAYS")
    print("-" * 70)
    test_arrays_6 = [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
    print(f"   Input: {test_arrays_6}")
    result_6 = merge_k_sorted_arrays(test_arrays_6)
    print(f"   Output: {result_6}")
    print(f"   Expected: [1, 2, 3, 4, 5, 6, 7, 8, 9]")
    print(f"\n   Why: Heap always extracts minimum among current elements")
    print(f"   Each array contributes one element at a time")

    # Test 7: Kth Largest in Stream
    print("\n7. KTH LARGEST IN STREAM (k=3)")
    print("-" * 70)
    stream_tracker = KthLargest(3, [4, 5, 8, 2])
    print(f"   Initial array: [4, 5, 8, 2], k=3")
    print(f"   Current 3 largest: [8, 5, 4], 3rd largest = 4")

    result_add_3 = stream_tracker.add(3)
    print(f"\n   Add 3 → {result_add_3} (Expected: 4)")
    print(f"   Array now: [4,5,8,2,3], 3 largest: [8,5,4]")

    result_add_5 = stream_tracker.add(5)
    print(f"   Add 5 → {result_add_5} (Expected: 5)")
    print(f"   Array now: [4,5,8,2,3,5], 3 largest: [8,5,5]")

    result_add_10 = stream_tracker.add(10)
    print(f"   Add 10 → {result_add_10} (Expected: 5)")
    print(f"   Array now: [4,5,8,2,3,5,10], 3 largest: [10,8,5]")

    # Test 8: Comparison Demo
    print("\n8. ALGORITHM COMPARISON")
    print("=" * 70)
    comparison_demo([3, 2, 3, 1, 2, 4, 5, 5, 6], 4)

    # Summary
    print("\n" + "=" * 70)
    print("KEY INSIGHTS - K LARGEST/SMALLEST PATTERNS")
    print("=" * 70)
    print("1. K largest → use MIN-heap (remove smallest when heap size > k)")
    print("2. K smallest → use MAX-heap (remove largest when heap size > k)")
    print("3. Python heapq is min-heap; negate values for max-heap behavior")
    print("4. Heap approach: O(n log k) - efficient when k << n")
    print("5. Quickselect: O(n) average - best for single kth element")
    print("6. For streams: maintain heap, top element is always answer")
    print("7. Merge k sorted: use heap with (value, source_index, position)")
    print("8. Top k frequent: can use heap O(n log k) or bucket sort O(n)")
    print("=" * 70)


if __name__ == "__main__":
    run_tests()

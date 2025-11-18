"""
20. Kth Largest Element in an Array

Given an integer array nums and an integer k, return the kth largest element
in the array. Note that it is the kth largest element in the sorted order,
not the kth distinct element.

LeetCode: https://leetcode.com/problems/kth-largest-element-in-an-array/
Difficulty: Medium
Pattern: Heap, Quick Select
"""

from typing import List
import heapq
import random


def find_kth_largest_sorting(nums: List[int], k: int) -> int:
    """
    Sorting approach.

    Time Complexity: O(n log n)
    Space Complexity: O(1) or O(n) depending on sort implementation
    """
    nums.sort(reverse=True)
    return nums[k - 1]


def find_kth_largest_heap(nums: List[int], k: int) -> int:
    """
    Min heap approach - maintain k largest elements.

    Time Complexity: O(n log k)
    Space Complexity: O(k)
    """
    # Python's heapq is a min heap
    heap = []

    for num in nums:
        heapq.heappush(heap, num)
        if len(heap) > k:
            heapq.heappop(heap)

    return heap[0]  # Top of min heap is kth largest


def find_kth_largest_heap_variant(nums: List[int], k: int) -> int:
    """
    Alternative heap approach using heapify and nlargest.

    Time Complexity: O(n log k)
    Space Complexity: O(k)
    """
    return heapq.nlargest(k, nums)[-1]


def find_kth_largest_quickselect(nums: List[int], k: int) -> int:
    """
    Quick Select approach (optimal average case).

    Time Complexity: O(n) average, O(n^2) worst case
    Space Complexity: O(1)
    """
    def partition(left: int, right: int) -> int:
        # Choose random pivot to avoid worst case
        pivot_idx = random.randint(left, right)
        nums[pivot_idx], nums[right] = nums[right], nums[pivot_idx]
        pivot = nums[right]

        i = left
        for j in range(left, right):
            if nums[j] >= pivot:  # For kth largest, use >= instead of <=
                nums[i], nums[j] = nums[j], nums[i]
                i += 1

        nums[i], nums[right] = nums[right], nums[i]
        return i

    def select(left: int, right: int, k: int) -> int:
        if left == right:
            return nums[left]

        pivot_idx = partition(left, right)

        if pivot_idx == k - 1:
            return nums[pivot_idx]
        elif pivot_idx > k - 1:
            return select(left, pivot_idx - 1, k)
        else:
            return select(pivot_idx + 1, right, k)

    return select(0, len(nums) - 1, k)


# Alias for optimal solution
find_kth_largest = find_kth_largest_heap


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [3, 2, 1, 5, 6, 4],
            'k': 2,
            'expected': 5,
            'description': 'Basic case - 2nd largest'
        },
        {
            'nums': [3, 2, 3, 1, 2, 4, 5, 5, 6],
            'k': 4,
            'expected': 4,
            'description': 'With duplicates - 4th largest'
        },
        {
            'nums': [1],
            'k': 1,
            'expected': 1,
            'description': 'Single element'
        },
        {
            'nums': [7, 6, 5, 4, 3, 2, 1],
            'k': 1,
            'expected': 7,
            'description': 'Largest element'
        },
        {
            'nums': [7, 6, 5, 4, 3, 2, 1],
            'k': 7,
            'expected': 1,
            'description': 'Smallest element (7th largest)'
        },
        {
            'nums': [2, 1],
            'k': 1,
            'expected': 2,
            'description': 'Two elements'
        }
    ]

    print("=" * 70)
    print("KTH LARGEST ELEMENT - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        result_sort = find_kth_largest_sorting(test['nums'][:], test['k'])
        result_heap = find_kth_largest_heap(test['nums'][:], test['k'])
        result_heap_var = find_kth_largest_heap_variant(test['nums'][:], test['k'])
        result_quickselect = find_kth_largest_quickselect(test['nums'][:], test['k'])

        passed = (result_sort == test['expected'] and
                 result_heap == test['expected'] and
                 result_heap_var == test['expected'] and
                 result_quickselect == test['expected'])

        print(f"\nTest {i}: {test['description']}")
        print(f"Array: {test['nums']}")
        print(f"k: {test['k']}")
        print(f"Sorted: {sorted(test['nums'], reverse=True)}")
        print(f"Expected: {test['expected']}")
        print(f"Got (Sorting): {result_sort}")
        print(f"Got (Min Heap): {result_heap}")
        print(f"Got (Heap Variant): {result_heap_var}")
        print(f"Got (Quick Select): {result_quickselect}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("MIN HEAP APPROACH EXPLANATION")
    print("=" * 70)
    print("""
Maintain a min heap of size k:
1. Add elements to heap one by one
2. If heap size exceeds k, remove smallest element
3. After processing all elements, top of heap is kth largest

Why it works:
- Min heap keeps k largest elements
- Smallest of these k elements is kth largest overall
- Top of min heap = smallest in heap = kth largest

Example: nums = [3,2,1,5,6,4], k = 2
- After processing: heap = [5, 6]
- Top of heap (5) is 2nd largest
""")

    print("\n" + "=" * 70)
    print("APPROACH COMPARISON")
    print("=" * 70)
    print("Sorting: Time O(n log n), Space O(1)")
    print("Min Heap: Time O(n log k), Space O(k) - GOOD for small k")
    print("Quick Select: Time O(n) avg, O(n^2) worst, Space O(1) - OPTIMAL")
    print("\nPython heapq functions:")
    print("- heappush(heap, item): Add item")
    print("- heappop(heap): Remove and return smallest")
    print("- heapify(list): Transform list into heap")
    print("- nlargest(k, list): Get k largest elements")

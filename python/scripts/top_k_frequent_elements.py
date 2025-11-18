"""
Top K Frequent Elements

Given an integer array nums and an integer k, return the k most frequent elements.
You may return the answer in any order.

LeetCode: https://leetcode.com/problems/top-k-frequent-elements/
Difficulty: Medium
Pattern: Heap / Hash Map / Bucket Sort
"""

from typing import List
from collections import Counter
import heapq


def top_k_frequent_sorting(nums: List[int], k: int) -> List[int]:
    """
    Sorting approach: Count frequencies, then sort by count.

    Time Complexity: O(n log n)
    Space Complexity: O(n)
    """
    # Count frequencies
    freq_map = Counter(nums)

    # Sort by frequency (descending)
    sorted_items = sorted(freq_map.items(), key=lambda x: x[1], reverse=True)

    # Return top k elements
    return [num for num, freq in sorted_items[:k]]


def top_k_frequent_heap(nums: List[int], k: int) -> List[int]:
    """
    Min heap approach: Maintain heap of size k.

    Time Complexity: O(n log k)
    Space Complexity: O(n)

    Why it works:
    - Count frequencies O(n)
    - Use min heap of size k to track top k elements
    - For each element, if heap size < k or freq > min freq, add to heap
    - Heap maintains k largest frequencies
    """
    # Count frequencies
    freq_map = Counter(nums)

    # Min heap of size k (stores tuples of (frequency, number))
    heap = []

    for num, freq in freq_map.items():
        heapq.heappush(heap, (freq, num))

        # Keep heap size at most k
        if len(heap) > k:
            heapq.heappop(heap)

    # Extract numbers from heap
    return [num for freq, num in heap]


def top_k_frequent(nums: List[int], k: int) -> List[int]:
    """
    Bucket sort approach (optimal for this problem).

    Time Complexity: O(n)
    Space Complexity: O(n)

    Why it works:
    - Count frequencies O(n)
    - Create buckets where index = frequency
    - Maximum frequency is n (all elements same)
    - Iterate buckets from high to low frequency
    - Collect k elements
    """
    # Count frequencies
    freq_map = Counter(nums)

    # Create buckets: bucket[i] = list of numbers with frequency i
    # Maximum frequency is len(nums)
    buckets = [[] for _ in range(len(nums) + 1)]

    for num, freq in freq_map.items():
        buckets[freq].append(num)

    # Collect top k from highest frequency buckets
    result = []
    for freq in range(len(buckets) - 1, 0, -1):
        for num in buckets[freq]:
            result.append(num)
            if len(result) == k:
                return result

    return result


def top_k_frequent_quickselect(nums: List[int], k: int) -> List[int]:
    """
    QuickSelect approach (average O(n), worst O(n^2)).

    Time Complexity: O(n) average, O(n^2) worst case
    Space Complexity: O(n)

    Why it works:
    - Similar to quicksort partitioning
    - Instead of fully sorting, we partition until we have k elements
    - More complex but theoretically optimal
    """
    freq_map = Counter(nums)
    unique_nums = list(freq_map.keys())

    def partition(left: int, right: int, pivot_index: int) -> int:
        """Partition and return final position of pivot."""
        pivot_freq = freq_map[unique_nums[pivot_index]]

        # Move pivot to end
        unique_nums[pivot_index], unique_nums[right] = unique_nums[right], unique_nums[pivot_index]

        # Move all elements with higher frequency to left
        store_index = left
        for i in range(left, right):
            if freq_map[unique_nums[i]] > pivot_freq:
                unique_nums[store_index], unique_nums[i] = unique_nums[i], unique_nums[store_index]
                store_index += 1

        # Move pivot to final position
        unique_nums[right], unique_nums[store_index] = unique_nums[store_index], unique_nums[right]

        return store_index

    def quickselect(left: int, right: int, k_smallest: int) -> None:
        """Find k elements with highest frequency."""
        if left == right:
            return

        # Random pivot for better average performance
        pivot_index = left + (right - left) // 2
        pivot_index = partition(left, right, pivot_index)

        if k_smallest == pivot_index:
            return
        elif k_smallest < pivot_index:
            quickselect(left, pivot_index - 1, k_smallest)
        else:
            quickselect(pivot_index + 1, right, k_smallest)

    n = len(unique_nums)
    quickselect(0, n - 1, k)

    return unique_nums[:k]


def top_k_frequent_with_frequencies(nums: List[int], k: int) -> List[tuple]:
    """
    Return top k elements with their frequencies.
    """
    freq_map = Counter(nums)

    # Use max heap (negate frequencies for Python's min heap)
    heap = [(-freq, num) for num, freq in freq_map.items()]
    heapq.heapify(heap)

    # Extract top k
    result = []
    for _ in range(k):
        neg_freq, num = heapq.heappop(heap)
        result.append((num, -neg_freq))

    return result


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'nums': [1, 1, 1, 2, 2, 3],
            'k': 2,
            'expected': {1, 2},
            'description': 'Two most frequent: 1 (3x) and 2 (2x)'
        },
        {
            'nums': [1],
            'k': 1,
            'expected': {1},
            'description': 'Single element'
        },
        {
            'nums': [4, 1, -1, 2, -1, 2, 3],
            'k': 2,
            'expected': {-1, 2},
            'description': 'Negative numbers, tied frequencies'
        },
        {
            'nums': [1, 2, 3, 4, 5],
            'k': 3,
            'expected': {1, 2, 3},  # Any 3 are valid (all freq = 1)
            'description': 'All same frequency'
        },
        {
            'nums': [5, 5, 5, 3, 3, 2],
            'k': 1,
            'expected': {5},
            'description': 'Single most frequent'
        }
    ]

    print("=" * 60)
    print("TOP K FREQUENT ELEMENTS - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = top_k_frequent(test['nums'], test['k'])
        result_set = set(result)

        # Check if result is valid (for ties, multiple answers possible)
        freq_map = Counter(test['nums'])
        all_freqs = sorted(freq_map.values(), reverse=True)
        kth_freq = all_freqs[test['k'] - 1] if test['k'] <= len(all_freqs) else 0

        # Valid if all returned elements have frequency >= kth_freq
        valid = all(freq_map[num] >= kth_freq for num in result)
        valid = valid and len(result) == test['k']

        print(f"\nTest {i}: {test['description']}")
        print(f"Input: nums = {test['nums']}, k = {test['k']}")
        print(f"Expected (one valid answer): {test['expected']}")
        print(f"Got: {result_set}")

        # Show frequencies
        with_freq = top_k_frequent_with_frequencies(test['nums'], test['k'])
        print(f"With frequencies: {with_freq}")

        print(f"Status: {'PASS' if valid else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Sorting: Time O(n log n), Space O(n)")
    print("Min Heap: Time O(n log k), Space O(n)")
    print("Bucket Sort (Optimal): Time O(n), Space O(n)")
    print("QuickSelect: Time O(n) average, Space O(n)")
    print("\nKey Insight: Frequency range is bounded by n,")
    print("so bucket sort achieves linear time!")

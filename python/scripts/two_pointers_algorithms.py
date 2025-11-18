"""
Two Pointers Algorithm - Comprehensive Examples with Variations

The two pointers technique is a common algorithmic pattern where two pointers
are used to traverse a data structure, typically an array or string.

Categories:
1. Opposite Direction Pointers (converging)
2. Same Direction Pointers (slow/fast)
3. Sliding Window (variable/fixed size)
4. Multiple Arrays
"""


# ==============================================================================
# CATEGORY 1: OPPOSITE DIRECTION POINTERS (Converging from both ends)
# ==============================================================================

def two_sum_sorted(numbers, target):
    """
    Find two numbers in a sorted array that add up to target.
    Time: O(n), Space: O(1)
    """
    left = 0
    right = len(numbers) - 1

    while left < right:
        current_sum = numbers[left] + numbers[right]

        if current_sum == target:
            return [left, right]
        elif current_sum < target:
            left += 1
        else:
            right -= 1

    return [-1, -1]


def is_palindrome(s):
    """
    Check if string is a palindrome (ignoring non-alphanumeric characters).
    Time: O(n), Space: O(1)
    """
    left = 0
    right = len(s) - 1

    while left < right:
        # Skip non-alphanumeric characters
        while left < right and not s[left].isalnum():
            left += 1
        while left < right and not s[right].isalnum():
            right -= 1

        if s[left].lower() != s[right].lower():
            return False

        left += 1
        right -= 1

    return True


def reverse_string(s):
    """
    Reverse a string in-place using two pointers.
    Time: O(n), Space: O(1)
    """
    chars = list(s)
    left = 0
    right = len(chars) - 1

    while left < right:
        chars[left], chars[right] = chars[right], chars[left]
        left += 1
        right -= 1

    return ''.join(chars)


def reverse_words_in_string(s):
    """
    Reverse each word in a string while maintaining word order.
    Time: O(n), Space: O(n)
    """
    words = s.split()
    result = []

    for word in words:
        left = 0
        right = len(word) - 1
        chars = list(word)

        while left < right:
            chars[left], chars[right] = chars[right], chars[left]
            left += 1
            right -= 1

        result.append(''.join(chars))

    return ' '.join(result)


def container_with_most_water(heights):
    """
    Find two lines that together with x-axis forms a container with most water.
    Time: O(n), Space: O(1)
    """
    left = 0
    right = len(heights) - 1
    max_area = 0

    while left < right:
        width = right - left
        height = min(heights[left], heights[right])
        area = width * height
        max_area = max(max_area, area)

        # Move pointer pointing to shorter line
        if heights[left] < heights[right]:
            left += 1
        else:
            right -= 1

    return max_area


def trapping_rain_water(heights):
    """
    Calculate how much water can be trapped after raining.
    Time: O(n), Space: O(1)
    """
    if not heights:
        return 0

    left = 0
    right = len(heights) - 1
    left_max = 0
    right_max = 0
    water = 0

    while left < right:
        if heights[left] < heights[right]:
            if heights[left] >= left_max:
                left_max = heights[left]
            else:
                water += left_max - heights[left]
            left += 1
        else:
            if heights[right] >= right_max:
                right_max = heights[right]
            else:
                water += right_max - heights[right]
            right -= 1

    return water


def three_sum(nums):
    """
    Find all unique triplets that sum to zero.
    Time: O(n^2), Space: O(1) excluding output
    """
    nums.sort()
    result = []

    for i in range(len(nums) - 2):
        # Skip duplicates for first number
        if i > 0 and nums[i] == nums[i - 1]:
            continue

        left = i + 1
        right = len(nums) - 1

        while left < right:
            total = nums[i] + nums[left] + nums[right]

            if total == 0:
                result.append([nums[i], nums[left], nums[right]])

                # Skip duplicates for second number
                while left < right and nums[left] == nums[left + 1]:
                    left += 1
                # Skip duplicates for third number
                while left < right and nums[right] == nums[right - 1]:
                    right -= 1

                left += 1
                right -= 1
            elif total < 0:
                left += 1
            else:
                right -= 1

    return result


def sort_colors(nums):
    """
    Sort array of 0s, 1s, and 2s in-place (Dutch National Flag problem).
    Time: O(n), Space: O(1)
    """
    left = 0  # boundary for 0s
    right = len(nums) - 1  # boundary for 2s
    current = 0

    while current <= right:
        if nums[current] == 0:
            nums[left], nums[current] = nums[current], nums[left]
            left += 1
            current += 1
        elif nums[current] == 2:
            nums[current], nums[right] = nums[right], nums[current]
            right -= 1
        else:
            current += 1

    return nums


# ==============================================================================
# CATEGORY 2: SAME DIRECTION POINTERS (Slow and Fast)
# ==============================================================================

def remove_duplicates_sorted_array(nums):
    """
    Remove duplicates from sorted array in-place, return new length.
    Time: O(n), Space: O(1)
    """
    if not nums:
        return 0

    slow = 0

    for fast in range(1, len(nums)):
        if nums[fast] != nums[slow]:
            slow += 1
            nums[slow] = nums[fast]

    return slow + 1


def remove_element(nums, val):
    """
    Remove all instances of val in-place, return new length.
    Time: O(n), Space: O(1)
    """
    slow = 0

    for fast in range(len(nums)):
        if nums[fast] != val:
            nums[slow] = nums[fast]
            slow += 1

    return slow


def move_zeroes(nums):
    """
    Move all zeros to end while maintaining relative order of non-zeros.
    Time: O(n), Space: O(1)
    """
    slow = 0

    # Move all non-zero elements to front
    for fast in range(len(nums)):
        if nums[fast] != 0:
            nums[slow] = nums[fast]
            slow += 1

    # Fill remaining positions with zeros
    for i in range(slow, len(nums)):
        nums[i] = 0

    return nums


def squares_of_sorted_array(nums):
    """
    Return squares of sorted array in sorted order.
    Time: O(n), Space: O(n)
    """
    result = [0] * len(nums)
    left = 0
    right = len(nums) - 1
    position = len(nums) - 1

    while left <= right:
        left_square = nums[left] * nums[left]
        right_square = nums[right] * nums[right]

        if left_square > right_square:
            result[position] = left_square
            left += 1
        else:
            result[position] = right_square
            right -= 1

        position -= 1

    return result


def partition_array(nums, pivot):
    """
    Partition array such that elements < pivot come before elements >= pivot.
    Time: O(n), Space: O(1)
    """
    slow = 0

    for fast in range(len(nums)):
        if nums[fast] < pivot:
            nums[slow], nums[fast] = nums[fast], nums[slow]
            slow += 1

    return slow


def segregate_even_odd(nums):
    """
    Segregate even and odd numbers (evens first, then odds).
    Time: O(n), Space: O(1)
    """
    slow = 0

    for fast in range(len(nums)):
        if nums[fast] % 2 == 0:
            nums[slow], nums[fast] = nums[fast], nums[slow]
            slow += 1

    return nums


# ==============================================================================
# CATEGORY 3: SLIDING WINDOW - Fixed Size
# ==============================================================================

def max_sum_subarray_size_k(nums, k):
    """
    Find maximum sum of subarray of size k.
    Time: O(n), Space: O(1)
    """
    if len(nums) < k:
        return 0

    # Calculate sum of first window
    window_sum = sum(nums[:k])
    max_sum = window_sum

    # Slide window
    for i in range(k, len(nums)):
        window_sum = window_sum - nums[i - k] + nums[i]
        max_sum = max(max_sum, window_sum)

    return max_sum


def max_average_subarray(nums, k):
    """
    Find maximum average of subarray of size k.
    Time: O(n), Space: O(1)
    """
    window_sum = sum(nums[:k])
    max_sum = window_sum

    for i in range(k, len(nums)):
        window_sum = window_sum - nums[i - k] + nums[i]
        max_sum = max(max_sum, window_sum)

    return max_sum / k


def contains_nearby_duplicate(nums, k):
    """
    Check if array contains duplicate within k distance.
    Time: O(n), Space: O(k)
    """
    window = set()

    for i in range(len(nums)):
        if i > k:
            window.remove(nums[i - k - 1])

        if nums[i] in window:
            return True

        window.add(nums[i])

    return False


def count_distinct_in_window(nums, k):
    """
    Count distinct elements in every window of size k.
    Time: O(n), Space: O(k)
    """
    from collections import defaultdict

    freq = defaultdict(int)
    result = []

    # First window
    for i in range(k):
        freq[nums[i]] += 1
    result.append(len(freq))

    # Slide window
    for i in range(k, len(nums)):
        # Remove leftmost element
        freq[nums[i - k]] -= 1
        if freq[nums[i - k]] == 0:
            del freq[nums[i - k]]

        # Add new element
        freq[nums[i]] += 1
        result.append(len(freq))

    return result


# ==============================================================================
# CATEGORY 4: SLIDING WINDOW - Variable Size
# ==============================================================================

def longest_substring_without_repeating(s):
    """
    Find length of longest substring without repeating characters.
    Time: O(n), Space: O(min(n, m)) where m is charset size
    """
    char_set = set()
    left = 0
    max_length = 0

    for right in range(len(s)):
        # Shrink window until no duplicates
        while s[right] in char_set:
            char_set.remove(s[left])
            left += 1

        char_set.add(s[right])
        max_length = max(max_length, right - left + 1)

    return max_length


def longest_substring_k_distinct(s, k):
    """
    Find length of longest substring with at most k distinct characters.
    Time: O(n), Space: O(k)
    """
    from collections import defaultdict

    char_count = defaultdict(int)
    left = 0
    max_length = 0

    for right in range(len(s)):
        char_count[s[right]] += 1

        # Shrink window if too many distinct characters
        while len(char_count) > k:
            char_count[s[left]] -= 1
            if char_count[s[left]] == 0:
                del char_count[s[left]]
            left += 1

        max_length = max(max_length, right - left + 1)

    return max_length


def min_window_substring(s, t):
    """
    Find minimum window in s that contains all characters from t.
    Time: O(n + m), Space: O(m)
    """
    from collections import Counter

    if not s or not t:
        return ""

    required = Counter(t)
    formed = 0
    window_counts = {}

    left = 0
    min_len = float('inf')
    min_left = 0

    for right in range(len(s)):
        char = s[right]
        window_counts[char] = window_counts.get(char, 0) + 1

        if char in required and window_counts[char] == required[char]:
            formed += 1

        # Try to shrink window
        while left <= right and formed == len(required):
            char = s[left]

            # Update result
            if right - left + 1 < min_len:
                min_len = right - left + 1
                min_left = left

            window_counts[char] -= 1
            if char in required and window_counts[char] < required[char]:
                formed -= 1

            left += 1

    return "" if min_len == float('inf') else s[min_left:min_left + min_len]


def max_consecutive_ones_k_flips(nums, k):
    """
    Find max consecutive 1s if you can flip at most k 0s.
    Time: O(n), Space: O(1)
    """
    left = 0
    zero_count = 0
    max_length = 0

    for right in range(len(nums)):
        if nums[right] == 0:
            zero_count += 1

        # Shrink window if too many zeros
        while zero_count > k:
            if nums[left] == 0:
                zero_count -= 1
            left += 1

        max_length = max(max_length, right - left + 1)

    return max_length


def subarray_sum_equals_k(nums, k):
    """
    Find total number of continuous subarrays whose sum equals k.
    Time: O(n), Space: O(n)
    """
    from collections import defaultdict

    prefix_sums = defaultdict(int)
    prefix_sums[0] = 1
    current_sum = 0
    count = 0

    for num in nums:
        current_sum += num

        # Check if (current_sum - k) exists
        if current_sum - k in prefix_sums:
            count += prefix_sums[current_sum - k]

        prefix_sums[current_sum] += 1

    return count


def minimum_size_subarray_sum(target, nums):
    """
    Find minimal length of contiguous subarray with sum >= target.
    Time: O(n), Space: O(1)
    """
    left = 0
    current_sum = 0
    min_length = float('inf')

    for right in range(len(nums)):
        current_sum += nums[right]

        # Shrink window while sum is valid
        while current_sum >= target:
            min_length = min(min_length, right - left + 1)
            current_sum -= nums[left]
            left += 1

    return 0 if min_length == float('inf') else min_length


def longest_repeating_character_replacement(s, k):
    """
    Find length of longest substring with same letters after k replacements.
    Time: O(n), Space: O(1)
    """
    from collections import defaultdict

    char_count = defaultdict(int)
    left = 0
    max_count = 0
    max_length = 0

    for right in range(len(s)):
        char_count[s[right]] += 1
        max_count = max(max_count, char_count[s[right]])

        # Shrink if replacements needed > k
        while (right - left + 1) - max_count > k:
            char_count[s[left]] -= 1
            left += 1

        max_length = max(max_length, right - left + 1)

    return max_length


# ==============================================================================
# CATEGORY 5: MULTIPLE ARRAYS
# ==============================================================================

def merge_sorted_arrays(nums1, m, nums2, n):
    """
    Merge two sorted arrays (nums1 has space for both).
    Time: O(m + n), Space: O(1)
    """
    p1 = m - 1
    p2 = n - 1
    p = m + n - 1

    while p1 >= 0 and p2 >= 0:
        if nums1[p1] > nums2[p2]:
            nums1[p] = nums1[p1]
            p1 -= 1
        else:
            nums1[p] = nums2[p2]
            p2 -= 1
        p -= 1

    # Copy remaining from nums2
    while p2 >= 0:
        nums1[p] = nums2[p2]
        p2 -= 1
        p -= 1

    return nums1


def intersection_two_arrays(nums1, nums2):
    """
    Find intersection of two sorted arrays.
    Time: O(n + m), Space: O(1) excluding output
    """
    i = 0
    j = 0
    result = []

    while i < len(nums1) and j < len(nums2):
        if nums1[i] < nums2[j]:
            i += 1
        elif nums1[i] > nums2[j]:
            j += 1
        else:
            if not result or result[-1] != nums1[i]:
                result.append(nums1[i])
            i += 1
            j += 1

    return result


def compare_version_numbers(version1, version2):
    """
    Compare two version numbers.
    Time: O(max(n, m)), Space: O(max(n, m))
    """
    v1 = list(map(int, version1.split('.')))
    v2 = list(map(int, version2.split('.')))

    i = 0
    j = 0

    while i < len(v1) or j < len(v2):
        num1 = v1[i] if i < len(v1) else 0
        num2 = v2[j] if j < len(v2) else 0

        if num1 < num2:
            return -1
        elif num1 > num2:
            return 1

        i += 1
        j += 1

    return 0


# ==============================================================================
# LINKED LIST TWO POINTERS
# ==============================================================================

class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next


def detect_cycle(head):
    """
    Detect cycle in linked list (Floyd's algorithm).
    Time: O(n), Space: O(1)
    """
    slow = head
    fast = head

    while fast and fast.next:
        slow = slow.next
        fast = fast.next.next

        if slow == fast:
            return True

    return False


def find_middle_node(head):
    """
    Find middle node of linked list.
    Time: O(n), Space: O(1)
    """
    slow = head
    fast = head

    while fast and fast.next:
        slow = slow.next
        fast = fast.next.next

    return slow


def nth_node_from_end(head, n):
    """
    Find nth node from end of linked list.
    Time: O(n), Space: O(1)
    """
    fast = head
    slow = head

    # Move fast n steps ahead
    for _ in range(n):
        if not fast:
            return None
        fast = fast.next

    # Move both until fast reaches end
    while fast:
        slow = slow.next
        fast = fast.next

    return slow


def palindrome_linked_list(head):
    """
    Check if linked list is palindrome.
    Time: O(n), Space: O(1)
    """
    if not head or not head.next:
        return True

    # Find middle
    slow = head
    fast = head
    while fast.next and fast.next.next:
        slow = slow.next
        fast = fast.next.next

    # Reverse second half
    second_half = reverse_linked_list(slow.next)

    # Compare
    first_half = head
    while second_half:
        if first_half.val != second_half.val:
            return False
        first_half = first_half.next
        second_half = second_half.next

    return True


def reverse_linked_list(head):
    """
    Reverse a linked list.
    Time: O(n), Space: O(1)
    """
    prev = None
    current = head

    while current:
        next_node = current.next
        current.next = prev
        prev = current
        current = next_node

    return prev


# ==============================================================================
# TESTING
# ==============================================================================

if __name__ == "__main__":
    print("=" * 70)
    print("TWO POINTERS ALGORITHM EXAMPLES")
    print("=" * 70)

    # Opposite Direction
    print("\n1. OPPOSITE DIRECTION POINTERS")
    print("-" * 70)

    print("\nTwo Sum (Sorted Array):")
    result = two_sum_sorted([2, 7, 11, 15], 9)
    print(f"  Input: [2, 7, 11, 15], target=9")
    print(f"  Output: {result}")

    print("\nIs Palindrome:")
    result = is_palindrome("A man, a plan, a canal: Panama")
    print(f"  Input: 'A man, a plan, a canal: Panama'")
    print(f"  Output: {result}")

    print("\nContainer With Most Water:")
    result = container_with_most_water([1, 8, 6, 2, 5, 4, 8, 3, 7])
    print(f"  Input: [1, 8, 6, 2, 5, 4, 8, 3, 7]")
    print(f"  Output: {result}")

    print("\nTrapping Rain Water:")
    result = trapping_rain_water([0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1])
    print(f"  Input: [0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1]")
    print(f"  Output: {result} units")

    print("\nThree Sum:")
    result = three_sum([-1, 0, 1, 2, -1, -4])
    print(f"  Input: [-1, 0, 1, 2, -1, -4]")
    print(f"  Output: {result}")

    # Same Direction
    print("\n\n2. SAME DIRECTION POINTERS (Slow/Fast)")
    print("-" * 70)

    print("\nRemove Duplicates from Sorted Array:")
    nums = [1, 1, 2, 2, 3, 4, 4, 5]
    length = remove_duplicates_sorted_array(nums)
    print(f"  Input: [1, 1, 2, 2, 3, 4, 4, 5]")
    print(f"  Output length: {length}, Array: {nums[:length]}")

    print("\nMove Zeroes:")
    nums = [0, 1, 0, 3, 12]
    move_zeroes(nums)
    print(f"  Input: [0, 1, 0, 3, 12]")
    print(f"  Output: {nums}")

    print("\nSquares of Sorted Array:")
    result = squares_of_sorted_array([-4, -1, 0, 3, 10])
    print(f"  Input: [-4, -1, 0, 3, 10]")
    print(f"  Output: {result}")

    # Fixed Window
    print("\n\n3. SLIDING WINDOW - Fixed Size")
    print("-" * 70)

    print("\nMax Sum Subarray of Size K:")
    result = max_sum_subarray_size_k([2, 1, 5, 1, 3, 2], 3)
    print(f"  Input: [2, 1, 5, 1, 3, 2], k=3")
    print(f"  Output: {result}")

    print("\nMax Average Subarray:")
    result = max_average_subarray([1, 12, -5, -6, 50, 3], 4)
    print(f"  Input: [1, 12, -5, -6, 50, 3], k=4")
    print(f"  Output: {result}")

    print("\nCount Distinct in Windows:")
    result = count_distinct_in_window([1, 2, 1, 3, 4, 2, 3], 4)
    print(f"  Input: [1, 2, 1, 3, 4, 2, 3], k=4")
    print(f"  Output: {result}")

    # Variable Window
    print("\n\n4. SLIDING WINDOW - Variable Size")
    print("-" * 70)

    print("\nLongest Substring Without Repeating:")
    result = longest_substring_without_repeating("abcabcbb")
    print(f"  Input: 'abcabcbb'")
    print(f"  Output: {result}")

    print("\nLongest Substring with K Distinct:")
    result = longest_substring_k_distinct("eceba", 2)
    print(f"  Input: 'eceba', k=2")
    print(f"  Output: {result}")

    print("\nMinimum Window Substring:")
    result = min_window_substring("ADOBECODEBANC", "ABC")
    print(f"  Input: s='ADOBECODEBANC', t='ABC'")
    print(f"  Output: '{result}'")

    print("\nMax Consecutive Ones with K Flips:")
    result = max_consecutive_ones_k_flips([1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0], 2)
    print(f"  Input: [1,1,1,0,0,0,1,1,1,1,0], k=2")
    print(f"  Output: {result}")

    print("\nMinimum Size Subarray Sum:")
    result = minimum_size_subarray_sum(7, [2, 3, 1, 2, 4, 3])
    print(f"  Input: target=7, [2, 3, 1, 2, 4, 3]")
    print(f"  Output: {result}")

    # Multiple Arrays
    print("\n\n5. MULTIPLE ARRAYS")
    print("-" * 70)

    print("\nMerge Sorted Arrays:")
    nums1 = [1, 2, 3, 0, 0, 0]
    nums2 = [2, 5, 6]
    merge_sorted_arrays(nums1, 3, nums2, 3)
    print(f"  Input: nums1=[1,2,3,0,0,0], nums2=[2,5,6]")
    print(f"  Output: {nums1}")

    print("\nIntersection of Two Arrays:")
    result = intersection_two_arrays([1, 2, 2, 3, 4], [2, 2, 3, 5])
    print(f"  Input: [1,2,2,3,4], [2,2,3,5]")
    print(f"  Output: {result}")

    print("\nCompare Version Numbers:")
    result = compare_version_numbers("1.01", "1.001")
    print(f"  Input: '1.01' vs '1.001'")
    print(f"  Output: {result} (0=equal, -1=less, 1=greater)")

    print("\n" + "=" * 70)
    print("All examples completed!")
    print("=" * 70)

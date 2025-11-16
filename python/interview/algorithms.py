"""
Python Interview Questions: Algorithms
=======================================
Topics: Sorting, Searching, Recursion, Common Algorithms
Greppable format for quick reference
"""

# ============================================================================
# SORTING ALGORITHMS
# ============================================================================

# Q: What is Bubble Sort?
# A: Simple sorting algorithm that repeatedly swaps adjacent elements if wrong order
# Time Complexity: O(n^2), Space: O(1)
def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        swapped = False
        for j in range(0, n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
                swapped = True
        if not swapped:
            break
    return arr

# Q: What is Selection Sort?
# A: Finds minimum element and places it at beginning
# Time Complexity: O(n^2), Space: O(1)
def selection_sort(arr):
    n = len(arr)
    for i in range(n):
        min_idx = i
        for j in range(i + 1, n):
            if arr[j] < arr[min_idx]:
                min_idx = j
        arr[i], arr[min_idx] = arr[min_idx], arr[i]
    return arr

# Q: What is Insertion Sort?
# A: Builds sorted array one item at a time by inserting elements
# Time Complexity: O(n^2), Space: O(1)
def insertion_sort(arr):
    for i in range(1, len(arr)):
        key = arr[i]
        j = i - 1
        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1
        arr[j + 1] = key
    return arr

# Q: What is Merge Sort?
# A: Divide and conquer algorithm that divides array, sorts, and merges
# Time Complexity: O(n log n), Space: O(n)
def merge_sort(arr):
    if len(arr) <= 1:
        return arr

    mid = len(arr) // 2
    left = merge_sort(arr[:mid])
    right = merge_sort(arr[mid:])

    return merge(left, right)

def merge(left, right):
    result = []
    i = j = 0

    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1

    result.extend(left[i:])
    result.extend(right[j:])
    return result

# Q: What is Quick Sort?
# A: Divide and conquer using pivot element to partition array
# Time Complexity: O(n log n) average, O(n^2) worst, Space: O(log n)
def quick_sort(arr):
    if len(arr) <= 1:
        return arr

    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]

    return quick_sort(left) + middle + quick_sort(right)

# Q: How do you use Python's built-in sort?
# A: Use sorted() function or list.sort() method
numbers = [3, 1, 4, 1, 5, 9, 2, 6]
sorted_list = sorted(numbers)  # Returns new sorted list
numbers.sort()  # In-place sort
# Custom sorting
words = ["banana", "apple", "cherry"]
sorted_by_length = sorted(words, key=len)
sorted_reverse = sorted(numbers, reverse=True)

# ============================================================================
# SEARCHING ALGORITHMS
# ============================================================================

# Q: What is Linear Search?
# A: Search by checking each element sequentially
# Time Complexity: O(n), Space: O(1)
def linear_search(arr, target):
    for i in range(len(arr)):
        if arr[i] == target:
            return i
    return -1

# Q: What is Binary Search?
# A: Search sorted array by repeatedly dividing in half
# Time Complexity: O(log n), Space: O(1)
def binary_search(arr, target):
    left, right = 0, len(arr) - 1

    while left <= right:
        mid = (left + right) // 2

        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1

# Q: How do you implement binary search recursively?
# A: Use divide and conquer with base case
def binary_search_recursive(arr, target, left, right):
    if left > right:
        return -1

    mid = (left + right) // 2

    if arr[mid] == target:
        return mid
    elif arr[mid] < target:
        return binary_search_recursive(arr, target, mid + 1, right)
    else:
        return binary_search_recursive(arr, target, left, mid - 1)

# Q: How do you find the first and last position of element in sorted array?
# A: Use modified binary search
def find_first_last_position(arr, target):
    def find_first():
        left, right = 0, len(arr) - 1
        result = -1
        while left <= right:
            mid = (left + right) // 2
            if arr[mid] == target:
                result = mid
                right = mid - 1  # Continue searching left
            elif arr[mid] < target:
                left = mid + 1
            else:
                right = mid - 1
        return result

    def find_last():
        left, right = 0, len(arr) - 1
        result = -1
        while left <= right:
            mid = (left + right) // 2
            if arr[mid] == target:
                result = mid
                left = mid + 1  # Continue searching right
            elif arr[mid] < target:
                left = mid + 1
            else:
                right = mid - 1
        return result

    return [find_first(), find_last()]

# ============================================================================
# RECURSION
# ============================================================================

# Q: What is recursion?
# A: A function that calls itself with a base case to stop
def factorial(n):
    # Base case
    if n == 0 or n == 1:
        return 1
    # Recursive case
    return n * factorial(n - 1)

# Q: How do you implement Fibonacci recursively?
# A: Define base cases for 0 and 1, recurse for others
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

# Q: How do you improve recursive Fibonacci with memoization?
# A: Cache results to avoid recomputation
def fibonacci_memo(n, memo={}):
    if n in memo:
        return memo[n]
    if n <= 1:
        return n
    memo[n] = fibonacci_memo(n - 1, memo) + fibonacci_memo(n - 2, memo)
    return memo[n]

# Q: How do you implement Fibonacci iteratively?
# A: Use a loop to avoid recursion overhead
def fibonacci_iterative(n):
    if n <= 1:
        return n
    prev, curr = 0, 1
    for _ in range(2, n + 1):
        prev, curr = curr, prev + curr
    return curr

# Q: How do you reverse a string recursively?
# A: Take first char, recurse on rest, append first to end
def reverse_string(s):
    if len(s) <= 1:
        return s
    return reverse_string(s[1:]) + s[0]

# Q: How do you calculate power using recursion?
# A: Base case: power 0 is 1, recurse by reducing power
def power(base, exp):
    if exp == 0:
        return 1
    if exp < 0:
        return 1 / power(base, -exp)
    return base * power(base, exp - 1)

# Q: How do you optimize power calculation?
# A: Use divide and conquer (fast exponentiation)
def power_optimized(base, exp):
    if exp == 0:
        return 1
    if exp < 0:
        return 1 / power_optimized(base, -exp)

    half = power_optimized(base, exp // 2)

    if exp % 2 == 0:
        return half * half
    else:
        return base * half * half

# ============================================================================
# TWO POINTERS TECHNIQUE
# ============================================================================

# Q: How do you reverse an array in-place?
# A: Use two pointers from start and end, swap and move toward center
def reverse_array(arr):
    left, right = 0, len(arr) - 1
    while left < right:
        arr[left], arr[right] = arr[right], arr[left]
        left += 1
        right -= 1
    return arr

# Q: How do you check if a string is a palindrome?
# A: Use two pointers from both ends
def is_palindrome(s):
    left, right = 0, len(s) - 1
    while left < right:
        if s[left] != s[right]:
            return False
        left += 1
        right -= 1
    return True

# Q: How do you find pair with given sum in sorted array?
# A: Use two pointers technique
def find_pair_with_sum(arr, target_sum):
    left, right = 0, len(arr) - 1

    while left < right:
        current_sum = arr[left] + arr[right]
        if current_sum == target_sum:
            return [left, right]
        elif current_sum < target_sum:
            left += 1
        else:
            right -= 1

    return None

# Q: How do you remove duplicates from sorted array in-place?
# A: Use two pointers - one for reading, one for writing
def remove_duplicates(arr):
    if not arr:
        return 0

    write_pos = 1
    for read_pos in range(1, len(arr)):
        if arr[read_pos] != arr[read_pos - 1]:
            arr[write_pos] = arr[read_pos]
            write_pos += 1

    return write_pos

# ============================================================================
# SLIDING WINDOW TECHNIQUE
# ============================================================================

# Q: How do you find maximum sum of k consecutive elements?
# A: Use sliding window technique
def max_sum_subarray(arr, k):
    if len(arr) < k:
        return None

    # Calculate sum of first window
    window_sum = sum(arr[:k])
    max_sum = window_sum

    # Slide the window
    for i in range(k, len(arr)):
        window_sum = window_sum - arr[i - k] + arr[i]
        max_sum = max(max_sum, window_sum)

    return max_sum

# Q: How do you find longest substring without repeating characters?
# A: Use sliding window with hash set
def longest_unique_substring(s):
    char_set = set()
    left = 0
    max_length = 0

    for right in range(len(s)):
        while s[right] in char_set:
            char_set.remove(s[left])
            left += 1
        char_set.add(s[right])
        max_length = max(max_length, right - left + 1)

    return max_length

# ============================================================================
# COMMON ARRAY/LIST PROBLEMS
# ============================================================================

# Q: How do you find the maximum element in an array?
# A: Use max() function or iterate
def find_max(arr):
    # Using built-in
    return max(arr)
    # OR manually
    # max_val = arr[0]
    # for num in arr:
    #     if num > max_val:
    #         max_val = num
    # return max_val

# Q: How do you find the second largest element?
# A: Track first and second largest while iterating
def second_largest(arr):
    if len(arr) < 2:
        return None

    first = second = float('-inf')

    for num in arr:
        if num > first:
            second = first
            first = num
        elif num > second and num != first:
            second = num

    return second if second != float('-inf') else None

# Q: How do you rotate an array by k positions?
# A: Reverse entire array, then reverse first k and last n-k elements
def rotate_array(arr, k):
    k = k % len(arr)  # Handle k > len(arr)

    def reverse(start, end):
        while start < end:
            arr[start], arr[end] = arr[end], arr[start]
            start += 1
            end -= 1

    reverse(0, len(arr) - 1)
    reverse(0, k - 1)
    reverse(k, len(arr) - 1)

    return arr

# Q: How do you find missing number in array of 1 to n?
# A: Use sum formula or XOR
def find_missing_number(arr, n):
    # Method 1: Using sum
    expected_sum = n * (n + 1) // 2
    actual_sum = sum(arr)
    return expected_sum - actual_sum

    # Method 2: Using XOR (works even with duplicates)
    # xor_all = 0
    # for i in range(1, n + 1):
    #     xor_all ^= i
    # for num in arr:
    #     xor_all ^= num
    # return xor_all

# Q: How do you find duplicates in an array?
# A: Use hash set to track seen elements
def find_duplicates(arr):
    seen = set()
    duplicates = set()

    for num in arr:
        if num in seen:
            duplicates.add(num)
        else:
            seen.add(num)

    return list(duplicates)

# Q: How do you merge two sorted arrays?
# A: Use two pointers technique
def merge_sorted_arrays(arr1, arr2):
    result = []
    i = j = 0

    while i < len(arr1) and j < len(arr2):
        if arr1[i] <= arr2[j]:
            result.append(arr1[i])
            i += 1
        else:
            result.append(arr2[j])
            j += 1

    result.extend(arr1[i:])
    result.extend(arr2[j:])
    return result

# Q: How do you find the intersection of two arrays?
# A: Use sets for O(n) time complexity
def array_intersection(arr1, arr2):
    return list(set(arr1) & set(arr2))

# Q: How do you find the union of two arrays?
# A: Use sets
def array_union(arr1, arr2):
    return list(set(arr1) | set(arr2))

# ============================================================================
# TIME AND SPACE COMPLEXITY CHEAT SHEET
# ============================================================================

"""
Common Time Complexities (from fastest to slowest):
- O(1): Constant - array access, hash table lookup
- O(log n): Logarithmic - binary search
- O(n): Linear - simple loop through array
- O(n log n): Linearithmic - efficient sorting (merge sort, quick sort)
- O(n^2): Quadratic - nested loops (bubble sort, selection sort)
- O(2^n): Exponential - recursive fibonacci
- O(n!): Factorial - permutations

Space Complexity:
- O(1): Constant - few variables
- O(n): Linear - array/list proportional to input
- O(log n): Logarithmic - recursive call stack for binary search
"""

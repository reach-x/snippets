"""
SLIDING WINDOW PATTERN - Comprehensive Guide with Full Documentation

The sliding window pattern is used to perform operations on a specific window size
of a given array or linked list. The window slides through the data structure to
capture different subsets of elements.

WHEN TO USE:
- Problems involving contiguous sequences (subarrays, substrings)
- Finding max/min/average of consecutive elements
- Longest/shortest substring with certain conditions
- Problems with phrases like "contiguous", "subarray", "substring"
- Optimization problems on sequences where brute force would be O(n*k) or O(n²)

TIME COMPLEXITY: Generally O(n) - each element visited at most twice
SPACE COMPLEXITY: Generally O(1) to O(window_size) depending on auxiliary data structures

TWO TYPES:
1. Fixed Window Size - window size remains constant throughout iteration
2. Variable Window Size - window expands/contracts based on conditions

CORE IDEA:
Instead of recalculating for each position, maintain a window and:
- Add new element on right
- Remove old element on left
- Update result based on current window
"""

from typing import List, Dict
from collections import Counter


# ============================================================================
# QUICK REFERENCE - COPY-PASTE TEMPLATES
# ============================================================================
"""
Use these minimal templates during interviews. Copy and adapt as needed.
"""

# TEMPLATE 1: Fixed Window Size
def sliding_window_fixed_template(array: List[int], k: int) -> int:
    """Find max/min/sum of k consecutive elements."""
    window_sum = sum(array[:k])  # Initial window
    result = window_sum

    for right_index in range(k, len(array)):
        window_sum = window_sum - array[right_index - k] + array[right_index]
        result = max(result, window_sum)  # or min() depending on problem

    return result


# TEMPLATE 2: Variable Window Size
def sliding_window_variable_template(array: List[int], target: int) -> int:
    """Find longest/shortest subarray matching condition."""
    left = 0
    current_sum = 0  # or use Counter/set for tracking
    result = 0  # or float('inf') for minimum

    for right in range(len(array)):
        current_sum += array[right]  # Expand window

        while current_sum > target:  # Contract window when invalid
            current_sum -= array[left]
            left += 1

        result = max(result, right - left + 1)  # Update result when valid

    return result


# TEMPLATE 3: Variable Window with HashMap
def sliding_window_hashmap_template(string: str, k: int) -> int:
    """Find longest substring with at most k distinct characters."""
    left = 0
    char_frequency = {}
    result = 0

    for right in range(len(string)):
        char_frequency[string[right]] = char_frequency.get(string[right], 0) + 1

        while len(char_frequency) > k:  # Too many distinct chars
            char_frequency[string[left]] -= 1
            if char_frequency[string[left]] == 0:
                del char_frequency[string[left]]
            left += 1

        result = max(result, right - left + 1)

    return result


# ============================================================================
# PATTERN 1: FIXED WINDOW SIZE
# ============================================================================

def max_sum_subarray_fixed(array: List[int], window_size: int) -> int:
    """
    Find maximum sum of window_size consecutive elements in the array.

    Example: array = [2, 1, 5, 1, 3, 2], window_size = 3
             Windows: [2,1,5]=8, [1,5,1]=7, [5,1,3]=9, [1,3,2]=6
             Maximum sum = 9

    Template for FIXED sliding window:
    1. Calculate sum of first window_size elements (initial window)
    2. Slide window: subtract leftmost element, add rightmost element
    3. Track maximum/minimum as you slide

    Why this works:
    - Instead of summing window_size elements for each position (O(n*k))
    - We reuse previous sum and adjust by 1 element (O(n))

    Args:
        array: List of integers to search
        window_size: Size of the sliding window (number of consecutive elements)

    Returns:
        Maximum sum found among all windows

    Time Complexity: O(n) where n is length of array
    Space Complexity: O(1) - only using variables for sum and max
    """
    # Edge case validation
    if not array or window_size <= 0 or window_size > len(array):
        return 0

    # Step 1: Calculate sum of first window (indices 0 to window_size-1)
    window_sum = sum(array[:window_size])
    maximum_sum = window_sum

    # Step 2: Slide the window from left to right through remaining array
    # Start from window_size because first window already calculated
    for right_index in range(window_size, len(array)):
        # Calculate index of element leaving the window
        left_index = right_index - window_size

        # Remove leftmost element that's exiting the window
        window_sum -= array[left_index]

        # Add new rightmost element entering the window
        window_sum += array[right_index]

        # Update maximum if current window has larger sum
        maximum_sum = max(maximum_sum, window_sum)

    return maximum_sum


def average_of_subarrays_fixed(array: List[int], window_size: int) -> List[float]:
    """
    Calculate average of all window_size-sized subarrays.

    Example: array = [1, 3, 2, 6, -1, 4, 1, 8, 2], window_size = 5
             Window 1: [1,3,2,6,-1] avg = 11/5 = 2.2
             Window 2: [3,2,6,-1,4] avg = 14/5 = 2.8
             And so on...

    Args:
        array: List of integers
        window_size: Size of each subarray window

    Returns:
        List of averages for each possible window

    Time Complexity: O(n)
    Space Complexity: O(n) for result array
    """
    # Edge case validation
    if not array or window_size <= 0 or window_size > len(array):
        return []

    averages = []

    # Calculate sum and average of first window
    window_sum = sum(array[:window_size])
    averages.append(window_sum / window_size)

    # Slide window through rest of array
    for right_index in range(window_size, len(array)):
        # Update sum: remove left element, add right element
        left_index = right_index - window_size
        window_sum = window_sum - array[left_index] + array[right_index]

        # Calculate and store average for this window
        averages.append(window_sum / window_size)

    return averages


# ============================================================================
# PATTERN 2: VARIABLE WINDOW SIZE (EXPAND/CONTRACT)
# ============================================================================

def longest_substring_k_distinct(string: str, max_distinct: int) -> int:
    """
    Find length of longest substring with at most max_distinct distinct characters.

    Example: string = "araaci", max_distinct = 2
             Substrings with ≤2 distinct: "ara", "araa", "aa", "aci", etc.
             Longest is "araa" with length 4

    Template for VARIABLE sliding window:
    1. Expand window by moving right pointer
    2. When condition violated (too many distinct chars), contract from left
    3. Track result at each valid state

    Args:
        string: Input string to search
        max_distinct: Maximum number of distinct characters allowed in window

    Returns:
        Length of longest valid substring

    Time Complexity: O(n) - each character added and removed at most once
    Space Complexity: O(max_distinct) - hashmap stores at most max_distinct+1 characters
    """
    if not string or max_distinct <= 0:
        return 0

    # Track frequency of each character in current window
    character_frequency: Dict[str, int] = {}
    maximum_length = 0
    left_pointer = 0

    # Expand window by moving right pointer through string
    for right_pointer in range(len(string)):
        # Add character at right pointer to window
        right_character = string[right_pointer]
        character_frequency[right_character] = character_frequency.get(right_character, 0) + 1

        # Contract window if we have too many distinct characters
        # Keep contracting until we're back to valid state
        while len(character_frequency) > max_distinct:
            # Remove leftmost character from window
            left_character = string[left_pointer]
            character_frequency[left_character] -= 1

            # Remove from map if frequency reaches zero
            if character_frequency[left_character] == 0:
                del character_frequency[left_character]

            # Move left pointer to contract window
            left_pointer += 1

        # Update maximum length with current valid window size
        current_window_size = right_pointer - left_pointer + 1
        maximum_length = max(maximum_length, current_window_size)

    return maximum_length


def longest_substring_without_repeating(string: str) -> int:
    """
    Find length of longest substring without repeating characters.

    Example: string = "abccabcbb"
             Substrings without repeats: "abc", "bca", "cab", "ab", "bc", "cb"
             Longest is "abc" (or "bca", "cab") with length 3

    Approach:
    - Use hashmap to track last seen index of each character
    - When we see duplicate, jump left pointer past previous occurrence
    - This ensures window always has unique characters

    Args:
        string: Input string to search

    Returns:
        Length of longest substring with all unique characters

    Time Complexity: O(n) where n is length of string
    Space Complexity: O(min(n, alphabet_size)) for the hashmap
    """
    if not string:
        return 0

    # Map each character to its most recent index in the string
    character_to_index: Dict[str, int] = {}
    maximum_length = 0
    left_pointer = 0

    for right_pointer in range(len(string)):
        current_character = string[right_pointer]

        # If character seen before AND within current window
        if current_character in character_to_index and character_to_index[current_character] >= left_pointer:
            # Move left pointer past the previous occurrence
            # This removes the duplicate from our window
            left_pointer = character_to_index[current_character] + 1

        # Update/store the current index of this character
        character_to_index[current_character] = right_pointer

        # Calculate current window size and update maximum
        current_window_size = right_pointer - left_pointer + 1
        maximum_length = max(maximum_length, current_window_size)

    return maximum_length


def min_window_substring(source_string: str, target_string: str) -> str:
    """
    Find minimum window in source_string that contains all characters from target_string.

    Example: source = "ADOBECODEBANC", target = "ABC"
             Windows containing ABC: "ADOBEC", "ODEBANC", "BANC"
             Minimum is "BANC" with length 4

    Template for MINIMUM WINDOW problems:
    1. Use frequency map for target characters
    2. Expand window until all target characters found
    3. Contract window while condition still satisfied (minimize)
    4. Track minimum window found

    Args:
        source_string: String to search within
        target_string: String containing characters we need to find

    Returns:
        Minimum substring containing all target characters, or empty string if none exists

    Time Complexity: O(|source| + |target|)
    Space Complexity: O(|target|) for frequency maps
    """
    if not source_string or not target_string or len(source_string) < len(target_string):
        return ""

    # Count frequency of each character we need to find
    target_frequency = Counter(target_string)
    unique_target_chars = len(target_frequency)  # Number of distinct characters in target

    # Track current window state
    window_frequency: Dict[str, int] = {}

    # Count how many target characters we have with correct frequency
    formed_characters = 0

    # Store result
    minimum_length = float('inf')
    minimum_left = 0

    left_pointer = 0

    # Expand window by moving right pointer
    for right_pointer in range(len(source_string)):
        # Add character from right into window
        right_character = source_string[right_pointer]
        window_frequency[right_character] = window_frequency.get(right_character, 0) + 1

        # Check if this character's frequency now matches target frequency
        if right_character in target_frequency and window_frequency[right_character] == target_frequency[right_character]:
            formed_characters += 1

        # Try to contract window while we still have all required characters
        while formed_characters == unique_target_chars and left_pointer <= right_pointer:
            # Update result if current window is smaller than previous minimum
            current_window_size = right_pointer - left_pointer + 1
            if current_window_size < minimum_length:
                minimum_length = current_window_size
                minimum_left = left_pointer

            # Remove character from left side of window
            left_character = source_string[left_pointer]
            window_frequency[left_character] -= 1

            # Check if removing this character breaks our requirement
            if left_character in target_frequency and window_frequency[left_character] < target_frequency[left_character]:
                formed_characters -= 1

            # Move left pointer to contract window
            left_pointer += 1

    # Return minimum window if found, otherwise empty string
    if minimum_length == float('inf'):
        return ""
    else:
        return source_string[minimum_left:minimum_left + minimum_length]


def max_consecutive_ones_k_flips(binary_array: List[int], max_flips: int) -> int:
    """
    Find maximum consecutive 1s if you can flip at most max_flips zeros to ones.

    Example: array = [1,1,1,0,0,0,1,1,1,1,0], max_flips = 2
             We can flip two 0s to get [1,1,1,1,1,0,1,1,1,1,0]
             Maximum consecutive 1s = 6

    Approach:
    - Maintain window with at most max_flips zeros
    - When we exceed max_flips zeros, contract from left
    - Track maximum window size seen

    Args:
        binary_array: Array containing only 0s and 1s
        max_flips: Maximum number of 0s we can flip to 1s

    Returns:
        Length of longest sequence of 1s achievable

    Time Complexity: O(n)
    Space Complexity: O(1)
    """
    maximum_ones = 0
    left_pointer = 0
    zero_count = 0

    for right_pointer in range(len(binary_array)):
        # Expand window: if we encounter a zero, increment counter
        if binary_array[right_pointer] == 0:
            zero_count += 1

        # Contract window if we have more zeros than allowed flips
        while zero_count > max_flips:
            if binary_array[left_pointer] == 0:
                zero_count -= 1
            left_pointer += 1

        # Update maximum length with current window size
        current_window_size = right_pointer - left_pointer + 1
        maximum_ones = max(maximum_ones, current_window_size)

    return maximum_ones


def length_of_longest_subarray_sum_k(array: List[int], target_sum: int) -> int:
    """
    Find length of longest subarray with sum equal to target_sum.
    Note: This works for arrays with POSITIVE numbers only.

    Example: array = [1,2,3,1,1,1,1,4,2,3], target_sum = 7
             Subarrays with sum 7: [3,1,1,1,1], [1,2,4], [7]
             Longest is [3,1,1,1,1] or [1,1,1,1,3] with length 4

    Approach:
    - Expand window until sum exceeds target
    - Contract window until sum is ≤ target
    - Check if we hit exact target

    Args:
        array: Array of positive integers
        target_sum: Target sum to find

    Returns:
        Length of longest subarray with sum equal to target_sum

    Time Complexity: O(n)
    Space Complexity: O(1)
    """
    maximum_length = 0
    left_pointer = 0
    current_sum = 0

    for right_pointer in range(len(array)):
        # Add right element to current sum
        current_sum += array[right_pointer]

        # Contract window while sum exceeds target
        while current_sum > target_sum and left_pointer <= right_pointer:
            current_sum -= array[left_pointer]
            left_pointer += 1

        # Check if current sum equals target
        if current_sum == target_sum:
            current_length = right_pointer - left_pointer + 1
            maximum_length = max(maximum_length, current_length)

    return maximum_length


def fruits_into_baskets(fruit_types: List[int]) -> int:
    """
    Pick maximum fruits from trees with at most 2 fruit types.
    This is equivalent to: longest subarray with at most 2 distinct elements.

    Example: fruit_types = [1,2,3,2,2]
             You can pick: [1,2], [2,3,2,2], etc.
             Maximum is [2,3,2,2] with 4 fruits

    Problem: You're walking along a row of trees, each tree has a type of fruit.
    You have two baskets, each can hold unlimited fruit but only one type per basket.
    Find maximum number of fruits you can collect.

    Args:
        fruit_types: Array where each element is a fruit type (integer)

    Returns:
        Maximum number of fruits that can be collected

    Time Complexity: O(n)
    Space Complexity: O(1) - at most 3 fruit types in map at any time
    """
    fruit_type_count: Dict[int, int] = {}
    maximum_fruits = 0
    left_pointer = 0

    for right_pointer in range(len(fruit_types)):
        # Add current fruit type to our collection
        current_fruit = fruit_types[right_pointer]
        fruit_type_count[current_fruit] = fruit_type_count.get(current_fruit, 0) + 1

        # If we have more than 2 types, contract window from left
        while len(fruit_type_count) > 2:
            left_fruit = fruit_types[left_pointer]
            fruit_type_count[left_fruit] -= 1

            # Remove type if count reaches zero
            if fruit_type_count[left_fruit] == 0:
                del fruit_type_count[left_fruit]

            left_pointer += 1

        # Update maximum fruits with current window size
        current_fruits = right_pointer - left_pointer + 1
        maximum_fruits = max(maximum_fruits, current_fruits)

    return maximum_fruits


# ============================================================================
# SLIDING WINDOW PATTERN TEMPLATES
# ============================================================================

def sliding_window_template_fixed(array: List[int], window_size: int):
    """
    TEMPLATE: Fixed Size Sliding Window

    Use this template when the window size is constant.

    Pattern:
    1. Calculate result for first window
    2. For remaining positions:
       - Remove element leaving window (left side)
       - Add element entering window (right side)
       - Update result
    """
    if not array or window_size <= 0 or window_size > len(array):
        return None

    # Step 1: Initialize window state for first window_size elements
    # Example: window_sum = sum(array[:window_size])
    # result = initial_value

    # Step 2: Slide window through remaining array
    for right_index in range(window_size, len(array)):
        # Calculate index of element leaving window
        left_index = right_index - window_size

        # Remove element going out of window
        # Example: window_sum -= array[left_index]

        # Add new element to window
        # Example: window_sum += array[right_index]

        # Process current window and update result
        # Example: result = max(result, window_sum)
        pass

    # return result


def sliding_window_template_variable(array: List[int], condition):
    """
    TEMPLATE: Variable Size Sliding Window

    Use this template when window size changes based on conditions.

    Pattern:
    1. Expand window by moving right pointer
    2. Contract window when condition violated
    3. Update result when window is valid
    """
    if not array:
        return None

    left_pointer = 0
    # window_state = {}  # Dictionary or variables to track window state
    # result = initial_value

    # Expand window with right pointer
    for right_pointer in range(len(array)):
        # Step 1: Add element to window
        # Example: window_state[array[right_pointer]] = ...

        # Step 2: Contract window while condition violated
        # while not is_valid(window_state):
        #     # Remove element from left
        #     # Example: window_state[array[left_pointer]] = ...
        #     left_pointer += 1

        # Step 3: Update result with current valid window
        # Example: result = max(result, right_pointer - left_pointer + 1)
        pass

    # return result


# ============================================================================
# TEST CASES WITH DETAILED EXPLANATIONS
# ============================================================================

def run_tests():
    """Run comprehensive tests for all sliding window patterns."""
    print("=" * 80)
    print("SLIDING WINDOW PATTERN - COMPREHENSIVE TEST CASES")
    print("=" * 80)

    # Test 1: Fixed Window - Maximum Sum
    print("\n" + "=" * 80)
    print("TEST 1: Maximum Sum Subarray with Fixed Window Size")
    print("=" * 80)
    test_array_1 = [2, 1, 5, 1, 3, 2]
    window_size_1 = 3
    print(f"Input Array: {test_array_1}")
    print(f"Window Size: {window_size_1}")
    print(f"All windows: [2,1,5]=8, [1,5,1]=7, [5,1,3]=9, [1,3,2]=6")
    result_1 = max_sum_subarray_fixed(test_array_1, window_size_1)
    print(f"Output: {result_1}")
    print(f"Expected: 9 (from subarray [5,1,3])")

    # Test 2: Fixed Window - Averages
    print("\n" + "=" * 80)
    print("TEST 2: Average of All Subarrays with Fixed Window Size")
    print("=" * 80)
    test_array_2 = [1, 3, 2, 6, -1, 4, 1, 8, 2]
    window_size_2 = 5
    print(f"Input Array: {test_array_2}")
    print(f"Window Size: {window_size_2}")
    result_2 = average_of_subarrays_fixed(test_array_2, window_size_2)
    print(f"Output: {result_2}")
    print(f"Expected: [2.2, 2.8, 2.4, 3.6, 2.8]")

    # Test 3: Variable Window - K Distinct Characters
    print("\n" + "=" * 80)
    print("TEST 3: Longest Substring with K Distinct Characters")
    print("=" * 80)
    test_string_3 = "araaci"
    max_distinct_3 = 2
    print(f"Input String: '{test_string_3}'")
    print(f"Max Distinct Characters: {max_distinct_3}")
    print(f"Explanation: 'araa' has 2 distinct chars (a, r) with length 4")
    result_3 = longest_substring_k_distinct(test_string_3, max_distinct_3)
    print(f"Output: {result_3}")
    print(f"Expected: 4")

    # Test 4: Variable Window - No Repeating Characters
    print("\n" + "=" * 80)
    print("TEST 4: Longest Substring Without Repeating Characters")
    print("=" * 80)
    test_string_4 = "abccabcbb"
    print(f"Input String: '{test_string_4}'")
    print(f"Explanation: 'abc' has no repeating characters with length 3")
    result_4 = longest_substring_without_repeating(test_string_4)
    print(f"Output: {result_4}")
    print(f"Expected: 3")

    # Test 5: Minimum Window Substring
    print("\n" + "=" * 80)
    print("TEST 5: Minimum Window Substring")
    print("=" * 80)
    source_5 = "ADOBECODEBANC"
    target_5 = "ABC"
    print(f"Source String: '{source_5}'")
    print(f"Target String: '{target_5}'")
    print(f"Explanation: 'BANC' is the smallest window containing A, B, and C")
    result_5 = min_window_substring(source_5, target_5)
    print(f"Output: '{result_5}'")
    print(f"Expected: 'BANC'")

    # Test 6: Max Consecutive Ones with K Flips
    print("\n" + "=" * 80)
    print("TEST 6: Maximum Consecutive Ones with K Flips")
    print("=" * 80)
    test_array_6 = [1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0]
    max_flips_6 = 2
    print(f"Input Array: {test_array_6}")
    print(f"Max Flips Allowed: {max_flips_6}")
    print(f"Explanation: Flip two 0s at indices 3 and 4 to get 6 consecutive 1s")
    result_6 = max_consecutive_ones_k_flips(test_array_6, max_flips_6)
    print(f"Output: {result_6}")
    print(f"Expected: 6")

    # Test 7: Longest Subarray Sum K
    print("\n" + "=" * 80)
    print("TEST 7: Longest Subarray with Sum Equal to K")
    print("=" * 80)
    test_array_7 = [1, 2, 3, 1, 1, 1, 1, 4, 2, 3]
    target_sum_7 = 7
    print(f"Input Array: {test_array_7}")
    print(f"Target Sum: {target_sum_7}")
    print(f"Explanation: Subarray [1,1,1,4] sums to 7 with length 4")
    result_7 = length_of_longest_subarray_sum_k(test_array_7, target_sum_7)
    print(f"Output: {result_7}")
    print(f"Expected: 4")

    # Test 8: Fruits into Baskets
    print("\n" + "=" * 80)
    print("TEST 8: Fruits into Baskets (Max 2 Types)")
    print("=" * 80)
    fruit_types = [1, 2, 3, 2, 2]
    print(f"Input Fruit Types: {fruit_types}")
    print(f"Explanation: Can collect [2,3,2,2] = 4 fruits with 2 types")
    result_8 = fruits_into_baskets(fruit_types)
    print(f"Output: {result_8}")
    print(f"Expected: 4")

    # Print Key Insights
    print("\n" + "=" * 80)
    print("KEY INSIGHTS AND PATTERNS")
    print("=" * 80)
    print("1. FIXED WINDOW:")
    print("   - Initialize window with first K elements")
    print("   - Slide: remove left element, add right element")
    print("   - Time: O(n), Space: usually O(1)")
    print()
    print("2. VARIABLE WINDOW:")
    print("   - Expand with right pointer (add elements)")
    print("   - Contract with left pointer when condition violated")
    print("   - Update result when window is valid")
    print()
    print("3. COMMON TRICKS:")
    print("   - Use HashMap/Counter for frequency tracking")
    print("   - Each element visited at most twice (added once, removed once)")
    print("   - Pattern: while (invalid) { contract from left }")
    print("   - Always check if current window is better than previous best")
    print()
    print("4. OPTIMIZATION:")
    print("   - Sliding window reduces O(n*k) or O(n²) to O(n)")
    print("   - Reuse computation instead of recalculating")
    print("   - Maintain state in variables/hashmaps")
    print("=" * 80)


if __name__ == "__main__":
    run_tests()

"""
TWO POINTERS PATTERN - Comprehensive Guide

The two pointers technique uses two pointers to iterate through a data structure
(usually an array or linked list) in tandem. It's extremely efficient for many
problems involving sorted arrays or searching for pairs/triplets.

WHEN TO USE:
- Sorted array problems
- Finding pairs/triplets with target sum
- Removing duplicates
- Palindrome checking
- Partitioning arrays
- Merging sorted arrays
- Container/area problems

TIME COMPLEXITY: Usually O(n) or O(n²) for nested loops
SPACE COMPLEXITY: Usually O(1) - in-place operations

THREE MAIN PATTERNS:
1. Opposite Ends: Start from both ends, move toward middle
2. Same Direction: Both pointers move in same direction (fast/slow)
3. Sliding Window: Special case where pointers maintain a window
"""

from typing import List


# ============================================================================
# PATTERN 1: TWO SUM (SORTED ARRAY)
# ============================================================================

def two_sum_sorted(numbers: List[int], target_sum: int) -> List[int]:
    """
    Find two numbers in sorted array that sum to target.

    Example:
        numbers = [2, 7, 11, 15], target = 9
        Output: [0, 1] (indices) or [2, 7] (values)

    Template for OPPOSITE-END two pointers:
    1. Left pointer at start, right pointer at end
    2. If current_sum < target: move left right (increase sum)
    3. If current_sum > target: move right left (decrease sum)
    4. If current_sum == target: found solution!

    Why it works:
    - Array is sorted
    - If sum too small, need larger values (move left pointer right)
    - If sum too large, need smaller values (move right pointer left)
    - Each move eliminates one element from consideration

    Time Complexity: O(n) - each pointer moves at most n times
    Space Complexity: O(1) - only use two pointers

    Args:
        numbers: Sorted array of integers
        target_sum: Target sum to find

    Returns:
        Indices of two numbers that sum to target, or empty list
    """
    left_pointer = 0
    right_pointer = len(numbers) - 1

    while left_pointer < right_pointer:
        # Calculate sum of current pair
        current_sum = numbers[left_pointer] + numbers[right_pointer]

        # Found target sum
        if current_sum == target_sum:
            return [left_pointer, right_pointer]

        # Sum too small, need larger values (move left pointer right)
        elif current_sum < target_sum:
            left_pointer += 1

        # Sum too large, need smaller values (move right pointer left)
        else:
            right_pointer -= 1

    # No solution found
    return []


def two_sum_all_pairs(numbers: List[int], target_sum: int) -> List[List[int]]:
    """
    Find ALL unique pairs that sum to target (no duplicate pairs).

    Example:
        numbers = [1, 1, 2, 2, 3, 4], target = 4
        Output: [[1, 3], [2, 2]]

    Key: Skip duplicate values to avoid repeated pairs.

    Why skip duplicates:
    - [1, 3] using first 1 is same as [1, 3] using second 1
    - After finding pair, skip all duplicate left/right values
    - Ensures unique pairs only

    Time Complexity: O(n log n) for sort + O(n) for two pointers
    Space Complexity: O(1) excluding output

    Args:
        numbers: Array of integers (will be sorted)
        target_sum: Target sum to find

    Returns:
        List of all unique pairs that sum to target
    """
    numbers.sort()  # Ensure array is sorted
    left_pointer = 0
    right_pointer = len(numbers) - 1
    result_pairs = []

    while left_pointer < right_pointer:
        current_sum = numbers[left_pointer] + numbers[right_pointer]

        if current_sum == target_sum:
            # Found a valid pair
            result_pairs.append([numbers[left_pointer], numbers[right_pointer]])

            # Skip duplicate left values
            while left_pointer < right_pointer and numbers[left_pointer] == numbers[left_pointer + 1]:
                left_pointer += 1

            # Skip duplicate right values
            while left_pointer < right_pointer and numbers[right_pointer] == numbers[right_pointer - 1]:
                right_pointer -= 1

            # Move both pointers inward
            left_pointer += 1
            right_pointer -= 1

        elif current_sum < target_sum:
            left_pointer += 1
        else:
            right_pointer -= 1

    return result_pairs


# ============================================================================
# PATTERN 2: THREE SUM
# ============================================================================

def three_sum(numbers: List[int], target_sum: int = 0) -> List[List[int]]:
    """
    Find all unique triplets that sum to target.

    Example:
        numbers = [-1, 0, 1, 2, -1, -4], target = 0
        Output: [[-1, -1, 2], [-1, 0, 1]]

    Strategy:
    1. Sort array
    2. Fix first element with outer loop
    3. Use two pointers for remaining two elements
    4. Skip duplicates to avoid repeated triplets

    Why it works:
    - Fix one element, reduces to two-sum problem for rest
    - Two pointers find pairs that sum to (target - fixed_element)
    - Sorting + skipping duplicates ensures unique triplets

    Time Complexity: O(n²) - n iterations * n for two pointers
    Space Complexity: O(1) excluding output

    Args:
        numbers: Array of integers
        target_sum: Target sum to find (default 0)

    Returns:
        List of all unique triplets that sum to target
    """
    numbers.sort()
    result_triplets = []

    # Fix first element (iterate to second-to-last position)
    for first_index in range(len(numbers) - 2):
        # Skip duplicate first elements
        if first_index > 0 and numbers[first_index] == numbers[first_index - 1]:
            continue

        # Two pointers for remaining elements
        left_pointer = first_index + 1
        right_pointer = len(numbers) - 1

        # What the remaining two elements should sum to
        remaining_target = target_sum - numbers[first_index]

        while left_pointer < right_pointer:
            current_pair_sum = numbers[left_pointer] + numbers[right_pointer]

            if current_pair_sum == remaining_target:
                # Found valid triplet
                result_triplets.append([
                    numbers[first_index],
                    numbers[left_pointer],
                    numbers[right_pointer]
                ])

                # Skip duplicate left values
                while left_pointer < right_pointer and numbers[left_pointer] == numbers[left_pointer + 1]:
                    left_pointer += 1

                # Skip duplicate right values
                while left_pointer < right_pointer and numbers[right_pointer] == numbers[right_pointer - 1]:
                    right_pointer -= 1

                # Move both pointers
                left_pointer += 1
                right_pointer -= 1

            elif current_pair_sum < remaining_target:
                left_pointer += 1
            else:
                right_pointer -= 1

    return result_triplets


def three_sum_closest(numbers: List[int], target_sum: int) -> int:
    """
    Find three numbers whose sum is closest to target.

    Example:
        numbers = [-1, 2, 1, -4], target = 1
        Output: 2 (sum of -1 + 2 + 1 = 2)

    Strategy:
    - Similar to three_sum, but track closest sum instead
    - Update closest when find smaller difference
    - Return exact target if found

    Time Complexity: O(n²)
    Space Complexity: O(1)

    Args:
        numbers: Array of integers
        target_sum: Target sum to get close to

    Returns:
        Sum of three numbers closest to target
    """
    numbers.sort()
    closest_sum = float('inf')
    minimum_difference = float('inf')

    # Fix first element
    for first_index in range(len(numbers) - 2):
        left_pointer = first_index + 1
        right_pointer = len(numbers) - 1

        while left_pointer < right_pointer:
            # Calculate current triplet sum
            current_triplet_sum = (numbers[first_index] +
                                  numbers[left_pointer] +
                                  numbers[right_pointer])

            # Calculate difference from target
            current_difference = abs(current_triplet_sum - target_sum)

            # Update closest if this is better
            if current_difference < minimum_difference:
                minimum_difference = current_difference
                closest_sum = current_triplet_sum

            # Adjust pointers based on comparison to target
            if current_triplet_sum < target_sum:
                left_pointer += 1  # Need larger sum
            elif current_triplet_sum > target_sum:
                right_pointer -= 1  # Need smaller sum
            else:
                # Exact match found!
                return target_sum

    return closest_sum


# ============================================================================
# PATTERN 3: FOUR SUM
# ============================================================================

def four_sum(numbers: List[int], target_sum: int) -> List[List[int]]:
    """
    Find all unique quadruplets that sum to target.

    Example:
        numbers = [1, 0, -1, 0, -2, 2], target = 0
        Output: [[-2, -1, 1, 2], [-2, 0, 0, 2], [-1, 0, 0, 1]]

    Strategy: Two nested loops to fix first two elements + two pointers.
    - Fix first element (outer loop)
    - Fix second element (inner loop)
    - Use two pointers for remaining two elements

    Time Complexity: O(n³)
    Space Complexity: O(1) excluding output

    Args:
        numbers: Array of integers
        target_sum: Target sum to find

    Returns:
        List of all unique quadruplets that sum to target
    """
    numbers.sort()
    result_quadruplets = []

    # Fix first element
    for first_index in range(len(numbers) - 3):
        # Skip duplicate first elements
        if first_index > 0 and numbers[first_index] == numbers[first_index - 1]:
            continue

        # Fix second element
        for second_index in range(first_index + 1, len(numbers) - 2):
            # Skip duplicate second elements
            if second_index > first_index + 1 and numbers[second_index] == numbers[second_index - 1]:
                continue

            # Two pointers for remaining elements
            left_pointer = second_index + 1
            right_pointer = len(numbers) - 1

            # What the remaining two elements should sum to
            remaining_target = target_sum - numbers[first_index] - numbers[second_index]

            while left_pointer < right_pointer:
                current_pair_sum = numbers[left_pointer] + numbers[right_pointer]

                if current_pair_sum == remaining_target:
                    # Found valid quadruplet
                    result_quadruplets.append([
                        numbers[first_index],
                        numbers[second_index],
                        numbers[left_pointer],
                        numbers[right_pointer]
                    ])

                    # Skip duplicates
                    while left_pointer < right_pointer and numbers[left_pointer] == numbers[left_pointer + 1]:
                        left_pointer += 1
                    while left_pointer < right_pointer and numbers[right_pointer] == numbers[right_pointer - 1]:
                        right_pointer -= 1

                    left_pointer += 1
                    right_pointer -= 1

                elif current_pair_sum < remaining_target:
                    left_pointer += 1
                else:
                    right_pointer -= 1

    return result_quadruplets


# ============================================================================
# PATTERN 4: REMOVE DUPLICATES
# ============================================================================

def remove_duplicates_sorted(numbers: List[int]) -> int:
    """
    Remove duplicates from sorted array in-place.

    Example:
        numbers = [1, 1, 2, 2, 2, 3, 3]
        After: [1, 2, 3, *, *, *, *] (first 3 elements are unique)
        Returns: 3 (length of unique elements)

    SAME-DIRECTION two pointers pattern:
    - Slow pointer: position to place next unique element
    - Fast pointer: scans through array looking for different elements

    Why it works:
    - Slow tracks where to write next unique element
    - Fast finds next different element
    - Copy fast to slow position when found
    - Array is sorted, so duplicates are adjacent

    Time Complexity: O(n)
    Space Complexity: O(1) - in-place modification

    Args:
        numbers: Sorted array of integers (modified in-place)

    Returns:
        Length of array with duplicates removed
    """
    # Handle empty array
    if not numbers:
        return 0

    # Slow pointer: position for next unique element
    slow_pointer = 0

    # Fast pointer: scans for next different element
    for fast_pointer in range(1, len(numbers)):
        # Found different element
        if numbers[fast_pointer] != numbers[slow_pointer]:
            slow_pointer += 1  # Move to next position
            numbers[slow_pointer] = numbers[fast_pointer]  # Copy unique element

    # Return length (slow_pointer is 0-indexed, so add 1)
    return slow_pointer + 1


def remove_duplicates_k_allowed(numbers: List[int], max_occurrences: int) -> int:
    """
    Remove duplicates, allowing each element to appear at most k times.

    Example:
        numbers = [1, 1, 1, 2, 2, 3], k = 2
        After: [1, 1, 2, 2, 3, *]
        Returns: 5

    Strategy:
    - Compare current element with element k positions back
    - If different, can include current element
    - Maintains invariant: no element appears more than k times

    Time Complexity: O(n)
    Space Complexity: O(1)

    Args:
        numbers: Sorted array of integers
        max_occurrences: Maximum allowed occurrences per element

    Returns:
        Length of array with excess duplicates removed
    """
    # If array smaller than k, all elements can stay
    if len(numbers) <= max_occurrences:
        return len(numbers)

    # Slow pointer: position for next element
    # Start at k (first k elements always valid)
    slow_pointer = max_occurrences

    # Fast pointer: scan from position k onward
    for fast_pointer in range(max_occurrences, len(numbers)):
        # Compare with element k positions back
        # If different, current element won't exceed k occurrences
        if numbers[fast_pointer] != numbers[slow_pointer - max_occurrences]:
            numbers[slow_pointer] = numbers[fast_pointer]
            slow_pointer += 1

    return slow_pointer


# ============================================================================
# PATTERN 5: PALINDROME VERIFICATION
# ============================================================================

def is_palindrome(string: str) -> bool:
    """
    Check if string is palindrome (ignore non-alphanumeric, case-insensitive).

    Example:
        "A man, a plan, a canal: Panama" → True
        "race a car" → False

    OPPOSITE-END pointers moving toward middle:
    - Compare characters from both ends
    - Skip non-alphanumeric characters
    - Case-insensitive comparison

    Why it works:
    - Palindrome reads same forward and backward
    - Compare mirrored positions from both ends
    - Both pointers meet in middle if palindrome

    Time Complexity: O(n)
    Space Complexity: O(1) - no extra space needed

    Args:
        string: String to check

    Returns:
        True if palindrome, False otherwise
    """
    left_pointer = 0
    right_pointer = len(string) - 1

    while left_pointer < right_pointer:
        # Skip non-alphanumeric characters from left
        while left_pointer < right_pointer and not string[left_pointer].isalnum():
            left_pointer += 1

        # Skip non-alphanumeric characters from right
        while left_pointer < right_pointer and not string[right_pointer].isalnum():
            right_pointer -= 1

        # Compare characters (case-insensitive)
        if string[left_pointer].lower() != string[right_pointer].lower():
            return False

        # Move both pointers inward
        left_pointer += 1
        right_pointer -= 1

    # All characters matched
    return True


def valid_palindrome_one_deletion(string: str) -> bool:
    """
    Check if string can be palindrome after deleting at most one character.

    Example:
        "aba" → True (already palindrome)
        "abca" → True (delete 'c', becomes "aba")
        "abc" → False (would need more than one deletion)

    Strategy: When mismatch found, try skipping left OR right character.
    - Use helper to check if substring is palindrome
    - If mismatch, try skipping left char OR right char
    - If either works, can form palindrome with one deletion

    Why it works:
    - Regular palindrome check until mismatch
    - At mismatch, the offending character must be one of the two
    - Try removing each and check if rest is palindrome

    Time Complexity: O(n)
    Space Complexity: O(1)

    Args:
        string: String to check

    Returns:
        True if can be palindrome with at most one deletion
    """
    def is_palindrome_range(left_index: int, right_index: int) -> bool:
        """Check if substring is palindrome."""
        while left_index < right_index:
            if string[left_index] != string[right_index]:
                return False
            left_index += 1
            right_index -= 1
        return True

    left_pointer = 0
    right_pointer = len(string) - 1

    while left_pointer < right_pointer:
        # Found mismatch
        if string[left_pointer] != string[right_pointer]:
            # Try skipping left character OR right character
            skip_left = is_palindrome_range(left_pointer + 1, right_pointer)
            skip_right = is_palindrome_range(left_pointer, right_pointer - 1)

            # If either works, can form palindrome with one deletion
            return skip_left or skip_right

        left_pointer += 1
        right_pointer -= 1

    # Already a palindrome (no deletion needed)
    return True


# ============================================================================
# PATTERN 6: PARTITION ARRAYS
# ============================================================================

def partition_array(numbers: List[int], pivot_value: int) -> List[int]:
    """
    Partition array: elements < pivot on left, >= pivot on right.

    Example:
        numbers = [3, 5, 2, 8, 1, 9], pivot = 5
        After: [3, 2, 1, 5, 8, 9] (or similar - order within partitions can vary)

    Strategy: Single pass, swap elements < pivot to left side.

    Time Complexity: O(n)
    Space Complexity: O(1)

    Args:
        numbers: Array of integers
        pivot_value: Pivot value for partitioning

    Returns:
        Partitioned array
    """
    # Next position for element < pivot
    left_boundary = 0

    for current_index in range(len(numbers)):
        # Current element belongs on left side (< pivot)
        if numbers[current_index] < pivot_value:
            # Swap to left boundary position
            numbers[left_boundary], numbers[current_index] = numbers[current_index], numbers[left_boundary]
            left_boundary += 1

    return numbers


def dutch_national_flag(numbers: List[int]) -> List[int]:
    """
    Sort array with three distinct values (0, 1, 2) - Dutch National Flag problem.

    Example:
        numbers = [2, 0, 2, 1, 1, 0]
        After: [0, 0, 1, 1, 2, 2]

    Three pointers pattern:
    - low: boundary for 0s (all elements before are 0)
    - mid: current element being examined
    - high: boundary for 2s (all elements after are 2)

    Why it works:
    - low to mid-1: all 0s
    - mid to high: unprocessed elements
    - high+1 to end: all 2s
    - When 0 found: swap to low and advance both low and mid
    - When 1 found: already in correct region, just advance mid
    - When 2 found: swap to high and decrement high (don't advance mid - need to process swapped element)

    Time Complexity: O(n) - single pass
    Space Complexity: O(1) - in-place sorting

    Args:
        numbers: Array containing only 0s, 1s, and 2s

    Returns:
        Sorted array
    """
    low_boundary = 0  # Boundary for 0s
    mid_pointer = 0  # Current element
    high_boundary = len(numbers) - 1  # Boundary for 2s

    while mid_pointer <= high_boundary:
        if numbers[mid_pointer] == 0:
            # Swap with low boundary and advance both
            numbers[low_boundary], numbers[mid_pointer] = numbers[mid_pointer], numbers[low_boundary]
            low_boundary += 1
            mid_pointer += 1

        elif numbers[mid_pointer] == 1:
            # Already in correct position, just advance mid
            mid_pointer += 1

        else:  # numbers[mid_pointer] == 2
            # Swap with high boundary and decrement high
            # Don't advance mid - need to process swapped element
            numbers[mid_pointer], numbers[high_boundary] = numbers[high_boundary], numbers[mid_pointer]
            high_boundary -= 1

    return numbers


# ============================================================================
# PATTERN 7: CONTAINER WITH MOST WATER
# ============================================================================

def max_area_container(heights: List[int]) -> int:
    """
    Find two lines that form container with maximum water.

    Example:
        heights = [1, 8, 6, 2, 5, 4, 8, 3, 7]
        Output: 49 (lines at indices 1 and 8: min(8,7) * (8-1) = 7 * 7 = 49)

    Area calculation: min(height[left], height[right]) * (right - left)

    Strategy: Start from both ends, move pointer with smaller height.
    - Width decreases as pointers move inward
    - Only way to potentially increase area is to find taller line
    - Moving taller pointer can't help (width decreases, height stays same or decreases)
    - Moving shorter pointer might find taller line and increase area

    Why move shorter pointer:
    - Current area limited by shorter height
    - Moving taller pointer: width decreases, height can only decrease → area definitely decreases
    - Moving shorter pointer: width decreases, but height might increase → area might increase
    - Greedy choice: always move shorter pointer

    Time Complexity: O(n) - each pointer moves at most n times
    Space Complexity: O(1)

    Args:
        heights: Array of heights representing vertical lines

    Returns:
        Maximum water area that can be contained
    """
    left_pointer = 0
    right_pointer = len(heights) - 1
    maximum_area = 0

    while left_pointer < right_pointer:
        # Calculate current area
        container_width = right_pointer - left_pointer
        container_height = min(heights[left_pointer], heights[right_pointer])
        current_area = container_width * container_height

        # Update maximum
        maximum_area = max(maximum_area, current_area)

        # Move pointer with smaller height (limiting factor)
        if heights[left_pointer] < heights[right_pointer]:
            left_pointer += 1
        else:
            right_pointer -= 1

    return maximum_area


# ============================================================================
# PATTERN 8: TRAPPING RAIN WATER
# ============================================================================

def trap_rain_water(heights: List[int]) -> int:
    """
    Calculate total trapped rain water.

    Example:
        heights = [0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1]
        Output: 6
        (Water trapped at indices: 2→1, 4→1, 5→2, 6→1, 9→1)

    Water at position i = min(max_height_left, max_height_right) - height[i]
    - Water level determined by lower of two surrounding walls
    - Subtract current height to get water depth

    Two pointers approach:
    - Track max_left and max_right as we move pointers
    - Process side with smaller max (water level determined by smaller side)
    - Guaranteed that water level is at least the smaller max

    Why it works:
    - Water at position depends on minimum of max_left and max_right
    - If max_left < max_right, we know water level at left is max_left
      (even if max_right could be higher further right, doesn't matter - limited by max_left)
    - Process left side, update max_left, calculate water
    - Mirror logic for right side

    Time Complexity: O(n) - single pass
    Space Complexity: O(1) - only a few variables

    Args:
        heights: Array of heights representing elevation map

    Returns:
        Total trapped water units
    """
    # Handle empty array
    if not heights:
        return 0

    left_pointer = 0
    right_pointer = len(heights) - 1

    # Track maximum heights seen so far from each side
    max_left_height = heights[left_pointer]
    max_right_height = heights[right_pointer]

    trapped_water = 0

    while left_pointer < right_pointer:
        # Process side with smaller max height
        if max_left_height < max_right_height:
            # Water level at left is determined by max_left_height
            left_pointer += 1
            max_left_height = max(max_left_height, heights[left_pointer])

            # Water trapped = water level - current height
            trapped_water += max_left_height - heights[left_pointer]

        else:
            # Water level at right is determined by max_right_height
            right_pointer -= 1
            max_right_height = max(max_right_height, heights[right_pointer])

            # Water trapped = water level - current height
            trapped_water += max_right_height - heights[right_pointer]

    return trapped_water


# ============================================================================
# PATTERN 9: MERGE SORTED ARRAYS
# ============================================================================

def merge_sorted_arrays(first_array: List[int], first_length: int,
                       second_array: List[int], second_length: int) -> None:
    """
    Merge second_array into first_array (first_array has size first_length + second_length).

    Example:
        first_array = [1, 2, 3, 0, 0, 0], first_length = 3
        second_array = [2, 5, 6], second_length = 3
        After: first_array = [1, 2, 2, 3, 5, 6]

    Strategy: Fill from back to avoid overwriting elements.
    - Start from end of both arrays
    - Compare and place larger element at end of first_array
    - Work backward to avoid overwriting unprocessed elements

    Why fill from back:
    - If we filled from front, would overwrite elements in first_array
    - Back of first_array has empty space
    - Working backward, never overwrite unprocessed element

    Time Complexity: O(first_length + second_length)
    Space Complexity: O(1) - in-place modification

    Args:
        first_array: Array with space for merged result (modified in-place)
        first_length: Number of actual elements in first_array
        second_array: Array to merge into first_array
        second_length: Number of elements in second_array

    Returns:
        None (first_array is modified in-place)
    """
    # Pointers to last actual elements in each array
    first_pointer = first_length - 1
    second_pointer = second_length - 1

    # Pointer to last position in first_array (where merged result goes)
    write_pointer = first_length + second_length - 1

    # Continue while there are elements left in second_array
    while second_pointer >= 0:
        # Compare and place larger element (if first_array has elements left)
        if first_pointer >= 0 and first_array[first_pointer] > second_array[second_pointer]:
            first_array[write_pointer] = first_array[first_pointer]
            first_pointer -= 1
        else:
            first_array[write_pointer] = second_array[second_pointer]
            second_pointer -= 1

        write_pointer -= 1


# ============================================================================
# PATTERN 10: SQUARES OF SORTED ARRAY
# ============================================================================

def sorted_squares(numbers: List[int]) -> List[int]:
    """
    Return sorted squares of sorted array (may contain negative numbers).

    Example:
        numbers = [-4, -1, 0, 3, 10]
        Output: [0, 1, 9, 16, 100]

    Challenge: Negatives become positive when squared
    - [-4, -1, 0, 3, 10] squares to [16, 1, 0, 9, 100]
    - Need to sort after squaring

    Optimized strategy: Two pointers from ends, compare absolute values.
    - Largest absolute values are at ends (most negative or most positive)
    - Largest squares at ends
    - Fill result from back with larger square each time

    Why it works:
    - After squaring, largest values come from ends (most negative or most positive)
    - Compare absolute values from both ends
    - Place larger square at end of result
    - Work backward to build sorted result

    Time Complexity: O(n) - single pass (better than O(n log n) sort)
    Space Complexity: O(n) for result array

    Args:
        numbers: Sorted array of integers (may include negatives)

    Returns:
        Sorted array of squared values
    """
    array_length = len(numbers)
    result_squares = [0] * array_length

    left_pointer = 0
    right_pointer = array_length - 1
    write_position = array_length - 1  # Fill from end (largest squares)

    while left_pointer <= right_pointer:
        # Calculate squares
        left_square = numbers[left_pointer] ** 2
        right_square = numbers[right_pointer] ** 2

        # Place larger square at current write position
        if left_square > right_square:
            result_squares[write_position] = left_square
            left_pointer += 1
        else:
            result_squares[write_position] = right_square
            right_pointer -= 1

        write_position -= 1

    return result_squares


# ============================================================================
# TWO POINTERS TEMPLATES - MASTER PATTERNS
# ============================================================================

def two_pointers_templates_explanation():
    """
    THREE MAIN TWO-POINTERS TEMPLATES

    # ================================================================
    # TEMPLATE 1: OPPOSITE ENDS (for sorted arrays)
    # ================================================================

    left_pointer = 0
    right_pointer = len(array) - 1

    while left_pointer < right_pointer:
        # Check condition with current pair
        if condition_met(array[left_pointer], array[right_pointer]):
            # Process and move both pointers
            left_pointer += 1
            right_pointer -= 1

        elif need_larger_value:
            # Move left pointer right to increase value
            left_pointer += 1

        else:
            # Move right pointer left to decrease value
            right_pointer -= 1

    Use cases:
    - Two sum in sorted array
    - Finding pairs/triplets with target sum
    - Palindrome verification
    - Container with most water


    # ================================================================
    # TEMPLATE 2: SAME DIRECTION (fast/slow pointers)
    # ================================================================

    slow_pointer = 0  # Tracks write position or special condition

    for fast_pointer in range(len(array)):
        if condition(array[fast_pointer]):
            # Process element
            array[slow_pointer] = array[fast_pointer]
            slow_pointer += 1

    return slow_pointer  # New length or write position

    Use cases:
    - Remove duplicates from sorted array
    - Remove specific elements
    - Move zeros to end
    - Partition array


    # ================================================================
    # TEMPLATE 3: THREE POINTERS (partition)
    # ================================================================

    low_pointer = 0  # Boundary for first group
    mid_pointer = 0  # Current element being processed
    high_pointer = len(array) - 1  # Boundary for last group

    while mid_pointer <= high_pointer:
        if array[mid_pointer] == value1:
            # Belongs in first group
            swap(array, low_pointer, mid_pointer)
            low_pointer += 1
            mid_pointer += 1

        elif array[mid_pointer] == value2:
            # Belongs in middle group (already in place)
            mid_pointer += 1

        else:
            # Belongs in last group
            swap(array, mid_pointer, high_pointer)
            high_pointer -= 1
            # Don't increment mid - need to process swapped element

    Use cases:
    - Dutch National Flag (sort 0s, 1s, 2s)
    - Partition array into three parts
    - Three-way quicksort


    # ================================================================
    # WHEN TO USE WHICH TEMPLATE
    # ================================================================

    OPPOSITE ENDS:
    - Array is sorted
    - Finding pairs/combinations with target sum
    - Palindrome checking
    - Container/area problems (max area, trapped water)
    - Merging from ends

    SAME DIRECTION:
    - Removing/filtering elements in-place
    - Finding subsequences
    - Partitioning into two groups
    - One pointer explores, other tracks result

    THREE POINTERS:
    - Sorting with few distinct values (0,1,2)
    - Partitioning into three groups
    - Dutch National Flag problem
    - Three-way partitioning


    # ================================================================
    # KEY INSIGHTS
    # ================================================================

    1. For sorted arrays: two pointers often O(n) vs O(n²) brute force

    2. Sum problems pattern:
       - Two sum: two pointers from ends
       - Three sum: fix one + two pointers
       - Four sum: fix two + two pointers

    3. Skip duplicates pattern:
       while left < right and array[left] == array[left + 1]:
           left += 1

    4. Container/area problems:
       - Move pointer with limiting factor
       - Width always decreases, so try to increase height

    5. Merge from back:
       - Avoids overwriting elements in place
       - Use when have space at end of array

    6. Same direction pointers:
       - Slow tracks write position
       - Fast scans for elements to keep

    7. Dutch flag invariant:
       - [0...low-1]: all value1
       - [low...mid-1]: all value2
       - [mid...high]: unprocessed
       - [high+1...end]: all value3
    """
    pass


# ============================================================================
# TEST CASES
# ============================================================================

def run_tests():
    """
    Comprehensive test cases demonstrating all two pointers patterns.
    """
    print("=" * 70)
    print("TWO POINTERS PATTERN - COMPREHENSIVE TEST CASES")
    print("=" * 70)

    # Test 1: Two Sum Sorted
    print("\n1. TWO SUM (SORTED ARRAY)")
    print("-" * 70)
    test_array_1 = [2, 7, 11, 15]
    print(f"   Array: {test_array_1}, Target: 9")
    result_1 = two_sum_sorted(test_array_1, 9)
    print(f"   Output: {result_1}")
    print(f"   Expected: [0, 1] (indices) or [2, 7] (values)")
    print(f"\n   Why: 2 + 7 = 9, found by opposite-end pointers")

    # Test 2: Three Sum
    print("\n2. THREE SUM (TARGET = 0)")
    print("-" * 70)
    test_array_2 = [-1, 0, 1, 2, -1, -4]
    print(f"   Array: {test_array_2}")
    result_2 = three_sum(test_array_2)
    print(f"   Output: {result_2}")
    print(f"   Expected: [[-1, -1, 2], [-1, 0, 1]]")
    print(f"\n   Why: Fix one element, use two pointers for remaining two")

    # Test 3: Remove Duplicates
    print("\n3. REMOVE DUPLICATES FROM SORTED ARRAY")
    print("-" * 70)
    test_array_3 = [1, 1, 2, 2, 2, 3, 3]
    print(f"   Array: {test_array_3}")
    length_3 = remove_duplicates_sorted(test_array_3)
    print(f"   Length: {length_3}, Array: {test_array_3[:length_3]}")
    print(f"   Expected: 3, [1, 2, 3]")
    print(f"\n   Why: Fast pointer finds unique elements, slow tracks write position")

    # Test 4: Palindrome
    print("\n4. VALID PALINDROME")
    print("-" * 70)
    test_string_4 = "A man, a plan, a canal: Panama"
    print(f"   String: '{test_string_4}'")
    result_4 = is_palindrome(test_string_4)
    print(f"   Is palindrome: {result_4}")
    print(f"   Expected: True")
    print(f"\n   Why: Reads same forward and backward (ignoring non-alphanumeric)")

    # Test 5: Dutch National Flag
    print("\n5. DUTCH NATIONAL FLAG (SORT 0s, 1s, 2s)")
    print("-" * 70)
    test_array_5 = [2, 0, 2, 1, 1, 0]
    print(f"   Before: {test_array_5}")
    dutch_national_flag(test_array_5)
    print(f"   After:  {test_array_5}")
    print(f"   Expected: [0, 0, 1, 1, 2, 2]")
    print(f"\n   Why: Three pointers maintain: 0s | 1s | unprocessed | 2s")

    # Test 6: Container With Most Water
    print("\n6. CONTAINER WITH MOST WATER")
    print("-" * 70)
    test_heights_6 = [1, 8, 6, 2, 5, 4, 8, 3, 7]
    print(f"   Heights: {test_heights_6}")
    result_6 = max_area_container(test_heights_6)
    print(f"   Max area: {result_6}")
    print(f"   Expected: 49 (min(8,7) * 7 = 49)")
    print(f"\n   Why: Start from ends, move shorter pointer (limiting factor)")

    # Test 7: Trapping Rain Water
    print("\n7. TRAPPING RAIN WATER")
    print("-" * 70)
    test_heights_7 = [0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1]
    print(f"   Heights: {test_heights_7}")
    result_7 = trap_rain_water(test_heights_7)
    print(f"   Water trapped: {result_7}")
    print(f"   Expected: 6")
    print(f"\n   Why: Water = min(max_left, max_right) - current_height")

    # Test 8: Merge Sorted Arrays
    print("\n8. MERGE SORTED ARRAYS")
    print("-" * 70)
    test_array_8a = [1, 2, 3, 0, 0, 0]
    test_array_8b = [2, 5, 6]
    print(f"   Array 1: {test_array_8a[:3]}, Array 2: {test_array_8b}")
    merge_sorted_arrays(test_array_8a, 3, test_array_8b, 3)
    print(f"   Merged: {test_array_8a}")
    print(f"   Expected: [1, 2, 2, 3, 5, 6]")
    print(f"\n   Why: Fill from back to avoid overwriting unprocessed elements")

    # Test 9: Sorted Squares
    print("\n9. SQUARES OF SORTED ARRAY")
    print("-" * 70)
    test_array_9 = [-4, -1, 0, 3, 10]
    print(f"   Array: {test_array_9}")
    result_9 = sorted_squares(test_array_9)
    print(f"   Squares: {result_9}")
    print(f"   Expected: [0, 1, 9, 16, 100]")
    print(f"\n   Why: Largest squares at ends, compare and fill from back")

    # Summary
    print("\n" + "=" * 70)
    print("KEY INSIGHTS - TWO POINTERS PATTERNS")
    print("=" * 70)
    print("1. Opposite ends: Start from both ends, move based on comparison")
    print("2. Same direction: Slow tracks position, fast scans array")
    print("3. For sorted arrays: two pointers often O(n) vs O(n²) brute force")
    print("4. Sum problems: fix element(s), use two pointers for rest")
    print("5. Skip duplicates: check current == next before processing")
    print("6. Container/area: move pointer with limiting factor (smaller height)")
    print("7. Merge from back: avoids overwriting elements in place")
    print("8. Three pointers: Dutch flag, partition with multiple values")
    print("9. Palindrome: compare from ends moving toward middle")
    print("10. O(1) space: in-place operations with just pointer variables")
    print("=" * 70)


if __name__ == "__main__":
    run_tests()

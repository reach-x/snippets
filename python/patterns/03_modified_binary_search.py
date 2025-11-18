"""
MODIFIED BINARY SEARCH PATTERN - Comprehensive Guide

Binary search is a powerful technique for finding elements in sorted arrays or
search spaces. The "modified" versions extend classic binary search to handle
rotated arrays, finding boundaries, searching 2D matrices, and more.

WHEN TO USE:
- Array is sorted or partially sorted
- Search space can be binary-divided
- Finding boundaries (first/last occurrence)
- Optimization problems with monotonic property
- Peak/valley finding

TIME COMPLEXITY: O(log n) - halve search space each iteration
SPACE COMPLEXITY: O(1) iterative, O(log n) recursive

CORE CONCEPT: Eliminate half of search space based on comparison with middle element

Python's bisect module provides:
- bisect_left: leftmost insertion point (first occurrence)
- bisect_right: rightmost insertion point (last occurrence + 1)
"""

from typing import List
import bisect


# ============================================================================
# PATTERN 1: CLASSIC BINARY SEARCH
# ============================================================================

def binary_search_iterative(sorted_array: List[int], target_value: int) -> int:
    """
    Classic binary search - iterative approach (preferred for efficiency).

    Searches for target_value in sorted_array and returns its index if found,
    or -1 if not found.

    Example:
        array = [1, 3, 5, 7, 9, 11, 13]
        binary_search_iterative(array, 7)  # Returns 3
        binary_search_iterative(array, 6)  # Returns -1

    Why it works:
    - Each comparison eliminates half of the remaining search space
    - With sorted data, comparing to middle element tells us which half contains target
    - Continues until search space is exhausted (left > right) or target found

    Time Complexity: O(log n) - halve search space each iteration
    Space Complexity: O(1) - only a few variables needed

    Args:
        sorted_array: Sorted list of integers to search in
        target_value: Value to search for

    Returns:
        Index of target_value if found, -1 otherwise
    """
    # Initialize search boundaries to entire array
    left_index = 0
    right_index = len(sorted_array) - 1

    # Continue while there are elements to check
    while left_index <= right_index:
        # Calculate middle index (avoid overflow with large numbers)
        # Using (left + right) // 2 works in Python, but left + (right - left) // 2
        # is safer in languages with fixed integer sizes
        middle_index = (left_index + right_index) // 2

        # Check if we found the target
        if sorted_array[middle_index] == target_value:
            return middle_index  # Found! Return the index

        # Target must be in right half (middle is too small)
        elif sorted_array[middle_index] < target_value:
            left_index = middle_index + 1  # Search right half only

        # Target must be in left half (middle is too large)
        else:
            right_index = middle_index - 1  # Search left half only

    # Search exhausted without finding target
    return -1


def binary_search_recursive(sorted_array: List[int], target_value: int) -> int:
    """
    Classic binary search - recursive approach.

    Same logic as iterative version but uses recursion instead of loop.
    Recursive version is more elegant but uses extra space for call stack.

    Example:
        array = [1, 3, 5, 7, 9, 11, 13]
        binary_search_recursive(array, 9)  # Returns 4

    Why recursion works here:
    - Base case: left > right means not found
    - Recursive case: check middle, then recurse on appropriate half
    - Each recursive call works on smaller problem (half the array)

    Time Complexity: O(log n) - halve search space each call
    Space Complexity: O(log n) - recursion stack depth

    Args:
        sorted_array: Sorted list of integers to search in
        target_value: Value to search for

    Returns:
        Index of target_value if found, -1 otherwise
    """
    def search_helper(left_index: int, right_index: int) -> int:
        """Helper function that performs the actual recursive search."""
        # Base case: search space exhausted
        if left_index > right_index:
            return -1

        # Calculate middle index
        middle_index = (left_index + right_index) // 2

        # Found the target at middle position
        if sorted_array[middle_index] == target_value:
            return middle_index

        # Target is larger, search right half
        elif sorted_array[middle_index] < target_value:
            return search_helper(middle_index + 1, right_index)

        # Target is smaller, search left half
        else:
            return search_helper(left_index, middle_index - 1)

    # Start recursive search with full array bounds
    return search_helper(0, len(sorted_array) - 1)


# ============================================================================
# PATTERN 2: FINDING BOUNDARIES (FIRST/LAST OCCURRENCE)
# ============================================================================

def find_first_occurrence(sorted_array: List[int], target_value: int) -> int:
    """
    Find the first (leftmost) occurrence of target_value in array with duplicates.

    Example:
        array = [1, 2, 2, 2, 3, 4, 5]
        find_first_occurrence(array, 2)  # Returns 1 (first occurrence of 2)

    Template for finding LEFT boundary:
    - When array[middle] == target, DON'T RETURN - keep searching left
    - Store the found index and continue searching left half
    - This ensures we find the leftmost occurrence

    Why it works:
    - Standard binary search returns any occurrence
    - By continuing to search left even after finding target, we ensure leftmost
    - Result variable tracks the leftmost found so far

    Time Complexity: O(log n)
    Space Complexity: O(1)

    Args:
        sorted_array: Sorted list with possible duplicates
        target_value: Value to find first occurrence of

    Returns:
        Index of first occurrence, or -1 if not found
    """
    left_index = 0
    right_index = len(sorted_array) - 1
    result_index = -1  # Track leftmost occurrence found so far

    while left_index <= right_index:
        middle_index = (left_index + right_index) // 2

        # Found target, but there might be earlier occurrences
        if sorted_array[middle_index] == target_value:
            result_index = middle_index  # Store this occurrence
            right_index = middle_index - 1  # Continue searching LEFT

        # Target is in right half
        elif sorted_array[middle_index] < target_value:
            left_index = middle_index + 1

        # Target is in left half
        else:
            right_index = middle_index - 1

    return result_index


def find_last_occurrence(sorted_array: List[int], target_value: int) -> int:
    """
    Find the last (rightmost) occurrence of target_value in array with duplicates.

    Example:
        array = [1, 2, 2, 2, 3, 4, 5]
        find_last_occurrence(array, 2)  # Returns 3 (last occurrence of 2)

    Template for finding RIGHT boundary:
    - When array[middle] == target, DON'T RETURN - keep searching right
    - Store the found index and continue searching right half
    - This ensures we find the rightmost occurrence

    Why it works:
    - Mirror of find_first_occurrence
    - By continuing to search right even after finding target, we ensure rightmost
    - Result variable tracks the rightmost found so far

    Time Complexity: O(log n)
    Space Complexity: O(1)

    Args:
        sorted_array: Sorted list with possible duplicates
        target_value: Value to find last occurrence of

    Returns:
        Index of last occurrence, or -1 if not found
    """
    left_index = 0
    right_index = len(sorted_array) - 1
    result_index = -1  # Track rightmost occurrence found so far

    while left_index <= right_index:
        middle_index = (left_index + right_index) // 2

        # Found target, but there might be later occurrences
        if sorted_array[middle_index] == target_value:
            result_index = middle_index  # Store this occurrence
            left_index = middle_index + 1  # Continue searching RIGHT

        # Target is in right half
        elif sorted_array[middle_index] < target_value:
            left_index = middle_index + 1

        # Target is in left half
        else:
            right_index = middle_index - 1

    return result_index


def find_range(sorted_array: List[int], target_value: int) -> List[int]:
    """
    Find first and last position of target_value in sorted array.

    Example:
        array = [1, 2, 2, 2, 3, 4, 5]
        find_range(array, 2)  # Returns [1, 3] (indices of first and last 2)
        find_range(array, 6)  # Returns [-1, -1] (not found)

    Time Complexity: O(log n) - two binary searches
    Space Complexity: O(1)

    Args:
        sorted_array: Sorted list with possible duplicates
        target_value: Value to find range for

    Returns:
        List containing [first_index, last_index], or [-1, -1] if not found
    """
    # Use both boundary-finding functions
    first_occurrence = find_first_occurrence(sorted_array, target_value)
    last_occurrence = find_last_occurrence(sorted_array, target_value)

    return [first_occurrence, last_occurrence]


# ============================================================================
# PATTERN 3: PYTHON BISECT MODULE
# ============================================================================

def bisect_examples(sorted_array: List[int], target_value: int):
    """
    Demonstrate Python's powerful bisect module for binary search operations.

    bisect_left(array, value):  Leftmost insertion point where array[i] >= value
                                (index of first occurrence if exists)

    bisect_right(array, value): Rightmost insertion point where array[i] > value
                                (index after last occurrence if exists)

    Example with duplicates:
        array = [1, 2, 2, 2, 3, 4, 5]
        bisect_left(array, 2)  = 1 (first 2)
        bisect_right(array, 2) = 4 (after last 2)

    For duplicates:
    - bisect_left gives index of first occurrence
    - bisect_right gives index after last occurrence
    - difference (right - left) gives count of occurrences

    Args:
        sorted_array: Sorted list to search in
        target_value: Value to find insertion points for
    """
    # Find insertion points using bisect module
    left_insertion_point = bisect.bisect_left(sorted_array, target_value)
    right_insertion_point = bisect.bisect_right(sorted_array, target_value)

    print(f"Array: {sorted_array}, Target: {target_value}")
    print(f"bisect_left:  {left_insertion_point}")
    print(f"bisect_right: {right_insertion_point}")

    # Check if target exists (left points to it if it exists)
    target_exists = (left_insertion_point < len(sorted_array) and
                    sorted_array[left_insertion_point] == target_value)
    print(f"Target exists: {target_exists}")

    # Count occurrences (difference between right and left positions)
    occurrence_count = right_insertion_point - left_insertion_point
    print(f"Occurrences: {occurrence_count}")

    return left_insertion_point, right_insertion_point


def count_occurrences_bisect(sorted_array: List[int], target_value: int) -> int:
    """
    Count occurrences of target_value using bisect module.

    Example:
        array = [1, 2, 2, 2, 3, 4, 5]
        count_occurrences_bisect(array, 2)  # Returns 3

    Why this works:
    - bisect_left finds first occurrence index
    - bisect_right finds index after last occurrence
    - difference gives total count

    Time Complexity: O(log n) - two binary searches
    Space Complexity: O(1)

    Args:
        sorted_array: Sorted list with possible duplicates
        target_value: Value to count occurrences of

    Returns:
        Count of occurrences in array
    """
    # Get both boundary positions
    left_boundary = bisect.bisect_left(sorted_array, target_value)
    right_boundary = bisect.bisect_right(sorted_array, target_value)

    # Difference gives count
    return right_boundary - left_boundary


# ============================================================================
# PATTERN 4: SEARCH IN ROTATED SORTED ARRAY
# ============================================================================

def search_rotated_array(numbers: List[int], target_value: int) -> int:
    """
    Search for target in rotated sorted array (no duplicates).

    A sorted array that has been rotated at some pivot point.

    Example:
        Original sorted: [0, 1, 2, 4, 5, 6, 7]
        Rotated at 4:    [4, 5, 6, 7, 0, 1, 2]
        search_rotated_array([4,5,6,7,0,1,2], 0)  # Returns 4

    Key insight: At least one half is always properly sorted.
    - Identify which half is sorted (compare left and middle)
    - Check if target is in the sorted half's range
    - Search appropriate half

    Why it works:
    - Rotation creates two sorted subarrays
    - At any middle point, one half must be properly sorted
    - If numbers[left] <= numbers[middle], left half is sorted
    - Otherwise, right half is sorted
    - Check if target is within sorted half's range to decide direction

    Time Complexity: O(log n) - binary search with rotation check
    Space Complexity: O(1)

    Args:
        numbers: Rotated sorted array (no duplicates)
        target_value: Value to search for

    Returns:
        Index of target if found, -1 otherwise
    """
    left_index = 0
    right_index = len(numbers) - 1

    while left_index <= right_index:
        middle_index = (left_index + right_index) // 2

        # Found the target
        if numbers[middle_index] == target_value:
            return middle_index

        # Determine which half is properly sorted
        if numbers[left_index] <= numbers[middle_index]:
            # Left half is sorted (no rotation in left half)
            # Check if target is in the sorted left half's range
            if numbers[left_index] <= target_value < numbers[middle_index]:
                # Target is in sorted left half
                right_index = middle_index - 1
            else:
                # Target must be in right half
                left_index = middle_index + 1
        else:
            # Right half is sorted (rotation is in left half)
            # Check if target is in the sorted right half's range
            if numbers[middle_index] < target_value <= numbers[right_index]:
                # Target is in sorted right half
                left_index = middle_index + 1
            else:
                # Target must be in left half
                right_index = middle_index - 1

    # Target not found
    return -1


def find_minimum_rotated_array(numbers: List[int]) -> int:
    """
    Find minimum element in rotated sorted array.

    Example:
        array = [4, 5, 6, 7, 0, 1, 2]
        find_minimum_rotated_array(array)  # Returns 0

    Key insight: Minimum is at the rotation point.
    - If numbers[middle] > numbers[right], rotation/minimum is in right half
    - Otherwise, minimum is in left half (including middle)

    Why it works:
    - In non-rotated array: numbers[middle] < numbers[right] always
    - If numbers[middle] > numbers[right], there's a rotation point between middle and right
    - The rotation point is where maximum meets minimum
    - Keep narrowing until we find it

    Time Complexity: O(log n)
    Space Complexity: O(1)

    Args:
        numbers: Rotated sorted array

    Returns:
        Minimum value in array
    """
    left_index = 0
    right_index = len(numbers) - 1

    # Note: using left < right (not <=) because we're finding a position, not a value
    while left_index < right_index:
        middle_index = (left_index + right_index) // 2

        # Middle is greater than right, so rotation/minimum is in right half
        if numbers[middle_index] > numbers[right_index]:
            # Minimum must be in right half (after middle)
            left_index = middle_index + 1
        else:
            # Minimum is in left half (including middle)
            # Don't exclude middle because it could be the minimum
            right_index = middle_index

    # Left and right converge on the minimum
    return numbers[left_index]


# ============================================================================
# PATTERN 5: SEARCH IN 2D MATRIX
# ============================================================================

def search_2d_matrix(matrix: List[List[int]], target_value: int) -> bool:
    """
    Search in 2D matrix where:
    - Each row is sorted left to right
    - First element of each row > last element of previous row

    This means the entire matrix can be viewed as one sorted 1D array.

    Example:
        matrix = [
            [1,  3,  5,  7],
            [10, 11, 16, 20],
            [23, 30, 34, 60]
        ]
        search_2d_matrix(matrix, 3)   # Returns True
        search_2d_matrix(matrix, 13)  # Returns False

    Strategy: Treat as 1D sorted array
    - Total elements = rows * columns
    - Convert 1D index to 2D: row = index // columns, column = index % columns
    - Apply standard binary search

    Why it works:
    - Matrix properties make it equivalent to sorted 1D array
    - Can use binary search on "virtual" 1D array
    - Convert indices when accessing actual 2D matrix

    Time Complexity: O(log(rows * columns))
    Space Complexity: O(1)

    Args:
        matrix: 2D sorted matrix
        target_value: Value to search for

    Returns:
        True if target found, False otherwise
    """
    # Handle empty matrix
    if not matrix or not matrix[0]:
        return False

    total_rows = len(matrix)
    total_columns = len(matrix[0])

    # Binary search on "virtual" 1D array
    left_index = 0
    right_index = total_rows * total_columns - 1

    while left_index <= right_index:
        middle_index = (left_index + right_index) // 2

        # Convert 1D middle_index to 2D coordinates
        # divmod returns (quotient, remainder) in one operation
        matrix_row, matrix_column = divmod(middle_index, total_columns)
        middle_value = matrix[matrix_row][matrix_column]

        # Standard binary search comparisons
        if middle_value == target_value:
            return True
        elif middle_value < target_value:
            left_index = middle_index + 1
        else:
            right_index = middle_index - 1

    return False


def search_2d_matrix_ii(matrix: List[List[int]], target_value: int) -> bool:
    """
    Search in matrix where:
    - Each row is sorted left to right
    - Each column is sorted top to bottom
    - BUT first element of row may NOT be > last of previous row

    Example:
        matrix = [
            [1,  4,  7,  11],
            [2,  5,  8,  12],
            [3,  6,  9,  16],
            [10, 13, 14, 17]
        ]
        search_2d_matrix_ii(matrix, 5)   # Returns True
        search_2d_matrix_ii(matrix, 20)  # Returns False

    Strategy: Start from top-right (or bottom-left) corner
    - From top-right: go left if target smaller, down if target larger
    - Each move eliminates one row or column

    Why top-right corner works:
    - All elements to the left are smaller (same row, sorted)
    - All elements below are larger (same column, sorted)
    - Can make definitive decision: left or down
    - Each move eliminates entire row or column

    Time Complexity: O(rows + columns) - worst case touch all rows and columns
    Space Complexity: O(1)

    Args:
        matrix: 2D matrix sorted by rows and columns
        target_value: Value to search for

    Returns:
        True if target found, False otherwise
    """
    # Handle empty matrix
    if not matrix or not matrix[0]:
        return False

    total_rows = len(matrix)
    total_columns = len(matrix[0])

    # Start from top-right corner
    current_row = 0
    current_column = total_columns - 1

    # Continue while within matrix bounds
    while current_row < total_rows and current_column >= 0:
        current_value = matrix[current_row][current_column]

        # Found the target
        if current_value == target_value:
            return True

        # Current value too large, eliminate this column (move left)
        elif current_value > target_value:
            current_column -= 1

        # Current value too small, eliminate this row (move down)
        else:
            current_row += 1

    # Target not found
    return False


# ============================================================================
# PATTERN 6: PEAK FINDING
# ============================================================================

def find_peak_element(numbers: List[int]) -> int:
    """
    Find index of a peak element (element greater than its neighbors).
    Array boundaries are considered negative infinity.

    Example:
        array = [1, 2, 3, 1]
        find_peak_element(array)  # Returns 2 (value 3 is peak)

        array = [1, 2, 1, 3, 5, 6, 4]
        find_peak_element(array)  # Returns 5 (value 6 is a peak)

    Key insight: Always move toward the higher neighbor.
    - If numbers[middle] < numbers[middle + 1], peak is in right half
    - Otherwise, peak is in left half (including middle)

    Why it works:
    - Array boundaries are -infinity, so there's always at least one peak
    - If middle < right neighbor, right side goes up (peak is right)
    - If middle >= right neighbor, we're on downslope or peak (peak is left/here)
    - By always moving toward increasing values, we must hit a peak

    Time Complexity: O(log n)
    Space Complexity: O(1)

    Args:
        numbers: Array of integers

    Returns:
        Index of a peak element (any peak if multiple exist)
    """
    left_index = 0
    right_index = len(numbers) - 1

    # Continue until left and right converge
    while left_index < right_index:
        middle_index = (left_index + right_index) // 2

        # Middle is smaller than right neighbor, so we're going uphill
        if numbers[middle_index] < numbers[middle_index + 1]:
            # Peak must be in right half (after middle)
            left_index = middle_index + 1
        else:
            # We're going downhill or at peak, search left (including middle)
            right_index = middle_index

    # Left and right have converged on a peak
    return left_index


# ============================================================================
# PATTERN 7: SEARCH INSERT POSITION
# ============================================================================

def search_insert_position(numbers: List[int], target_value: int) -> int:
    """
    Find index where target should be inserted to maintain sorted order.

    Example:
        array = [1, 3, 5, 6]
        search_insert_position(array, 5)  # Returns 2 (found at index 2)
        search_insert_position(array, 2)  # Returns 1 (insert before 3)
        search_insert_position(array, 7)  # Returns 4 (insert at end)
        search_insert_position(array, 0)  # Returns 0 (insert at start)

    This is equivalent to bisect_left from bisect module.

    Why it works:
    - Standard binary search, but when not found, left_index points to insert position
    - Left_index ends up at the position where all elements before are < target
    - All elements at or after are >= target

    Time Complexity: O(log n)
    Space Complexity: O(1)

    Args:
        numbers: Sorted array
        target_value: Value to insert

    Returns:
        Index where target should be inserted
    """
    left_index = 0
    right_index = len(numbers) - 1

    while left_index <= right_index:
        middle_index = (left_index + right_index) // 2

        # Found exact match
        if numbers[middle_index] == target_value:
            return middle_index

        # Target is larger, search right
        elif numbers[middle_index] < target_value:
            left_index = middle_index + 1

        # Target is smaller, search left
        else:
            right_index = middle_index - 1

    # Not found: left_index is the insertion position
    # All elements before left_index are < target
    # All elements from left_index onward are >= target
    return left_index


# ============================================================================
# PATTERN 8: SEARCH IN INFINITE ARRAY
# ============================================================================

def search_infinite_array(reader, target_value: int) -> int:
    """
    Search in infinite sorted array (or very large array where size is unknown).

    Assume 'reader' object has reader.get(index) method that returns value at index,
    or a very large number if index is out of bounds.

    Example:
        Infinite array conceptually: [1, 2, 3, 4, 5, 6, 7, ...]
        search_infinite_array(reader, 5)  # Returns 4

    Strategy:
    1. Find bounds: exponentially expand right until array[right] >= target
    2. Binary search within those bounds

    Why exponential expansion works:
    - Don't know array size, so can't start with right = n-1
    - Doubling right index: 1, 2, 4, 8, 16, 32, ...
    - If target is at position k, we find bounds in O(log k) operations
    - Then binary search in O(log k) more operations

    Time Complexity: O(log n) where n is position of target
    Space Complexity: O(1)

    Args:
        reader: Object with get(index) method for reading array
        target_value: Value to search for

    Returns:
        Index of target if found, -1 otherwise
    """
    # Phase 1: Find bounds by exponentially expanding right
    left_index = 0
    right_index = 1

    # Double right_index until we pass the target
    while reader.get(right_index) < target_value:
        left_index = right_index  # Previous right becomes new left
        right_index *= 2  # Exponentially expand

    # Phase 2: Binary search within found bounds
    while left_index <= right_index:
        middle_index = (left_index + right_index) // 2
        current_value = reader.get(middle_index)

        # Found target
        if current_value == target_value:
            return middle_index

        # Target is larger
        elif current_value < target_value:
            left_index = middle_index + 1

        # Target is smaller
        else:
            right_index = middle_index - 1

    # Not found
    return -1


# ============================================================================
# PATTERN 9: FIND SMALLEST LETTER GREATER THAN TARGET
# ============================================================================

def next_greatest_letter(letters: List[str], target_character: str) -> str:
    """
    Find smallest letter greater than target in circular sorted array.

    Example:
        letters = ['c', 'f', 'j']
        next_greatest_letter(letters, 'a')  # Returns 'c' (smallest > 'a')
        next_greatest_letter(letters, 'c')  # Returns 'f' (smallest > 'c')
        next_greatest_letter(letters, 'k')  # Returns 'c' (circular, wraps to start)

    Why it works:
    - If target >= last letter, wrap around (return first)
    - Otherwise, binary search for smallest letter > target
    - Modified binary search that always moves right when letters[middle] <= target
    - Converges on first letter strictly greater than target

    Time Complexity: O(log n)
    Space Complexity: O(1)

    Args:
        letters: Sorted array of characters (circular)
        target_character: Character to find next greatest after

    Returns:
        Smallest character greater than target (wraps circularly)
    """
    # Handle circular case: if target >= last letter, return first
    if target_character >= letters[-1]:
        return letters[0]

    left_index = 0
    right_index = len(letters) - 1

    # Find first letter strictly greater than target
    while left_index < right_index:
        middle_index = (left_index + right_index) // 2

        # Middle letter is <= target, search right for something greater
        if letters[middle_index] <= target_character:
            left_index = middle_index + 1

        # Middle letter is > target, but there might be smaller option to the left
        else:
            right_index = middle_index

    # Left has converged on first letter > target
    return letters[left_index]


# ============================================================================
# BINARY SEARCH TEMPLATES - MASTER PATTERNS
# ============================================================================

def binary_search_template_1(sorted_array: List[int], target_value: int) -> int:
    """
    TEMPLATE 1: Basic Binary Search

    Use when: Accessing single element determines which half to search

    Loop condition: left <= right (can check exact match)
    Update: left = middle + 1, right = middle - 1 (exclude middle each time)
    Return: specific index or -1

    Best for: Finding exact element, search insert position

    Example:
        array = [1, 3, 5, 7, 9]
        binary_search_template_1(array, 5)  # Returns 2
    """
    left_index = 0
    right_index = len(sorted_array) - 1

    while left_index <= right_index:
        middle_index = (left_index + right_index) // 2

        if sorted_array[middle_index] == target_value:
            return middle_index
        elif sorted_array[middle_index] < target_value:
            left_index = middle_index + 1
        else:
            right_index = middle_index - 1

    return -1


def binary_search_template_2(sorted_array: List[int], target_value: int) -> int:
    """
    TEMPLATE 2: Finding Left/Right Boundary

    Use when: Need to access element and its neighbor

    Loop condition: left < right (not <=, will converge)
    Update: left = middle + 1, right = middle (or vice versa, keep one side inclusive)
    Return: left (left and right converge to same position)

    Best for: Finding boundaries, peak finding, rotated array minimum

    Example:
        array = [1, 2, 2, 2, 3]
        binary_search_template_2(array, 2)  # Returns 1 (first occurrence)
    """
    left_index = 0
    right_index = len(sorted_array) - 1

    while left_index < right_index:
        middle_index = (left_index + right_index) // 2

        # Adjust condition based on what you're looking for
        if sorted_array[middle_index] < target_value:
            left_index = middle_index + 1  # Exclude middle
        else:
            right_index = middle_index  # Include middle

    # Verify the converged position contains target
    return left_index if sorted_array[left_index] == target_value else -1


# ============================================================================
# TEST CASES
# ============================================================================

def run_tests():
    """
    Comprehensive test cases demonstrating all binary search patterns.
    """
    print("=" * 70)
    print("MODIFIED BINARY SEARCH PATTERN - COMPREHENSIVE TEST CASES")
    print("=" * 70)

    # Test 1: Classic Binary Search
    print("\n1. CLASSIC BINARY SEARCH")
    print("-" * 70)
    test_array_1 = [1, 3, 5, 7, 9, 11, 13]
    print(f"   Array: {test_array_1}")
    print(f"   Search for 7:  Index {binary_search_iterative(test_array_1, 7)}")
    print(f"   Search for 6:  Index {binary_search_iterative(test_array_1, 6)}")
    print(f"   Expected: 3 (found), -1 (not found)")
    print(f"\n   Why: Binary search eliminates half the array each comparison")
    print(f"   7 is at index 3, 6 doesn't exist so returns -1")

    # Test 2: First and Last Occurrence
    print("\n2. FIND FIRST AND LAST OCCURRENCE (BOUNDARIES)")
    print("-" * 70)
    test_array_2 = [1, 2, 2, 2, 3, 4, 5]
    print(f"   Array: {test_array_2}, Target: 2")
    range_result = find_range(test_array_2, 2)
    print(f"   Range: {range_result}")
    print(f"   Expected: [1, 3]")
    print(f"\n   Why: Value 2 appears at indices 1, 2, 3")
    print(f"   First occurrence: index 1, Last occurrence: index 3")

    # Test 3: Bisect Module
    print("\n3. PYTHON BISECT MODULE")
    print("-" * 70)
    test_array_3 = [1, 2, 2, 2, 3, 4, 5]
    print(f"   Testing with array: {test_array_3}, target: 2")
    bisect_examples(test_array_3, 2)
    print(f"   Count using bisect: {count_occurrences_bisect(test_array_3, 2)}")
    print(f"\n   Why: bisect_right - bisect_left = number of occurrences")
    print(f"   bisect_left=1 (first 2), bisect_right=4 (after last 2), count=3")

    # Test 4: Search Rotated Array
    print("\n4. SEARCH IN ROTATED SORTED ARRAY")
    print("-" * 70)
    test_array_4 = [4, 5, 6, 7, 0, 1, 2]
    print(f"   Rotated Array: {test_array_4}")
    print(f"   Search for 0:  Index {search_rotated_array(test_array_4, 0)}")
    print(f"   Search for 3:  Index {search_rotated_array(test_array_4, 3)}")
    print(f"   Expected: 4 (found), -1 (not found)")
    print(f"\n   Why: Array rotated at index 4 (where 7 meets 0)")
    print(f"   One half is always sorted - identify it and check if target in range")

    # Test 5: Find Minimum in Rotated Array
    print("\n5. FIND MINIMUM IN ROTATED ARRAY")
    print("-" * 70)
    test_array_5 = [4, 5, 6, 7, 0, 1, 2]
    print(f"   Rotated Array: {test_array_5}")
    minimum_value = find_minimum_rotated_array(test_array_5)
    print(f"   Minimum: {minimum_value}")
    print(f"   Expected: 0")
    print(f"\n   Why: Minimum is at rotation point (where max meets min)")
    print(f"   If middle > right, rotation is in right half")

    # Test 6: Search 2D Matrix
    print("\n6. SEARCH 2D MATRIX (FULLY SORTED)")
    print("-" * 70)
    test_matrix_6 = [[1, 3, 5, 7], [10, 11, 16, 20], [23, 30, 34, 60]]
    print(f"   Matrix: {test_matrix_6}")
    print(f"   Search for 3:   {search_2d_matrix(test_matrix_6, 3)}")
    print(f"   Search for 13:  {search_2d_matrix(test_matrix_6, 13)}")
    print(f"   Expected: True, False")
    print(f"\n   Why: Matrix is sorted like 1D array [1,3,5,7,10,11,16,20,23,30,34,60]")
    print(f"   Use binary search with index conversion: row=index//cols, col=index%cols")

    # Test 7: Find Peak Element
    print("\n7. FIND PEAK ELEMENT")
    print("-" * 70)
    test_array_7 = [1, 2, 3, 1]
    peak_index = find_peak_element(test_array_7)
    print(f"   Array: {test_array_7}")
    print(f"   Peak index: {peak_index}")
    print(f"   Peak value: {test_array_7[peak_index]}")
    print(f"   Expected: index 2, value 3")
    print(f"\n   Why: Element 3 at index 2 is greater than both neighbors (2 and 1)")
    print(f"   Always move toward higher neighbor to find peak")

    # Test 8: Search Insert Position
    print("\n8. SEARCH INSERT POSITION")
    print("-" * 70)
    test_array_8 = [1, 3, 5, 6]
    print(f"   Array: {test_array_8}")
    print(f"   Insert position for 5: {search_insert_position(test_array_8, 5)}")
    print(f"   Insert position for 2: {search_insert_position(test_array_8, 2)}")
    print(f"   Insert position for 7: {search_insert_position(test_array_8, 7)}")
    print(f"   Expected: 2 (found at 2), 1 (insert before 3), 4 (insert at end)")
    print(f"\n   Why: Returns index where target exists or should be inserted")
    print(f"   Maintains sorted order after insertion")

    # Test 9: Next Greatest Letter
    print("\n9. NEXT GREATEST LETTER (CIRCULAR)")
    print("-" * 70)
    test_letters_9 = ['c', 'f', 'j']
    print(f"   Letters: {test_letters_9}")
    print(f"   Next after 'a': {next_greatest_letter(test_letters_9, 'a')}")
    print(f"   Next after 'c': {next_greatest_letter(test_letters_9, 'c')}")
    print(f"   Next after 'k': {next_greatest_letter(test_letters_9, 'k')}")
    print(f"   Expected: 'c' (smallest > a), 'f' (smallest > c), 'c' (wraps circularly)")
    print(f"\n   Why: Finds next letter in circular fashion")
    print(f"   If target >= last letter, wraps to first letter")

    # Summary
    print("\n" + "=" * 70)
    print("KEY INSIGHTS - BINARY SEARCH PATTERNS")
    print("=" * 70)
    print("1. bisect_left: First occurrence / leftmost insertion point")
    print("2. bisect_right: After last occurrence / rightmost insertion point")
    print("3. Finding first: When found, continue searching LEFT (right = mid - 1)")
    print("4. Finding last: When found, continue searching RIGHT (left = mid + 1)")
    print("5. Rotated array: Identify sorted half, check if target in its range")
    print("6. 2D matrix (fully sorted): Treat as 1D, use divmod for coordinates")
    print("7. 2D matrix (row/col sorted): Start top-right, move left or down")
    print("8. Peak finding: Always move toward higher neighbor")
    print("9. Template choice:")
    print("   - left <= right: Find exact match, exclude middle each iteration")
    print("   - left < right: Find boundary, converge to position")
    print("=" * 70)


if __name__ == "__main__":
    run_tests()

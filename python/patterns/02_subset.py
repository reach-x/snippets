"""
SUBSET PATTERN (BACKTRACKING) - Comprehensive Guide with Full Documentation

The subset pattern uses backtracking to generate all possible combinations,
permutations, or subsets of a given set. This is a fundamental technique for
combinatorial problems.

WHEN TO USE:
- Generate all subsets/combinations/permutations
- Problems asking for "all possible ways" or "find all solutions"
- Decision tree exploration (choose/not choose at each step)
- Constraint satisfaction problems
- Enumeration problems

TIME COMPLEXITY:
- Subsets: O(2^n) - each element can be included or excluded (2 choices)
- Permutations: O(n! * n) - n! permutations, each takes O(n) to build
- Combinations: O(C(n,k) * k) where C(n,k) is binomial coefficient

SPACE COMPLEXITY: O(n) for recursion depth + O(output size) for results

CORE CONCEPT:
At each step in the decision tree:
1. Make a choice (include/exclude element, or choose next element)
2. Recurse to explore that choice
3. Backtrack (undo the choice) to try other options

The key is to systematically explore all possibilities without missing any.
"""

from typing import List, Set


# ============================================================================
# QUICK REFERENCE - COPY-PASTE TEMPLATES
# ============================================================================
"""
Use these minimal templates during interviews. Copy and adapt as needed.
CRITICAL: Always copy current_path with current_path[:] when adding to result!
"""

# TEMPLATE 1: Subsets (all combinations)
def subsets_template(nums: List[int]) -> List[List[int]]:
    """Generate all subsets (power set)."""
    result = []

    def backtrack(start_index: int, current_path: List[int]):
        result.append(current_path[:])  # CRITICAL: Make a copy!

        for i in range(start_index, len(nums)):
            current_path.append(nums[i])  # Make choice
            backtrack(i + 1, current_path)  # Explore
            current_path.pop()  # Undo choice (backtrack)

    backtrack(0, [])
    return result


# TEMPLATE 2: Permutations (all orderings)
def permutations_template(nums: List[int]) -> List[List[int]]:
    """Generate all permutations."""
    result = []

    def backtrack(current_path: List[int]):
        if len(current_path) == len(nums):
            result.append(current_path[:])  # CRITICAL: Make a copy!
            return

        for num in nums:
            if num in current_path:  # Skip if already used
                continue
            current_path.append(num)  # Make choice
            backtrack(current_path)  # Explore
            current_path.pop()  # Undo choice

    backtrack([])
    return result


# TEMPLATE 3: Combinations (choose k elements)
def combinations_template(n: int, k: int) -> List[List[int]]:
    """Choose k numbers from 1 to n."""
    result = []

    def backtrack(start: int, current_path: List[int]):
        if len(current_path) == k:
            result.append(current_path[:])  # Found k elements
            return

        for i in range(start, n + 1):
            current_path.append(i)
            backtrack(i + 1, current_path)  # Next element must be > i
            current_path.pop()

    backtrack(1, [])
    return result


# TEMPLATE 4: Combination Sum (with reuse allowed)
def combination_sum_template(candidates: List[int], target: int) -> List[List[int]]:
    """Find all combinations that sum to target (can reuse numbers)."""
    result = []

    def backtrack(start: int, current_path: List[int], current_sum: int):
        if current_sum == target:
            result.append(current_path[:])
            return
        if current_sum > target:
            return  # Prune: exceeded target

        for i in range(start, len(candidates)):
            current_path.append(candidates[i])
            backtrack(i, current_path, current_sum + candidates[i])  # i not i+1 (reuse)
            current_path.pop()

    backtrack(0, [], 0)
    return result


# ============================================================================
# PATTERN 1: SUBSETS (POWER SET)
# ============================================================================

def subsets(numbers: List[int]) -> List[List[int]]:
    """
    Generate all possible subsets (power set) of a set.

    Example: numbers = [1, 2, 3]
             Power set = [[], [1], [2], [1,2], [3], [1,3], [2,3], [1,2,3]]
             Total: 2^3 = 8 subsets

    Approach: Backtracking with Include/Exclude decision tree
    - At each number, we have 2 choices: include it or exclude it
    - Explore all paths through this decision tree

    Args:
        numbers: List of integers (assumed to have no duplicates)

    Returns:
        List of all possible subsets

    Time Complexity: O(2^n) where n is length of numbers
    Space Complexity: O(n) for recursion depth
    """
    all_subsets = []

    def backtrack(start_index: int, current_subset: List[int]):
        """
        Recursive function to build subsets.

        Args:
            start_index: Index to start considering numbers from
            current_subset: Subset built so far
        """
        # Add current subset to result (every path is valid)
        # IMPORTANT: Make a copy because we'll modify current_subset
        all_subsets.append(current_subset[:])

        # Try adding each remaining number to current subset
        for element_index in range(start_index, len(numbers)):
            # Choice: Include numbers[element_index]
            current_subset.append(numbers[element_index])

            # Recurse with next position (element_index + 1)
            # This ensures we don't reuse same element
            backtrack(element_index + 1, current_subset)

            # Backtrack: Remove the number we just added
            # This allows us to try other combinations
            current_subset.pop()

    # Start with empty subset at index 0
    backtrack(0, [])
    return all_subsets


def subsets_iterative(numbers: List[int]) -> List[List[int]]:
    """
    Generate subsets iteratively (BFS/level-by-level approach).

    Example: numbers = [1, 2, 3]
             Start: [[]]
             Add 1: [[], [1]]
             Add 2: [[], [1], [2], [1,2]]
             Add 3: [[], [1], [2], [1,2], [3], [1,3], [2,3], [1,2,3]]

    For each number, we add it to all existing subsets to create new subsets.

    Args:
        numbers: List of integers

    Returns:
        List of all possible subsets

    Time Complexity: O(2^n)
    Space Complexity: O(2^n) for output
    """
    all_subsets = [[]]  # Start with empty subset

    # For each number in the array
    for number in numbers:
        # For each existing subset, create a new subset by adding current number
        new_subsets = []

        for existing_subset in all_subsets:
            # Create new subset: existing subset + current number
            new_subset = existing_subset + [number]
            new_subsets.append(new_subset)

        # Add all new subsets to our result
        all_subsets.extend(new_subsets)

    return all_subsets


def subsets_with_duplicates(numbers: List[int]) -> List[List[int]]:
    """
    Generate all unique subsets when array contains duplicate elements.

    Example: numbers = [1, 2, 2]
             Subsets: [[], [1], [1,2], [1,2,2], [2], [2,2]]
             Note: [1,2] and [2] appear only once despite two 2s

    Key Strategy:
    1. Sort array first to group duplicates together
    2. Skip duplicates at same recursion level to avoid duplicate subsets
    3. Only skip if it's not the first occurrence at this level

    Args:
        numbers: List of integers (may contain duplicates)

    Returns:
        List of all unique subsets

    Time Complexity: O(2^n)
    Space Complexity: O(n) for recursion
    """
    # Sort to group duplicate elements together
    numbers.sort()
    all_subsets = []

    def backtrack(start_index: int, current_subset: List[int]):
        """Build subsets while skipping duplicates."""
        # Add current subset to results
        all_subsets.append(current_subset[:])

        for element_index in range(start_index, len(numbers)):
            # Skip duplicates at same recursion level
            # element_index > start_index means it's not the first element at this level
            # numbers[element_index] == numbers[element_index - 1] means it's a duplicate
            if element_index > start_index and numbers[element_index] == numbers[element_index - 1]:
                continue  # Skip this duplicate to avoid duplicate subsets

            # Include current number
            current_subset.append(numbers[element_index])
            backtrack(element_index + 1, current_subset)
            # Backtrack
            current_subset.pop()

    backtrack(0, [])
    return all_subsets


# ============================================================================
# PATTERN 2: PERMUTATIONS
# ============================================================================

def permutations(numbers: List[int]) -> List[List[int]]:
    """
    Generate all possible permutations of an array.

    Example: numbers = [1, 2, 3]
             Permutations: [1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]
             Total: 3! = 6 permutations

    Approach:
    - At each position, try all unused numbers
    - Track which numbers have been used
    - When all positions filled, we have a complete permutation

    Args:
        numbers: List of integers (no duplicates)

    Returns:
        List of all permutations

    Time Complexity: O(n! * n) - n! permutations, each takes O(n) to build
    Space Complexity: O(n) for recursion depth and used array
    """
    all_permutations = []
    is_used = [False] * len(numbers)  # Track which numbers are used

    def backtrack(current_permutation: List[int]):
        """
        Build permutations by trying each unused number.

        Args:
            current_permutation: Permutation built so far
        """
        # Base case: permutation is complete (all positions filled)
        if len(current_permutation) == len(numbers):
            all_permutations.append(current_permutation[:])
            return

        # Try each number in the array
        for number_index in range(len(numbers)):
            # Skip if this number is already used in current permutation
            if is_used[number_index]:
                continue

            # Choice: Use numbers[number_index] at current position
            current_permutation.append(numbers[number_index])
            is_used[number_index] = True

            # Explore: Recurse to fill next position
            backtrack(current_permutation)

            # Undo choice (backtrack) to try other numbers
            current_permutation.pop()
            is_used[number_index] = False

    backtrack([])
    return all_permutations


def permutations_with_duplicates(numbers: List[int]) -> List[List[int]]:
    """
    Generate unique permutations when array contains duplicate elements.

    Example: numbers = [1, 1, 2]
             Unique permutations: [1,1,2], [1,2,1], [2,1,1]
             Total: 3 (not 6, because two 1s are identical)

    Key Strategy:
    1. Sort array to group duplicates
    2. Skip duplicate if:
       - Current number == previous number AND
       - Previous number not yet used
    3. This ensures duplicates are used in sorted order only

    Args:
        numbers: List of integers (may contain duplicates)

    Returns:
        List of all unique permutations

    Time Complexity: O(n! * n)
    Space Complexity: O(n)
    """
    # Sort to group duplicates together
    numbers.sort()
    all_permutations = []
    is_used = [False] * len(numbers)

    def backtrack(current_permutation: List[int]):
        """Build unique permutations while skipping duplicates."""
        # Base case: permutation complete
        if len(current_permutation) == len(numbers):
            all_permutations.append(current_permutation[:])
            return

        for number_index in range(len(numbers)):
            # Skip if already used
            if is_used[number_index]:
                continue

            # Skip duplicate: if current == previous and previous not used
            # This ensures we only use duplicates in order (left to right)
            if (number_index > 0 and
                numbers[number_index] == numbers[number_index - 1] and
                not is_used[number_index - 1]):
                continue

            # Choose current number
            current_permutation.append(numbers[number_index])
            is_used[number_index] = True

            # Explore
            backtrack(current_permutation)

            # Backtrack
            current_permutation.pop()
            is_used[number_index] = False

    backtrack([])
    return all_permutations


# ============================================================================
# PATTERN 3: COMBINATIONS
# ============================================================================

def combinations(max_number: int, combination_size: int) -> List[List[int]]:
    """
    Generate all combinations of combination_size numbers from 1 to max_number.

    Example: max_number=4, combination_size=2
             Combinations: [1,2], [1,3], [1,4], [2,3], [2,4], [3,4]
             Total: C(4,2) = 6 combinations

    Note: Combination vs Permutation:
    - Combination: {1,2} == {2,1} (order doesn't matter)
    - Permutation: [1,2] != [2,1] (order matters)

    Args:
        max_number: Maximum number to consider (inclusive)
        combination_size: Size of each combination

    Returns:
        List of all combinations

    Time Complexity: O(C(n,k) * k) where C(n,k) is binomial coefficient
    Space Complexity: O(k) for recursion depth
    """
    all_combinations = []

    def backtrack(start_number: int, current_combination: List[int]):
        """
        Build combinations starting from start_number.

        Args:
            start_number: Next number to consider
            current_combination: Combination built so far
        """
        # Base case: combination is complete
        if len(current_combination) == combination_size:
            all_combinations.append(current_combination[:])
            return

        # Pruning optimization: Check if enough numbers remain
        numbers_still_needed = combination_size - len(current_combination)
        numbers_available = max_number - start_number + 1

        if numbers_available < numbers_still_needed:
            return  # Not enough numbers left, no point continuing

        # Try each number from start_number to max_number
        for number in range(start_number, max_number + 1):
            # Choose this number
            current_combination.append(number)

            # Recurse with number + 1 (ensures no duplicates and maintains order)
            backtrack(number + 1, current_combination)

            # Backtrack
            current_combination.pop()

    backtrack(1, [])
    return all_combinations


def combination_sum(candidate_numbers: List[int], target_sum: int) -> List[List[int]]:
    """
    Find all combinations of numbers that sum to target.
    Numbers can be reused unlimited times.

    Example: candidates = [2,3,6,7], target = 7
             Combinations: [2,2,3], [7]

    Args:
        candidate_numbers: Numbers we can use (unlimited times each)
        target_sum: Target sum we want to achieve

    Returns:
        List of all combinations that sum to target

    Time Complexity: O(2^target_sum) - worst case with small numbers
    Space Complexity: O(target_sum / min(candidates)) - max recursion depth
    """
    all_combinations = []
    candidate_numbers.sort()  # Optional: helps with pruning

    def backtrack(start_index: int, current_combination: List[int], current_sum: int):
        """
        Build combinations that sum to target.

        Args:
            start_index: Index to start from (for avoiding duplicates)
            current_combination: Numbers chosen so far
            current_sum: Sum of current combination
        """
        # Base case: found a valid combination
        if current_sum == target_sum:
            all_combinations.append(current_combination[:])
            return

        # Pruning: if sum exceeds target, no point continuing
        if current_sum > target_sum:
            return

        # Try each candidate starting from start_index
        for candidate_index in range(start_index, len(candidate_numbers)):
            number = candidate_numbers[candidate_index]

            # Choose this number
            current_combination.append(number)

            # Recurse with same index (allows reuse of same number)
            # This is the key difference from regular combinations
            backtrack(candidate_index, current_combination, current_sum + number)

            # Backtrack
            current_combination.pop()

    backtrack(0, [], 0)
    return all_combinations


def combination_sum_unique(candidate_numbers: List[int], target_sum: int) -> List[List[int]]:
    """
    Find all unique combinations that sum to target.
    Each number can only be used once. Array may contain duplicates.

    Example: candidates = [10,1,2,7,6,1,5], target = 8
             Unique combinations: [1,1,6], [1,2,5], [1,7], [2,6]

    Key: Skip duplicate numbers at same recursion level.

    Args:
        candidate_numbers: Numbers available (each used at most once)
        target_sum: Target sum to achieve

    Returns:
        List of all unique combinations that sum to target

    Time Complexity: O(2^n)
    Space Complexity: O(n)
    """
    all_combinations = []
    candidate_numbers.sort()  # Sort to group duplicates

    def backtrack(start_index: int, current_combination: List[int], current_sum: int):
        """Build unique combinations."""
        # Base case: found valid combination
        if current_sum == target_sum:
            all_combinations.append(current_combination[:])
            return

        # Pruning
        if current_sum > target_sum:
            return

        for candidate_index in range(start_index, len(candidate_numbers)):
            # Skip duplicates at same recursion level
            if candidate_index > start_index and candidate_numbers[candidate_index] == candidate_numbers[candidate_index - 1]:
                continue

            number = candidate_numbers[candidate_index]

            # Choose
            current_combination.append(number)

            # Recurse with next index (each number used at most once)
            backtrack(candidate_index + 1, current_combination, current_sum + number)

            # Backtrack
            current_combination.pop()

    backtrack(0, [], 0)
    return all_combinations


# ============================================================================
# PATTERN 4: LETTER COMBINATIONS
# ============================================================================

def letter_combinations(digit_string: str) -> List[str]:
    """
    Generate all letter combinations from phone number digits.

    Example: digits = "23"
             Phone: 2->abc, 3->def
             Combinations: "ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"

    Args:
        digit_string: String of digits (2-9)

    Returns:
        List of all possible letter combinations

    Time Complexity: O(4^n) worst case (digit 7 and 9 have 4 letters)
    Space Complexity: O(n) for recursion depth
    """
    if not digit_string:
        return []

    # Phone keypad mapping
    digit_to_letters = {
        '2': 'abc', '3': 'def', '4': 'ghi', '5': 'jkl',
        '6': 'mno', '7': 'pqrs', '8': 'tuv', '9': 'wxyz'
    }

    all_combinations = []

    def backtrack(digit_index: int, current_combination: str):
        """
        Build letter combinations digit by digit.

        Args:
            digit_index: Current position in digit string
            current_combination: Letters chosen so far
        """
        # Base case: processed all digits
        if digit_index == len(digit_string):
            all_combinations.append(current_combination)
            return

        # Get letters for current digit
        current_digit = digit_string[digit_index]
        possible_letters = digit_to_letters[current_digit]

        # Try each possible letter for this digit
        for letter in possible_letters:
            # No explicit backtracking needed since strings are immutable
            # We just pass current_combination + letter to next recursion
            backtrack(digit_index + 1, current_combination + letter)

    backtrack(0, "")
    return all_combinations


# ============================================================================
# PATTERN 5: PARTITION PROBLEMS
# ============================================================================

def partition_palindromes(string: str) -> List[List[str]]:
    """
    Partition string into all possible palindromic substrings.

    Example: string = "aab"
             Partitions: ["a", "a", "b"] and ["aa", "b"]
             Both partitions have all palindromes

    Strategy:
    - Try all possible positions to partition
    - Only partition if substring is palindrome
    - Recursively partition remaining string

    Args:
        string: Input string to partition

    Returns:
        List of all valid palindromic partitions

    Time Complexity: O(n * 2^n) - 2^n partitions, each checks palindrome in O(n)
    Space Complexity: O(n) for recursion depth
    """
    all_partitions = []

    def is_palindrome(substring: str) -> bool:
        """Check if string reads same forwards and backwards."""
        return substring == substring[::-1]

    def backtrack(start_index: int, current_partition: List[str]):
        """
        Build palindromic partitions.

        Args:
            start_index: Position in string to start partitioning from
            current_partition: Palindromic substrings chosen so far
        """
        # Base case: reached end of string (valid complete partition)
        if start_index == len(string):
            all_partitions.append(current_partition[:])
            return

        # Try all possible end positions for next palindrome
        for end_index in range(start_index + 1, len(string) + 1):
            substring = string[start_index:end_index]

            # Only proceed if substring is palindrome
            if is_palindrome(substring):
                # Choose this palindromic substring
                current_partition.append(substring)

                # Recurse on remaining string
                backtrack(end_index, current_partition)

                # Backtrack
                current_partition.pop()

    backtrack(0, [])
    return all_partitions


# ============================================================================
# PATTERN 6: N-QUEENS PROBLEM
# ============================================================================

def solve_n_queens(board_size: int) -> List[List[str]]:
    """
    Place board_size queens on board_size×board_size board so no two queens attack each other.

    Rules:
    - Queens attack horizontally, vertically, and diagonally
    - No two queens can be in same row, column, or diagonal

    Example: board_size = 4
             Two solutions exist (see test output)

    Strategy:
    - Place one queen per row (ensures different rows)
    - Track occupied columns and diagonals
    - For each row, try each column that's safe

    Args:
        board_size: Size of chess board and number of queens

    Returns:
        List of all valid board configurations

    Time Complexity: O(n!) - try different positions per row
    Space Complexity: O(n) for recursion and state tracking
    """
    all_solutions = []
    board = [['.'] * board_size for _ in range(board_size)]

    # Track attacked positions using sets for O(1) lookup
    occupied_columns: Set[int] = set()
    occupied_diagonal_1: Set[int] = set()  # Diagonal: row - column
    occupied_diagonal_2: Set[int] = set()  # Diagonal: row + column

    def backtrack(row_index: int):
        """
        Place queens row by row.

        Args:
            row_index: Current row to place queen in
        """
        # Base case: placed queen in all rows (found solution)
        if row_index == board_size:
            # Convert board to required string format
            solution = [''.join(row) for row in board]
            all_solutions.append(solution)
            return

        # Try placing queen in each column of current row
        for column_index in range(board_size):
            # Check if this position is under attack
            diagonal_1_value = row_index - column_index
            diagonal_2_value = row_index + column_index

            if (column_index in occupied_columns or
                diagonal_1_value in occupied_diagonal_1 or
                diagonal_2_value in occupied_diagonal_2):
                continue  # Position under attack, skip

            # Place queen (this position is safe)
            board[row_index][column_index] = 'Q'
            occupied_columns.add(column_index)
            occupied_diagonal_1.add(diagonal_1_value)
            occupied_diagonal_2.add(diagonal_2_value)

            # Recurse to next row
            backtrack(row_index + 1)

            # Remove queen (backtrack)
            board[row_index][column_index] = '.'
            occupied_columns.remove(column_index)
            occupied_diagonal_1.remove(diagonal_1_value)
            occupied_diagonal_2.remove(diagonal_2_value)

    backtrack(0)
    return all_solutions


# ============================================================================
# BACKTRACKING TEMPLATE
# ============================================================================

def backtracking_template():
    """
    UNIVERSAL BACKTRACKING TEMPLATE - Use this as a starting point

    Step 1: Define result container
    result = []

    Step 2: Define backtracking function
    def backtrack(current_state, available_choices):
        # Base case: reached goal
        if is_complete(current_state):
            result.append(current_state[:])  # IMPORTANT: Make a copy!
            return

        # Try each possible choice
        for choice in available_choices:
            # Skip invalid choices (pruning)
            if not is_valid(choice):
                continue

            # Make choice (modify state)
            current_state.append(choice)

            # Explore with this choice
            backtrack(current_state, updated_choices)

            # Undo choice (backtrack)
            current_state.pop()

    Step 3: Start backtracking
    backtrack(initial_state, initial_choices)

    Step 4: Return results
    return result

    KEY PRINCIPLES:
    1. Always copy path when adding to result: path[:]
    2. Make choice → Explore → Undo choice (backtrack)
    3. Use pruning to skip invalid choices early
    4. For duplicates: sort first, skip at same recursion level
    5. Track state efficiently (use sets for O(1) operations)
    6. Base case: when to add current state to results
    7. Recursive case: try all valid next choices

    COMMON PATTERNS:
    - Subsets: Include/exclude each element
    - Permutations: Try each unused element
    - Combinations: Try elements from start_index onwards
    - Partitions: Try all split points
    """
    pass


# ============================================================================
# TEST CASES WITH DETAILED EXPLANATIONS
# ============================================================================

def run_tests():
    """Run comprehensive tests for all subset/backtracking patterns."""
    print("=" * 80)
    print("SUBSET PATTERN (BACKTRACKING) - COMPREHENSIVE TEST CASES")
    print("=" * 80)

    # Test 1: Subsets
    print("\n" + "=" * 80)
    print("TEST 1: Subsets (Power Set)")
    print("=" * 80)
    test_numbers_1 = [1, 2, 3]
    print(f"Input: {test_numbers_1}")
    result_1 = subsets(test_numbers_1)
    print(f"Output: {result_1}")
    print(f"Count: {len(result_1)} subsets (expected: 2^3 = 8)")
    print(f"Expected: [[], [1], [2], [1,2], [3], [1,3], [2,3], [1,2,3]]")

    # Test 2: Subsets with Duplicates
    print("\n" + "=" * 80)
    print("TEST 2: Subsets with Duplicates")
    print("=" * 80)
    test_numbers_2 = [1, 2, 2]
    print(f"Input: {test_numbers_2}")
    result_2 = subsets_with_duplicates(test_numbers_2)
    print(f"Output: {result_2}")
    print(f"Explanation: Two 2s, but subsets are unique")
    print(f"Expected: [[], [1], [1,2], [1,2,2], [2], [2,2]]")

    # Test 3: Permutations
    print("\n" + "=" * 80)
    print("TEST 3: Permutations")
    print("=" * 80)
    test_numbers_3 = [1, 2, 3]
    print(f"Input: {test_numbers_3}")
    result_3 = permutations(test_numbers_3)
    print(f"Output: {result_3}")
    print(f"Count: {len(result_3)} permutations (expected: 3! = 6)")

    # Test 4: Combinations
    print("\n" + "=" * 80)
    print("TEST 4: Combinations C(4, 2)")
    print("=" * 80)
    print(f"Input: max_number=4, combination_size=2")
    result_4 = combinations(4, 2)
    print(f"Output: {result_4}")
    print(f"Count: {len(result_4)} combinations (expected: C(4,2) = 6)")
    print(f"Expected: [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]")

    # Test 5: Combination Sum
    print("\n" + "=" * 80)
    print("TEST 5: Combination Sum (with reuse)")
    print("=" * 80)
    test_candidates_5 = [2, 3, 6, 7]
    target_5 = 7
    print(f"Input: candidates={test_candidates_5}, target={target_5}")
    result_5 = combination_sum(test_candidates_5, target_5)
    print(f"Output: {result_5}")
    print(f"Explanation: Can reuse numbers, so [2,2,3] is valid")
    print(f"Expected: [[2,2,3], [7]]")

    # Test 6: Letter Combinations
    print("\n" + "=" * 80)
    print("TEST 6: Letter Combinations of Phone Number")
    print("=" * 80)
    test_digits_6 = "23"
    print(f"Input: '{test_digits_6}'")
    print(f"Phone mapping: 2→abc, 3→def")
    result_6 = letter_combinations(test_digits_6)
    print(f"Output: {result_6}")
    print(f"Count: {len(result_6)} combinations (expected: 3×3 = 9)")
    print(f"Expected: ['ad','ae','af','bd','be','bf','cd','ce','cf']")

    # Test 7: Palindrome Partitioning
    print("\n" + "=" * 80)
    print("TEST 7: Palindrome Partitioning")
    print("=" * 80)
    test_string_7 = "aab"
    print(f"Input: '{test_string_7}'")
    result_7 = partition_palindromes(test_string_7)
    print(f"Output: {result_7}")
    print(f"Explanation: 'aa' is palindrome, 'a','a','b' all palindromes")
    print(f"Expected: [['a','a','b'], ['aa','b']]")

    # Test 8: N-Queens
    print("\n" + "=" * 80)
    print("TEST 8: N-Queens Problem (board_size=4)")
    print("=" * 80)
    result_8 = solve_n_queens(4)
    print(f"Input: board_size=4")
    print(f"Number of solutions: {len(result_8)}")
    print(f"First solution:")
    for row in result_8[0]:
        print(f"   {row}")
    print(f"Explanation: Queens placed so none attack each other")

    # Print Key Insights
    print("\n" + "=" * 80)
    print("KEY INSIGHTS AND PATTERNS")
    print("=" * 80)
    print("1. BACKTRACKING PATTERN:")
    print("   Make choice → Explore → Undo choice (backtrack)")
    print()
    print("2. CRITICAL RULE:")
    print("   Always copy path when adding to result: current_path[:]")
    print("   Without copy, all results point to same modified list!")
    print()
    print("3. HANDLING DUPLICATES:")
    print("   - Sort array first to group duplicates")
    print("   - Skip duplicates at same recursion level")
    print("   - Check: if index > start and nums[index] == nums[index-1]")
    print()
    print("4. STATE TRACKING:")
    print("   - Use sets for O(1) lookups (columns, diagonals)")
    print("   - Use boolean arrays for visited/used tracking")
    print("   - Pass index to avoid revisiting earlier elements")
    print()
    print("5. OPTIMIZATION (PRUNING):")
    print("   - Check validity before recursing (not after)")
    print("   - Early termination when goal impossible")
    print("   - Skip branches that can't lead to solution")
    print()
    print("6. TIME COMPLEXITY:")
    print("   - Subsets: O(2^n) - binary choice per element")
    print("   - Permutations: O(n!) - factorial choices")
    print("   - Combinations: O(C(n,k)) - binomial coefficient")
    print("=" * 80)


if __name__ == "__main__":
    run_tests()

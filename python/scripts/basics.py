from collections import Counter, defaultdict, deque

def sum_list(numbers):
    """Return the sum of all numbers in the list."""
    if not numbers:
        return 0

    total = 0
    for number in numbers:
        total += number

    return total


def find_max(numbers):
    """Return the largest number in the list, or None if list is empty."""
    if not numbers:
        return None

    max_number = numbers[0]
    for number in numbers:
        if number > max_number:
            max_number = number

    return max_number


def count_vowels(text):
    """Count and return the number of vowels in a given string."""
    if not text:
        return 0

    vowels = "aeiou"
    count = 0

    for letter in text.lower():
        if letter in vowels:
            count += 1

    return count


def remove_duplicates(numbers):
    """Return a list of numbers with duplicates removed, preserving order."""
    seen = set()
    unique = []

    for number in numbers:
        if number not in seen:
            seen.add(number)
            unique.append(number)

    return unique


def find_missing_number(numbers):
    """
    Find the missing number in a sequence from 0 to n.
    Assumes exactly one number is missing.
    """
    n = len(numbers)
    expected_sum = n * (n + 1) // 2
    actual_sum = sum(numbers)
    return expected_sum - actual_sum


def merge_sorted_lists(list1, list2):
    """
    Merge two sorted lists into a single sorted list.
    """
    i, j = 0, 0
    merged = []

    while i < len(list1) and j < len(list2):
        if list1[i] <= list2[j]:
            merged.append(list1[i])
            i += 1
        else:
            merged.append(list2[j])
            j += 1

    # Append any remaining elements
    merged.extend(list1[i:])
    merged.extend(list2[j:])
    return merged


def most_frequent(numbers):
    """Return the most frequently occurring number in the list."""
    if not numbers:
        return ""

    frequency = Counter(numbers)
    most_common = frequency.most_common(1)
    return most_common[0][0] if most_common else None


def longest_common_prefix(strings):
    """
    Find the longest common prefix string among a list of strings.
    """
    if not strings:
        return ""

    prefix = strings[0]
    for s in strings[1:]:
        while not s.startswith(prefix):
            prefix = prefix[:-1]
            if not prefix:
                return ""
    return prefix


def product_except_self(numbers):
    """
    Return an array where each element is the product of all other elements.
    Does not use division. O(n) time, O(1) extra space.
    """
    n = len(numbers)
    if n == 0:
        return []

    prefix = [1] * n

    # Compute prefix products
    for i in range(1, n):
        prefix[i] = prefix[i - 1] * numbers[i - 1]

    # Compute suffix products and multiply in place
    suffix = 1
    for i in range(n - 1, -1, -1):
        prefix[i] *= suffix
        suffix *= numbers[i]

    return prefix


def word_pattern(pattern, text):
    """
    Check if a pattern matches a string of words bijectively.
    Example: pattern = "abba", text = "dog cat cat dog" -> True
    """
    words = text.split()
    if len(pattern) != len(words):
        return False

    char_to_word = {}
    word_to_char = {}

    for c, w in zip(pattern, words):
        # Check for mismatches in existing mappings
        if c in char_to_word and char_to_word[c] != w:
            return False
        if w in word_to_char and word_to_char[w] != c:
            return False

        # Establish mapping both ways
        char_to_word[c] = w
        word_to_char[w] = c

    return True


def binary_search(sorted_list, target):
    """
    Perform binary search on a sorted list.
    Return index of target or -1 if not found.
    """
    left, right = 0, len(sorted_list) - 1

    while left <= right:
        mid = (left + right) // 2
        value = sorted_list[mid]

        if value == target:
            return mid
        elif value < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1


def reverse_words_in_string(sentence):
    """Reverse the order of words in a sentence."""
    return " ".join(reversed(sentence.split())).strip()


def valid_anagram(word, anagram):
    """Check if two strings are anagrams of each other."""
    if len(word) != len(anagram):
        return False
    return sorted(word) == sorted(anagram)


def longest_palindrome_substring(letters):
    """
    Return the length of the longest palindrome that can be built
    from the given letters.
    """
    counts = Counter(letters)
    length = 0
    odd_found = False

    for count in counts.values():
        if count % 2 == 0:
            length += count
        else:
            length += count - 1
            odd_found = True

    # Add one odd character to the center if available
    if odd_found:
        length += 1

    return length


def rotate_array(numbers, steps):
    """Rotate an array to the right by 'steps' positions."""
    n = len(numbers)
    if n == 0:
        return None

    steps %= n  # handle k > n

    def reverse(start, end):
        """Helper function to reverse part of the list in-place."""
        while start < end:
            numbers[start], numbers[end] = numbers[end], numbers[start]
            start += 1
            end -= 1

    # Reverse entire list
    reverse(0, n - 1)
    # Reverse first k elements
    reverse(0, steps - 1)
    # Reverse remaining elements
    reverse(steps, n - 1)

    return numbers


def find_duplicate(numbers):
    """Find a duplicate number in the list, if any."""
    numbers.sort()
    for i in range(1, len(numbers)):
        if numbers[i] == numbers[i - 1]:
            return numbers[i]
    return None


def container_with_most_water(height):
    """Return the maximum water area between two lines."""
    left, right = 0, len(height) - 1
    max_area = 0

    while left < right:
        width = right - left
        area = min(height[left], height[right]) * width
        max_area = max(max_area, area)

        # Move pointer of smaller line inward
        if height[left] < height[right]:
            left += 1
        else:
            right -= 1

    return max_area


def house_robber(nums):
    """Return the maximum sum of non-adjacent house values."""
    if not nums:
        return 0
    if len(nums) == 1:
        return nums[0]

    dp = [0] * len(nums)
    dp[0] = nums[0]
    dp[1] = max(nums[0], nums[1])

    for i in range(2, len(nums)):
        dp[i] = max(nums[i] + dp[i - 2], dp[i - 1])

    return dp[-1]


def coin_change(coins, amount):
    """Return fewest coins needed to make 'amount' using given 'coins'."""
    dp = [amount + 1] * (amount + 1)
    dp[0] = 0

    for a in range(1, amount + 1):
        for c in coins:
            if a - c >= 0:
                dp[a] = min(dp[a], 1 + dp[a - c])

    return dp[amount] if dp[amount] != amount + 1 else -1


class TreeNode:
    """Binary tree node definition."""

    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right


def symmetric_tree(root):
    """Check if a binary tree is symmetric (mirror of itself)."""
    if not root:
        return True

    def is_mirror(t1, t2):
        if not t1 and not t2:
            return True
        if not t1 or not t2:
            return False
        return (
                t1.val == t2.val
                and is_mirror(t1.left, t2.right)
                and is_mirror(t1.right, t2.left)
        )

    return is_mirror(root.left, root.right)


def path_sum(root, target_sum):
    """Return True if a root-to-leaf path sums to target_sum."""
    if not root:
        return False

    target_sum -= root.val

    # If at a leaf, check if sum matches
    if not root.left and not root.right:
        return target_sum == 0

    return path_sum(root.left, target_sum) or path_sum(root.right, target_sum)


def single_number(nums):
    """Find the element that appears only once using XOR."""
    result = 0
    for num in nums:
        result ^= num
    return result


def happy_number(n):
    """Determine if a number is happy (sum of squares eventually reaches 1)."""
    seen = set()
    while n != 1:
        if n in seen:
            return False
        seen.add(n)
        n = sum(int(digit) ** 2 for digit in str(n))
    return True


def max_consecutive_ones(nums, k):
    """Find the longest sequence of 1's after flipping at most k zeros."""
    left = 0
    zero_count = 0
    max_len = 0

    for right in range(len(nums)):
        if nums[right] == 0:
            zero_count += 1

        # Shrink window if too many zeros
        while zero_count > k:
            if nums[left] == 0:
                zero_count -= 1
            left += 1

        # Track maximum valid window size
        max_len = max(max_len, right - left + 1)

    return max_len

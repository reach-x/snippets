"""
Python Interview Questions: Coding Challenges
==============================================
Topics: Common interview coding problems with solutions
Greppable format for quick reference
"""

# ============================================================================
# STRING PROBLEMS
# ============================================================================

# Q: How do you reverse a string?
# A: Use slicing
def reverse_string(s):
    return s[::-1]
# Example: reverse_string("hello") -> "olleh"

# Q: How do you check if a string is a palindrome?
# A: Compare with reversed version or two pointers
def is_palindrome(s):
    # Method 1: Simple comparison
    return s == s[::-1]

    # Method 2: Two pointers (more efficient for large strings)
    # left, right = 0, len(s) - 1
    # while left < right:
    #     if s[left] != s[right]:
    #         return False
    #     left += 1
    #     right -= 1
    # return True

# Q: How do you check if string is palindrome ignoring spaces and case?
# A: Clean string first, then check
def is_palindrome_ignore_case_space(s):
    cleaned = ''.join(s.lower().split())
    return cleaned == cleaned[::-1]
# Example: is_palindrome_ignore_case_space("A man a plan a canal Panama") -> True

# Q: How do you find first non-repeating character?
# A: Use Counter to count occurrences
from collections import Counter

def first_non_repeating_char(s):
    counts = Counter(s)
    for char in s:
        if counts[char] == 1:
            return char
    return None
# Example: first_non_repeating_char("aabbcdee") -> "c"

# Q: How do you check if two strings are anagrams?
# A: Sort both and compare, or use Counter
def are_anagrams(s1, s2):
    # Method 1: Sorting
    return sorted(s1) == sorted(s2)

    # Method 2: Counter (better for large strings)
    # return Counter(s1) == Counter(s2)

# Example: are_anagrams("listen", "silent") -> True

# Q: How do you find all permutations of a string?
# A: Use itertools.permutations or recursion
from itertools import permutations

def string_permutations(s):
    return [''.join(p) for p in permutations(s)]

# Recursive approach
def permutations_recursive(s):
    if len(s) <= 1:
        return [s]
    perms = []
    for i, char in enumerate(s):
        remaining = s[:i] + s[i+1:]
        for perm in permutations_recursive(remaining):
            perms.append(char + perm)
    return perms

# Q: How do you count vowels in a string?
# A: Iterate and check membership in vowels set
def count_vowels(s):
    vowels = set('aeiouAEIOU')
    return sum(1 for char in s if char in vowels)
# Example: count_vowels("hello world") -> 3

# Q: How do you find longest substring without repeating characters?
# A: Use sliding window with set
def longest_unique_substring(s):
    char_set = set()
    left = 0
    max_length = 0
    max_substring = ""

    for right in range(len(s)):
        while s[right] in char_set:
            char_set.remove(s[left])
            left += 1
        char_set.add(s[right])
        if right - left + 1 > max_length:
            max_length = right - left + 1
            max_substring = s[left:right+1]

    return max_substring
# Example: longest_unique_substring("abcabcbb") -> "abc"

# Q: How do you compress a string (e.g., "aaabbcccc" -> "a3b2c4")?
# A: Count consecutive characters
def compress_string(s):
    if not s:
        return ""

    result = []
    current_char = s[0]
    count = 1

    for i in range(1, len(s)):
        if s[i] == current_char:
            count += 1
        else:
            result.append(current_char + str(count))
            current_char = s[i]
            count = 1

    result.append(current_char + str(count))
    compressed = ''.join(result)

    return compressed if len(compressed) < len(s) else s
# Example: compress_string("aaabbcccc") -> "a3b2c4"

# ============================================================================
# ARRAY/LIST PROBLEMS
# ============================================================================

# Q: How do you find two numbers that sum to target?
# A: Use hash map for O(n) solution
def two_sum(numbers, target):
    seen = {}
    for i, num in enumerate(numbers):
        complement = target - num
        if complement in seen:
            return [seen[complement], i]
        seen[num] = i
    return None
# Example: two_sum([2, 7, 11, 15], 9) -> [0, 1]

# Q: How do you find maximum subarray sum (Kadane's algorithm)?
# A: Track current and maximum sum
def max_subarray_sum(arr):
    if not arr:
        return 0

    current_sum = max_sum = arr[0]

    for num in arr[1:]:
        current_sum = max(num, current_sum + num)
        max_sum = max(max_sum, current_sum)

    return max_sum
# Example: max_subarray_sum([-2, 1, -3, 4, -1, 2, 1, -5, 4]) -> 6

# Q: How do you rotate array by k positions?
# A: Reverse subarrays
def rotate_array(arr, k):
    k = k % len(arr)
    arr[:] = arr[-k:] + arr[:-k]
    return arr
# Example: rotate_array([1, 2, 3, 4, 5], 2) -> [4, 5, 1, 2, 3]

# Q: How do you find missing number in array 1 to n?
# A: Use sum formula or XOR
def find_missing_number(arr, n):
    expected_sum = n * (n + 1) // 2
    actual_sum = sum(arr)
    return expected_sum - actual_sum
# Example: find_missing_number([1, 2, 4, 5], 5) -> 3

# Q: How do you find duplicate number in array?
# A: Use set to track seen numbers
def find_duplicate(arr):
    seen = set()
    for num in arr:
        if num in seen:
            return num
        seen.add(num)
    return None
# Example: find_duplicate([1, 2, 3, 2, 4]) -> 2

# Q: How do you merge two sorted arrays?
# A: Use two pointers
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
# Example: merge_sorted_arrays([1, 3, 5], [2, 4, 6]) -> [1, 2, 3, 4, 5, 6]

# Q: How do you find intersection of two arrays?
# A: Use sets for O(n) time
def array_intersection(arr1, arr2):
    return list(set(arr1) & set(arr2))
# Example: array_intersection([1, 2, 3, 4], [3, 4, 5, 6]) -> [3, 4]

# Q: How do you find all pairs with given difference?
# A: Use hash set
def find_pairs_with_difference(arr, k):
    pairs = []
    num_set = set(arr)

    for num in arr:
        if num + k in num_set:
            pairs.append((num, num + k))

    return list(set(pairs))
# Example: find_pairs_with_difference([1, 5, 3, 4, 2], 2) -> [(1, 3), (3, 5), (2, 4)]

# Q: How do you move all zeros to end of array?
# A: Two pointer approach
def move_zeros_to_end(arr):
    non_zero_pos = 0

    for i in range(len(arr)):
        if arr[i] != 0:
            arr[non_zero_pos], arr[i] = arr[i], arr[non_zero_pos]
            non_zero_pos += 1

    return arr
# Example: move_zeros_to_end([0, 1, 0, 3, 12]) -> [1, 3, 12, 0, 0]

# ============================================================================
# LINKED LIST PROBLEMS (Conceptual - using classes)
# ============================================================================

class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

# Q: How do you reverse a linked list?
# A: Three pointers: prev, current, next
def reverse_linked_list(head):
    prev = None
    current = head

    while current:
        next_node = current.next
        current.next = prev
        prev = current
        current = next_node

    return prev

# Q: How do you detect cycle in linked list?
# A: Floyd's cycle detection (slow and fast pointers)
def has_cycle(head):
    if not head or not head.next:
        return False

    slow = fast = head

    while fast and fast.next:
        slow = slow.next
        fast = fast.next.next
        if slow == fast:
            return True

    return False

# Q: How do you find middle of linked list?
# A: Slow and fast pointers
def find_middle(head):
    if not head:
        return None

    slow = fast = head

    while fast and fast.next:
        slow = slow.next
        fast = fast.next.next

    return slow

# Q: How do you merge two sorted linked lists?
# A: Compare values and build new list
def merge_sorted_lists(l1, l2):
    dummy = ListNode(0)
    current = dummy

    while l1 and l2:
        if l1.val <= l2.val:
            current.next = l1
            l1 = l1.next
        else:
            current.next = l2
            l2 = l2.next
        current = current.next

    current.next = l1 or l2
    return dummy.next

# ============================================================================
# TREE PROBLEMS (Conceptual)
# ============================================================================

class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

# Q: How do you traverse tree in-order?
# A: Left, Root, Right (recursive)
def inorder_traversal(root):
    if not root:
        return []
    return inorder_traversal(root.left) + [root.val] + inorder_traversal(root.right)

# Q: How do you traverse tree pre-order?
# A: Root, Left, Right
def preorder_traversal(root):
    if not root:
        return []
    return [root.val] + preorder_traversal(root.left) + preorder_traversal(root.right)

# Q: How do you traverse tree post-order?
# A: Left, Right, Root
def postorder_traversal(root):
    if not root:
        return []
    return postorder_traversal(root.left) + postorder_traversal(root.right) + [root.val]

# Q: How do you find maximum depth of tree?
# A: Recursively find max of left and right depths
def max_depth(root):
    if not root:
        return 0
    return 1 + max(max_depth(root.left), max_depth(root.right))

# Q: How do you check if tree is balanced?
# A: Check if height difference between left and right <= 1
def is_balanced(root):
    def get_height(node):
        if not node:
            return 0
        left_height = get_height(node.left)
        if left_height == -1:
            return -1
        right_height = get_height(node.right)
        if right_height == -1:
            return -1

        if abs(left_height - right_height) > 1:
            return -1

        return 1 + max(left_height, right_height)

    return get_height(root) != -1

# Q: How do you perform level-order traversal (BFS)?
# A: Use queue
from collections import deque

def level_order_traversal(root):
    if not root:
        return []

    result = []
    queue = deque([root])

    while queue:
        level_size = len(queue)
        level = []

        for _ in range(level_size):
            node = queue.popleft()
            level.append(node.val)

            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)

        result.append(level)

    return result

# ============================================================================
# MATHEMATICAL PROBLEMS
# ============================================================================

# Q: How do you check if number is prime?
# A: Check divisibility up to sqrt(n)
def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False

    for i in range(3, int(n**0.5) + 1, 2):
        if n % i == 0:
            return False
    return True

# Q: How do you find greatest common divisor (GCD)?
# A: Use Euclidean algorithm
def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

# Q: How do you find least common multiple (LCM)?
# A: Use formula: LCM(a,b) = (a*b) / GCD(a,b)
def lcm(a, b):
    return abs(a * b) // gcd(a, b)

# Q: How do you generate Fibonacci sequence up to n terms?
# A: Iterative approach
def fibonacci_sequence(n):
    if n <= 0:
        return []
    if n == 1:
        return [0]

    fib = [0, 1]
    for i in range(2, n):
        fib.append(fib[i-1] + fib[i-2])

    return fib

# Q: How do you calculate factorial?
# A: Iterative or recursive
def factorial(n):
    if n == 0 or n == 1:
        return 1
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result

# Q: How do you check if number is power of two?
# A: Use bitwise AND
def is_power_of_two(n):
    return n > 0 and (n & (n - 1)) == 0

# Q: How do you reverse digits of integer?
# A: Convert to string or use modulo
def reverse_integer(n):
    # Method 1: String conversion
    is_negative = n < 0
    reversed_num = int(str(abs(n))[::-1])
    return -reversed_num if is_negative else reversed_num

    # Method 2: Mathematical
    # result = 0
    # is_negative = n < 0
    # n = abs(n)
    # while n > 0:
    #     result = result * 10 + n % 10
    #     n //= 10
    # return -result if is_negative else result

# ============================================================================
# DYNAMIC PROGRAMMING PROBLEMS
# ============================================================================

# Q: How do you solve coin change problem (minimum coins)?
# A: Use dynamic programming
def coin_change(coins, amount):
    dp = [float('inf')] * (amount + 1)
    dp[0] = 0

    for i in range(1, amount + 1):
        for coin in coins:
            if coin <= i:
                dp[i] = min(dp[i], dp[i - coin] + 1)

    return dp[amount] if dp[amount] != float('inf') else -1
# Example: coin_change([1, 2, 5], 11) -> 3 (5+5+1)

# Q: How do you solve climbing stairs problem?
# A: Fibonacci-like DP (can take 1 or 2 steps)
def climb_stairs(n):
    if n <= 2:
        return n

    prev, curr = 1, 2
    for _ in range(3, n + 1):
        prev, curr = curr, prev + curr

    return curr
# Example: climb_stairs(5) -> 8 ways

# Q: How do you solve longest common subsequence?
# A: Use 2D DP table
def longest_common_subsequence(text1, text2):
    m, n = len(text1), len(text2)
    dp = [[0] * (n + 1) for _ in range(m + 1)]

    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if text1[i-1] == text2[j-1]:
                dp[i][j] = dp[i-1][j-1] + 1
            else:
                dp[i][j] = max(dp[i-1][j], dp[i][j-1])

    return dp[m][n]
# Example: longest_common_subsequence("abcde", "ace") -> 3

# Q: How do you find longest increasing subsequence?
# A: Use DP array
def longest_increasing_subsequence(arr):
    if not arr:
        return 0

    dp = [1] * len(arr)

    for i in range(1, len(arr)):
        for j in range(i):
            if arr[i] > arr[j]:
                dp[i] = max(dp[i], dp[j] + 1)

    return max(dp)
# Example: longest_increasing_subsequence([10, 9, 2, 5, 3, 7, 101, 18]) -> 4

# ============================================================================
# MISCELLANEOUS PROBLEMS
# ============================================================================

# Q: How do you implement LRU Cache?
# A: Use OrderedDict for O(1) operations
from collections import OrderedDict

class LRUCache:
    def __init__(self, capacity):
        self.cache = OrderedDict()
        self.capacity = capacity

    def get(self, key):
        if key not in self.cache:
            return -1
        self.cache.move_to_end(key)
        return self.cache[key]

    def put(self, key, value):
        if key in self.cache:
            self.cache.move_to_end(key)
        self.cache[key] = value
        if len(self.cache) > self.capacity:
            self.cache.popitem(last=False)

# Q: How do you validate parentheses?
# A: Use stack
def is_valid_parentheses(s):
    stack = []
    mapping = {')': '(', '}': '{', ']': '['}

    for char in s:
        if char in mapping:
            top = stack.pop() if stack else '#'
            if mapping[char] != top:
                return False
        else:
            stack.append(char)

    return not stack
# Example: is_valid_parentheses("()[]{}") -> True

# Q: How do you find kth largest element?
# A: Use heap or sorting
import heapq

def find_kth_largest(nums, k):
    # Method 1: Using heap (more efficient)
    return heapq.nlargest(k, nums)[-1]

    # Method 2: Sorting
    # return sorted(nums, reverse=True)[k-1]

# Q: How do you group anagrams?
# A: Use sorted string as key
def group_anagrams(words):
    from collections import defaultdict
    groups = defaultdict(list)

    for word in words:
        key = ''.join(sorted(word))
        groups[key].append(word)

    return list(groups.values())
# Example: group_anagrams(["eat","tea","tan","ate","nat","bat"])
# -> [["eat","tea","ate"],["tan","nat"],["bat"]]

# Q: How do you implement stack using queues?
# A: Use two queues or one queue with rotation
from collections import deque

class StackUsingQueue:
    def __init__(self):
        self.queue = deque()

    def push(self, x):
        self.queue.append(x)
        for _ in range(len(self.queue) - 1):
            self.queue.append(self.queue.popleft())

    def pop(self):
        return self.queue.popleft()

    def top(self):
        return self.queue[0]

    def empty(self):
        return len(self.queue) == 0

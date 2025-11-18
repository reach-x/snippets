"""
15. Climbing Stairs

You are climbing a staircase. It takes n steps to reach the top.
Each time you can either climb 1 or 2 steps. In how many distinct ways
can you climb to the top?

LeetCode: https://leetcode.com/problems/climbing-stairs/
Difficulty: Easy
Pattern: Dynamic Programming, Fibonacci
"""


def climb_stairs_recursive(n: int) -> int:
    """
    Naive recursive approach (exponential time).

    Time Complexity: O(2^n)
    Space Complexity: O(n) for recursion stack
    """
    if n <= 2:
        return n

    return climb_stairs_recursive(n - 1) + climb_stairs_recursive(n - 2)


def climb_stairs_memoization(n: int) -> int:
    """
    Top-down DP with memoization.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    memo = {}

    def helper(n: int) -> int:
        if n <= 2:
            return n

        if n in memo:
            return memo[n]

        memo[n] = helper(n - 1) + helper(n - 2)
        return memo[n]

    return helper(n)


def climb_stairs_tabulation(n: int) -> int:
    """
    Bottom-up DP with tabulation.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    if n <= 2:
        return n

    dp = [0] * (n + 1)
    dp[1] = 1
    dp[2] = 2

    for i in range(3, n + 1):
        dp[i] = dp[i - 1] + dp[i - 2]

    return dp[n]


def climb_stairs(n: int) -> int:
    """
    Optimal: Space-optimized DP.

    Time Complexity: O(n)
    Space Complexity: O(1)
    """
    if n <= 2:
        return n

    prev2 = 1  # Ways to reach step 1
    prev1 = 2  # Ways to reach step 2

    for i in range(3, n + 1):
        current = prev1 + prev2
        prev2 = prev1
        prev1 = current

    return prev1


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {'n': 2, 'expected': 2, 'description': 'Two steps: 1+1 or 2'},
        {'n': 3, 'expected': 3, 'description': 'Three steps: 1+1+1, 1+2, 2+1'},
        {'n': 4, 'expected': 5, 'description': 'Four steps'},
        {'n': 5, 'expected': 8, 'description': 'Five steps (Fibonacci)'},
        {'n': 1, 'expected': 1, 'description': 'One step'},
        {'n': 10, 'expected': 89, 'description': 'Ten steps'},
    ]

    print("=" * 70)
    print("CLIMBING STAIRS - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        result_memo = climb_stairs_memoization(test['n'])
        result_tab = climb_stairs_tabulation(test['n'])
        result_optimal = climb_stairs(test['n'])

        passed = (result_memo == test['expected'] and
                 result_tab == test['expected'] and
                 result_optimal == test['expected'])

        print(f"\nTest {i}: {test['description']}")
        print(f"n = {test['n']}")
        print(f"Expected: {test['expected']}")
        print(f"Got (Memoization): {result_memo}")
        print(f"Got (Tabulation): {result_tab}")
        print(f"Got (Optimal): {result_optimal}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("PATTERN RECOGNITION")
    print("=" * 70)
    print("This is a Fibonacci sequence!")
    print("\nWays to reach step n:")
    print("  f(n) = f(n-1) + f(n-2)")
    print("\nWhy?")
    print("  - To reach step n, you can come from:")
    print("    1. Step n-1 (take 1 step)")
    print("    2. Step n-2 (take 2 steps)")
    print("\nSequence: 1, 2, 3, 5, 8, 13, 21, 34, 55, 89...")

    print("\n" + "=" * 70)
    print("APPROACH COMPARISON")
    print("=" * 70)
    print("Naive Recursive: Time O(2^n), Space O(n)")
    print("Memoization: Time O(n), Space O(n)")
    print("Tabulation: Time O(n), Space O(n)")
    print("Optimal: Time O(n), Space O(1) - BEST")

    print("\n" + "=" * 70)
    print("OPTIMAL SOLUTION")
    print("=" * 70)
    print("""
def climb_stairs(n):
    if n <= 2:
        return n

    prev2, prev1 = 1, 2

    for i in range(3, n + 1):
        current = prev1 + prev2
        prev2 = prev1
        prev1 = current

    return prev1
""")

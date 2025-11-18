"""
17. 0/1 Knapsack

Given weights and values of n items, put these items in a knapsack of capacity W
to get the maximum total value in the knapsack. In other words, given two integer
arrays val[0..n-1] and wt[0..n-1] which represent values and weights associated
with n items respectively. Also given an integer W which represents knapsack capacity,
find out the maximum value subset of val[] such that sum of the weights of this
subset is smaller than or equal to W. You cannot break an item, either pick the
complete item or don't pick it (0-1 property).

Note: This is NOT a LeetCode problem, but a fundamental DP pattern that appears
in many interview problems.

Difficulty: Medium
Pattern: Dynamic Programming
"""

from typing import List, Tuple


def knapsack_recursive(weights: List[int], values: List[int], capacity: int) -> int:
    """
    Naive recursive approach.

    Time Complexity: O(2^n)
    Space Complexity: O(n) for recursion stack
    """
    def helper(i: int, remaining_capacity: int) -> int:
        # Base case: no items left or no capacity
        if i >= len(weights) or remaining_capacity == 0:
            return 0

        # If current item too heavy, skip it
        if weights[i] > remaining_capacity:
            return helper(i + 1, remaining_capacity)

        # Two choices: include or exclude current item
        include = values[i] + helper(i + 1, remaining_capacity - weights[i])
        exclude = helper(i + 1, remaining_capacity)

        return max(include, exclude)

    return helper(0, capacity)


def knapsack_memoization(weights: List[int], values: List[int], capacity: int) -> int:
    """
    Top-down DP with memoization.

    Time Complexity: O(n * W)
    Space Complexity: O(n * W)
    """
    memo = {}

    def helper(i: int, remaining_capacity: int) -> int:
        if i >= len(weights) or remaining_capacity == 0:
            return 0

        if (i, remaining_capacity) in memo:
            return memo[(i, remaining_capacity)]

        if weights[i] > remaining_capacity:
            result = helper(i + 1, remaining_capacity)
        else:
            include = values[i] + helper(i + 1, remaining_capacity - weights[i])
            exclude = helper(i + 1, remaining_capacity)
            result = max(include, exclude)

        memo[(i, remaining_capacity)] = result
        return result

    return helper(0, capacity)


def knapsack_tabulation(weights: List[int], values: List[int], capacity: int) -> int:
    """
    Bottom-up DP with tabulation.

    Time Complexity: O(n * W)
    Space Complexity: O(n * W)
    """
    n = len(weights)
    dp = [[0] * (capacity + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        for w in range(capacity + 1):
            # If current item too heavy, don't include
            if weights[i - 1] > w:
                dp[i][w] = dp[i - 1][w]
            else:
                # Max of including or excluding current item
                include = values[i - 1] + dp[i - 1][w - weights[i - 1]]
                exclude = dp[i - 1][w]
                dp[i][w] = max(include, exclude)

    return dp[n][capacity]


def knapsack_optimized(weights: List[int], values: List[int], capacity: int) -> int:
    """
    Space-optimized DP using 1D array.

    Time Complexity: O(n * W)
    Space Complexity: O(W)
    """
    dp = [0] * (capacity + 1)

    for i in range(len(weights)):
        # Traverse backwards to avoid using updated values
        for w in range(capacity, weights[i] - 1, -1):
            dp[w] = max(dp[w], values[i] + dp[w - weights[i]])

    return dp[capacity]


# Alias for optimal solution
knapsack = knapsack_optimized


def knapsack_with_items(weights: List[int], values: List[int], capacity: int) -> dict:
    """
    Returns which items to include for maximum value.
    """
    n = len(weights)
    dp = [[0] * (capacity + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        for w in range(capacity + 1):
            if weights[i - 1] > w:
                dp[i][w] = dp[i - 1][w]
            else:
                include = values[i - 1] + dp[i - 1][w - weights[i - 1]]
                exclude = dp[i - 1][w]
                dp[i][w] = max(include, exclude)

    # Backtrack to find items
    selected_items = []
    w = capacity
    for i in range(n, 0, -1):
        if dp[i][w] != dp[i - 1][w]:
            selected_items.append(i - 1)
            w -= weights[i - 1]

    selected_items.reverse()

    return {
        'max_value': dp[n][capacity],
        'selected_indices': selected_items,
        'selected_weights': [weights[i] for i in selected_items],
        'selected_values': [values[i] for i in selected_items],
        'total_weight': sum(weights[i] for i in selected_items)
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'weights': [1, 3, 4, 5],
            'values': [1, 4, 5, 7],
            'capacity': 7,
            'expected': 9,
            'description': 'Basic case'
        },
        {
            'weights': [2, 3, 4, 5],
            'values': [3, 4, 5, 6],
            'capacity': 5,
            'expected': 7,
            'description': 'Limited capacity'
        },
        {
            'weights': [1, 2, 3],
            'values': [6, 10, 12],
            'capacity': 5,
            'expected': 22,
            'description': 'Take all items'
        },
        {
            'weights': [10, 20, 30],
            'values': [60, 100, 120],
            'capacity': 50,
            'expected': 220,
            'description': 'Classic example'
        }
    ]

    print("=" * 70)
    print("0/1 KNAPSACK - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        result_memo = knapsack_memoization(test['weights'], test['values'], test['capacity'])
        result_tab = knapsack_tabulation(test['weights'], test['values'], test['capacity'])
        result_optimal = knapsack_optimized(test['weights'], test['values'], test['capacity'])
        details = knapsack_with_items(test['weights'], test['values'], test['capacity'])

        passed = (result_memo == test['expected'] and
                 result_tab == test['expected'] and
                 result_optimal == test['expected'])

        print(f"\nTest {i}: {test['description']}")
        print(f"Weights: {test['weights']}")
        print(f"Values: {test['values']}")
        print(f"Capacity: {test['capacity']}")
        print(f"Expected: {test['expected']}")
        print(f"Got (Memoization): {result_memo}")
        print(f"Got (Tabulation): {result_tab}")
        print(f"Got (Optimized): {result_optimal}")
        print(f"Selected items (indices): {details['selected_indices']}")
        print(f"Selected weights: {details['selected_weights']} (total: {details['total_weight']})")
        print(f"Selected values: {details['selected_values']} (total: {details['max_value']})")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 70)
    print("DP RECURRENCE RELATION")
    print("=" * 70)
    print("""
dp[i][w] = maximum value using first i items with capacity w

Recurrence:
  If weight[i-1] > w:
    dp[i][w] = dp[i-1][w]  (can't include item)
  Else:
    dp[i][w] = max(
      dp[i-1][w],  (exclude item)
      value[i-1] + dp[i-1][w - weight[i-1]]  (include item)
    )

Base case:
  dp[0][w] = 0 (no items)
  dp[i][0] = 0 (no capacity)
""")

    print("\n" + "=" * 70)
    print("APPROACH COMPARISON")
    print("=" * 70)
    print("Naive Recursive: Time O(2^n), Space O(n)")
    print("Memoization: Time O(n*W), Space O(n*W)")
    print("Tabulation: Time O(n*W), Space O(n*W)")
    print("Optimized 1D: Time O(n*W), Space O(W) - BEST SPACE")

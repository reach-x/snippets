"""
Coin Change

You are given an integer array coins representing coins of different denominations
and an integer amount representing a total amount of money.

Return the fewest number of coins that you need to make up that amount. If that
amount of money cannot be made up by any combination of the coins, return -1.

You may assume that you have an infinite number of each kind of coin.

LeetCode: https://leetcode.com/problems/coin-change/
Difficulty: Medium
Pattern: Dynamic Programming (Unbounded Knapsack)
"""

from typing import List


def coin_change_recursive(coins: List[int], amount: int) -> int:
    """
    Recursive brute force approach (exponential time).

    Time Complexity: O(amount^num_coins) - exponential
    Space Complexity: O(amount) for recursion stack

    NOT PRACTICAL - shown for understanding only.
    """
    def helper(remaining: int) -> int:
        # Base cases
        if remaining == 0:
            return 0
        if remaining < 0:
            return -1

        # Try each coin
        min_coins = float('inf')
        for coin in coins:
            result = helper(remaining - coin)
            if result != -1:
                min_coins = min(min_coins, result + 1)

        return min_coins if min_coins != float('inf') else -1

    return helper(amount)


def coin_change_memoization(coins: List[int], amount: int) -> int:
    """
    Top-down DP with memoization.

    Time Complexity: O(amount * num_coins)
    Space Complexity: O(amount) for memo and recursion stack
    """
    memo = {}

    def helper(remaining: int) -> int:
        # Base cases
        if remaining == 0:
            return 0
        if remaining < 0:
            return -1

        # Check memo
        if remaining in memo:
            return memo[remaining]

        # Try each coin
        min_coins = float('inf')
        for coin in coins:
            result = helper(remaining - coin)
            if result != -1:
                min_coins = min(min_coins, result + 1)

        memo[remaining] = min_coins if min_coins != float('inf') else -1
        return memo[remaining]

    return helper(amount)


def coin_change(coins: List[int], amount: int) -> int:
    """
    Bottom-up DP approach (optimal).

    Time Complexity: O(amount * num_coins)
    Space Complexity: O(amount)

    Why it works:
    - dp[i] = minimum coins needed to make amount i
    - dp[0] = 0 (no coins needed for amount 0)
    - For each amount, try each coin:
      dp[i] = min(dp[i], dp[i - coin] + 1)
    - This is unbounded knapsack (can use each coin unlimited times)
    """
    # dp[i] = min coins to make amount i
    dp = [float('inf')] * (amount + 1)
    dp[0] = 0  # Base case: 0 coins for amount 0

    # For each amount from 1 to target
    for current_amount in range(1, amount + 1):
        # Try each coin
        for coin in coins:
            if coin <= current_amount:
                # Can use this coin
                dp[current_amount] = min(
                    dp[current_amount],
                    dp[current_amount - coin] + 1
                )

    return dp[amount] if dp[amount] != float('inf') else -1


def coin_change_with_coins(coins: List[int], amount: int) -> dict:
    """
    Bottom-up DP that also tracks which coins were used.

    Returns dict with min coins count and actual coins used.
    """
    dp = [float('inf')] * (amount + 1)
    dp[0] = 0

    # Track which coin was used for each amount
    coin_used = [-1] * (amount + 1)

    for current_amount in range(1, amount + 1):
        for coin in coins:
            if coin <= current_amount:
                if dp[current_amount - coin] + 1 < dp[current_amount]:
                    dp[current_amount] = dp[current_amount - coin] + 1
                    coin_used[current_amount] = coin

    # Reconstruct which coins were used
    if dp[amount] == float('inf'):
        return {'min_coins': -1, 'coins': []}

    # Backtrack to find coins
    result_coins = []
    remaining = amount
    while remaining > 0:
        coin = coin_used[remaining]
        result_coins.append(coin)
        remaining -= coin

    return {
        'min_coins': dp[amount],
        'coins': sorted(result_coins)
    }


def coin_change_bfs(coins: List[int], amount: int) -> int:
    """
    BFS approach (alternative perspective).

    Time Complexity: O(amount * num_coins)
    Space Complexity: O(amount)

    Why it works:
    - Treat as shortest path problem
    - Each amount is a node
    - Edges are coin values
    - Find shortest path from 0 to amount
    """
    if amount == 0:
        return 0

    from collections import deque

    queue = deque([(0, 0)])  # (current_amount, num_coins)
    visited = {0}

    while queue:
        current_amount, num_coins = queue.popleft()

        # Try adding each coin
        for coin in coins:
            next_amount = current_amount + coin

            if next_amount == amount:
                return num_coins + 1

            if next_amount < amount and next_amount not in visited:
                visited.add(next_amount)
                queue.append((next_amount, num_coins + 1))

    return -1


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'coins': [1, 2, 5],
            'amount': 11,
            'expected': 3,
            'description': '11 = 5 + 5 + 1 (3 coins)'
        },
        {
            'coins': [2],
            'amount': 3,
            'expected': -1,
            'description': 'Impossible: odd amount with only even coins'
        },
        {
            'coins': [1],
            'amount': 0,
            'expected': 0,
            'description': 'Zero amount needs zero coins'
        },
        {
            'coins': [1, 2, 5],
            'amount': 100,
            'expected': 20,
            'description': '100 = 5 * 20'
        },
        {
            'coins': [1, 3, 4, 5],
            'amount': 7,
            'expected': 2,
            'description': '7 = 3 + 4 or 7 = 5 + 1 + 1 (choose 3+4)'
        },
        {
            'coins': [1, 5, 6, 8],
            'amount': 11,
            'expected': 2,
            'description': '11 = 5 + 6 (2 coins)'
        },
        {
            'coins': [186, 419, 83, 408],
            'amount': 6249,
            'expected': 20,
            'description': 'Large amount with mixed coins'
        }
    ]

    print("=" * 60)
    print("COIN CHANGE - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = coin_change(test['coins'], test['amount'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Coins: {test['coins']}, Amount: {test['amount']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")

        # Show actual coins used
        if result != -1:
            details = coin_change_with_coins(test['coins'], test['amount'])
            print(f"Coins used: {details['coins']}")
            print(f"Verification: {sum(details['coins'])} = {test['amount']}")

        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("COMPLEXITY ANALYSIS")
    print("=" * 60)
    print("Recursive: Time O(amount^num_coins), Space O(amount) - TOO SLOW")
    print("Memoization: Time O(amount * num_coins), Space O(amount)")
    print("Bottom-up DP (Optimal): Time O(amount * num_coins), Space O(amount)")
    print("BFS: Time O(amount * num_coins), Space O(amount)")
    print("\nKey Insight: Classic unbounded knapsack problem")
    print("dp[i] = min(dp[i], dp[i - coin] + 1) for each coin")

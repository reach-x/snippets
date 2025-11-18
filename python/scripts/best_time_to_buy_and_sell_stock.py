"""
6. Best Time to Buy and Sell Stock

You are given an array prices where prices[i] is the price of a given stock on the ith day.
You want to maximize your profit by choosing a single day to buy one stock and choosing
a different day in the future to sell that stock. Return the maximum profit you can achieve
from this transaction. If you cannot achieve any profit, return 0.

LeetCode: https://leetcode.com/problems/best-time-to-buy-and-sell-stock/
Difficulty: Easy
Pattern: Greedy, Sliding Window
"""

from typing import List


def max_profit_brute_force(prices: List[int]) -> int:
    """
    Brute force: Try all buy/sell combinations.

    Time Complexity: O(n^2)
    Space Complexity: O(1)
    """
    max_profit = 0

    for i in range(len(prices)):
        for j in range(i + 1, len(prices)):
            profit = prices[j] - prices[i]
            max_profit = max(max_profit, profit)

    return max_profit


def max_profit(prices: List[int]) -> int:
    """
    Optimal: Track minimum price and maximum profit.

    Time Complexity: O(n)
    Space Complexity: O(1)
    """
    if not prices:
        return 0

    min_price = float('inf')
    max_profit = 0

    for price in prices:
        # Update minimum price seen so far
        min_price = min(min_price, price)
        # Calculate profit if we sell at current price
        profit = price - min_price
        # Update maximum profit
        max_profit = max(max_profit, profit)

    return max_profit


def max_profit_with_details(prices: List[int]) -> dict:
    """
    Returns detailed information about the best transaction.
    """
    if not prices:
        return {
            'max_profit': 0,
            'buy_day': -1,
            'sell_day': -1,
            'buy_price': 0,
            'sell_price': 0
        }

    min_price = float('inf')
    max_profit = 0
    buy_day = 0
    sell_day = 0
    temp_buy_day = 0

    for i, price in enumerate(prices):
        if price < min_price:
            min_price = price
            temp_buy_day = i

        profit = price - min_price
        if profit > max_profit:
            max_profit = profit
            buy_day = temp_buy_day
            sell_day = i

    return {
        'max_profit': max_profit,
        'buy_day': buy_day,
        'sell_day': sell_day,
        'buy_price': prices[buy_day] if max_profit > 0 else 0,
        'sell_price': prices[sell_day] if max_profit > 0 else 0
    }


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'prices': [7, 1, 5, 3, 6, 4],
            'expected': 5,
            'description': 'Buy at 1, sell at 6'
        },
        {
            'prices': [7, 6, 4, 3, 1],
            'expected': 0,
            'description': 'No profit possible (decreasing)'
        },
        {
            'prices': [2, 4, 1],
            'expected': 2,
            'description': 'Buy at 2, sell at 4'
        },
        {
            'prices': [1, 2, 3, 4, 5],
            'expected': 4,
            'description': 'Buy at 1, sell at 5 (increasing)'
        },
        {
            'prices': [3, 3, 3, 3, 3],
            'expected': 0,
            'description': 'All same price'
        },
        {
            'prices': [2, 1, 2, 1, 0, 1, 2],
            'expected': 2,
            'description': 'Multiple local mins/maxs'
        }
    ]

    print("=" * 60)
    print("BEST TIME TO BUY AND SELL STOCK - TEST RESULTS")
    print("=" * 60)

    for i, test in enumerate(test_cases, 1):
        result = max_profit(test['prices'])
        details = max_profit_with_details(test['prices'])
        passed = result == test['expected']

        print(f"\nTest {i}: {test['description']}")
        print(f"Prices: {test['prices']}")
        print(f"Expected Profit: {test['expected']}")
        print(f"Got Profit: {result}")

        if details['max_profit'] > 0:
            print(f"Best Transaction: Buy on day {details['buy_day']} at ${details['buy_price']}, "
                  f"sell on day {details['sell_day']} at ${details['sell_price']}")
        else:
            print(f"Best Transaction: No profitable transaction possible")

        print(f"Status: {'PASS' if passed else 'FAIL'}")

    print("\n" + "=" * 60)
    print("APPROACH COMPARISON")
    print("=" * 60)
    print("Brute Force: Time O(n^2), Space O(1)")
    print("One Pass (Optimal): Time O(n), Space O(1)")
    print("\nKey Insight:")
    print("Track the minimum price seen so far and the maximum profit")
    print("At each price, calculate profit if we sell at current price")

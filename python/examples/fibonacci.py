#!/usr/bin/env python3

def fibonacci_recursive(n):
    """Fibonacci using recursion - O(2^n)"""
    if n <= 1:
        return n
    return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)

def fibonacci_memoization(n, memo={}):
    """Fibonacci with memoization - O(n)"""
    if n in memo:
        return memo[n]
    if n <= 1:
        return n

    memo[n] = fibonacci_memoization(n - 1, memo) + fibonacci_memoization(n - 2, memo)
    return memo[n]

def fibonacci_iterative(n):
    """Fibonacci iterative - O(n)"""
    if n <= 1:
        return n

    prev, curr = 0, 1
    for _ in range(2, n + 1):
        prev, curr = curr, prev + curr

    return curr

def fibonacci_generator(limit):
    """Generate Fibonacci sequence up to limit"""
    a, b = 0, 1
    for _ in range(limit):
        yield a
        a, b = b, a + b

if __name__ == '__main__':
    n = 10

    print(f"Fibonacci({n}) recursive: {fibonacci_recursive(n)}")
    print(f"Fibonacci({n}) memoized: {fibonacci_memoization(n)}")
    print(f"Fibonacci({n}) iterative: {fibonacci_iterative(n)}")

    print(f"\nFirst {n} Fibonacci numbers:")
    print(list(fibonacci_generator(n)))

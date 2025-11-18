"""
14. Combination Sum

Given an array of distinct integers candidates and a target integer target,
return a list of all unique combinations of candidates where the chosen numbers
sum to target. You may return the combinations in any order.

The same number may be chosen from candidates an unlimited number of times.
Two combinations are unique if the frequency of at least one of the chosen
numbers is different.

LeetCode: https://leetcode.com/problems/combination-sum/
Difficulty: Medium
Pattern: Backtracking
"""

from typing import List


def combination_sum(candidates: List[int], target: int) -> List[List[int]]:
    """
    Backtracking approach.

    Time Complexity: O(n^(target/min)) worst case
    Space Complexity: O(target/min) for recursion depth
    """
    result = []

    def backtrack(start: int, current: List[int], remaining: int):
        # Base case: found valid combination
        if remaining == 0:
            result.append(current[:])
            return

        # Base case: exceeded target
        if remaining < 0:
            return

        for i in range(start, len(candidates)):
            # Choose
            current.append(candidates[i])

            # Explore (can reuse same element, so pass i not i+1)
            backtrack(i, current, remaining - candidates[i])

            # Backtrack
            current.pop()

    backtrack(0, [], target)
    return result


def combination_sum_optimized(candidates: List[int], target: int) -> List[List[int]]:
    """
    Optimized with sorting and early termination.

    Time Complexity: O(n^(target/min))
    Space Complexity: O(target/min)
    """
    result = []
    candidates.sort()  # Sort to enable early termination

    def backtrack(start: int, current: List[int], remaining: int):
        if remaining == 0:
            result.append(current[:])
            return

        for i in range(start, len(candidates)):
            # Early termination: if current candidate too large, rest will be too
            if candidates[i] > remaining:
                break

            current.append(candidates[i])
            backtrack(i, current, remaining - candidates[i])
            current.pop()

    backtrack(0, [], target)
    return result


# Test Cases
if __name__ == "__main__":
    test_cases = [
        {
            'candidates': [2, 3, 6, 7],
            'target': 7,
            'expected': [[2, 2, 3], [7]],
            'description': 'Basic case'
        },
        {
            'candidates': [2, 3, 5],
            'target': 8,
            'expected': [[2, 2, 2, 2], [2, 3, 3], [3, 5]],
            'description': 'Multiple valid combinations'
        },
        {
            'candidates': [2],
            'target': 1,
            'expected': [],
            'description': 'No valid combination'
        },
        {
            'candidates': [1],
            'target': 1,
            'expected': [[1]],
            'description': 'Single element match'
        },
        {
            'candidates': [1],
            'target': 2,
            'expected': [[1, 1]],
            'description': 'Reuse same element'
        }
    ]

    print("=" * 70)
    print("COMBINATION SUM - TEST RESULTS")
    print("=" * 70)

    for i, test in enumerate(test_cases, 1):
        result = combination_sum(test['candidates'], test['target'])
        result_optimized = combination_sum_optimized(test['candidates'], test['target'])

        # Sort for comparison
        result_sorted = [sorted(combo) for combo in result]
        result_sorted.sort()
        expected_sorted = [sorted(combo) for combo in test['expected']]
        expected_sorted.sort()

        passed = result_sorted == expected_sorted

        print(f"\nTest {i}: {test['description']}")
        print(f"Candidates: {test['candidates']}")
        print(f"Target: {test['target']}")
        print(f"Expected: {test['expected']}")
        print(f"Got: {result}")
        print(f"Number of combinations: {len(result)}")
        print(f"Status: {'PASS' if passed else 'FAIL'}")

        # Verify each combination
        if result:
            print("Verification:")
            for combo in result:
                combo_sum = sum(combo)
                print(f"  {combo} = {combo_sum}")

    print("\n" + "=" * 70)
    print("BACKTRACKING TEMPLATE")
    print("=" * 70)
    print("""
def combination_sum(candidates, target):
    result = []

    def backtrack(start, current, remaining):
        # Base case: found valid combination
        if remaining == 0:
            result.append(current[:])
            return

        # Base case: exceeded target
        if remaining < 0:
            return

        for i in range(start, len(candidates)):
            # Choose
            current.append(candidates[i])

            # Explore (can reuse same element)
            backtrack(i, current, remaining - candidates[i])

            # Backtrack
            current.pop()

    backtrack(0, [], target)
    return result
""")

    print("\n" + "=" * 70)
    print("KEY INSIGHTS")
    print("=" * 70)
    print("- Can reuse elements: pass 'i' not 'i+1' in recursion")
    print("- Use 'remaining' to track target - current sum")
    print("- Sort candidates for early termination optimization")
    print("- When remaining < 0, backtrack immediately")
    print("- When remaining == 0, found valid combination")
    print("\nComplexity: O(n^(target/min)) where min is smallest candidate")

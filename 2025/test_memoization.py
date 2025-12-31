#!/usr/bin/env python3
"""
Test to show the performance difference between manual memoization and @lru_cache.
"""
import time
from functools import lru_cache

# Recursive function with manual memoization (like elisp-mirrored)
def fib_manual(n):
    memo = {}

    def fib_inner(n):
        if n in memo:
            return memo[n]

        if n <= 1:
            result = n
        else:
            result = fib_inner(n-1) + fib_inner(n-2)

        memo[n] = result
        return result

    return fib_inner(n)


# Recursive function with @lru_cache (like idiomatic)
def fib_lru_wrapper(n):
    @lru_cache(maxsize=None)
    def fib_inner(n):
        if n <= 1:
            return n
        return fib_inner(n-1) + fib_inner(n-2)

    return fib_inner(n)


def benchmark():
    """Benchmark both approaches."""
    N = 30
    iterations = 10000

    print(f"Computing fib({N}) {iterations} times...\n")

    # Manual memoization
    start = time.perf_counter()
    for _ in range(iterations):
        result1 = fib_manual(N)
    elapsed_manual = (time.perf_counter() - start) * 1000

    # @lru_cache
    start = time.perf_counter()
    for _ in range(iterations):
        result2 = fib_lru_wrapper(N)
    elapsed_lru = (time.perf_counter() - start) * 1000

    print(f"Manual memoization: {elapsed_manual:.2f}ms")
    print(f"@lru_cache:         {elapsed_lru:.2f}ms")
    print(f"Speedup:            {elapsed_manual/elapsed_lru:.2f}x")
    print(f"\nBoth produced result: {result1}")

    # Show what's happening per call
    print(f"\nPer-call overhead:")
    print(f"  Manual: {elapsed_manual/iterations:.4f}ms")
    print(f"  LRU:    {elapsed_lru/iterations:.4f}ms")


if __name__ == '__main__':
    benchmark()

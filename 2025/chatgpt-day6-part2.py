#!/usr/bin/env python3
import sys
import math
from typing import List, Tuple

def parse_problems(grid: List[str]) -> List[Tuple[str, int, int]]:
    """
    Return list of (op, left_col, right_col) for each problem segment.
    A segment is a maximal contiguous run of columns that are NOT entirely spaces.
    """
    if not grid:
        return []

    h = len(grid)
    w = max(len(r) for r in grid)
    grid = [r.ljust(w) for r in grid]

    # Columns that are entirely spaces across all rows separate problems.
    sep = [all(grid[r][c] == " " for r in range(h)) for c in range(w)]

    problems = []
    c = 0
    while c < w:
        if sep[c]:
            c += 1
            continue
        start = c
        while c < w and not sep[c]:
            c += 1
        end = c - 1
        # operator lives on the bottom row somewhere inside [start, end]
        op = None
        for cc in range(start, end + 1):
            ch = grid[h - 1][cc]
            if ch in "+*":
                op = ch
                break
        if op is None:
            raise ValueError(f"No operator found in segment cols {start}-{end}")
        problems.append((op, start, end))

    return problems

def solve_part2(data: str) -> int:
    lines = data.splitlines()
    if not lines:
        return 0

    h = len(lines)
    w = max(len(r) for r in lines)
    grid = [r.ljust(w) for r in lines]

    total = 0
    for op, left, right in parse_problems(grid):
        nums = []
        # Right-to-left: each column is one number; read columns from right -> left
        for c in range(right, left - 1, -1):
            digits = []
            for r in range(h - 1):  # exclude bottom operator row
                ch = grid[r][c]
                if ch.isdigit():
                    digits.append(ch)
            if digits:
                nums.append(int("".join(digits)))

        if not nums:
            raise ValueError(f"Problem cols {left}-{right} has no numbers")

        if op == "+":
            total += sum(nums)
        else:  # "*"
            total += math.prod(nums)

    return total

if __name__ == "__main__":
    inp = sys.stdin.read()
    print(solve_part2(inp))


"""
Common data structures and utilities for AOC solutions.
Mirrors the elisp implementation patterns exactly.
"""
import os
import functools
from pathlib import Path
from dataclasses import dataclass

# Path utilities
AOC_ROOT = Path(__file__).parent.parent

def get_input_path(day: int) -> Path:
    return AOC_ROOT / "inputs" / f"day{day:02d}.txt"

def get_sample_path(day: int) -> Path:
    return AOC_ROOT / "inputs" / f"day{day:02d}_sample.txt"

def read_input(day: int) -> str:
    return get_input_path(day).read_text().strip()

def read_sample(day: int) -> str:
    return get_sample_path(day).read_text().strip()

# Testing utilities
EXPECTED_RESULTS = {}

def expect(func_name: str, expected: str):
    """Register expected result for a function."""
    EXPECTED_RESULTS[func_name] = expected

def check_result(func_name: str, result: str, input_type: str = "real"):
    """Check if result matches expected value."""
    if input_type == "sample":
        expected = EXPECTED_RESULTS.get(func_name)
        if expected is None:
            print(f"{func_name} [sample]: {result} (no expected)")
        elif result == expected:
            print(f"✓ {func_name} [sample]: {result}")
        else:
            raise AssertionError(f"✗ {func_name} [sample]: got {result}, expected {expected}")
    else:
        print(f"{func_name}: {result}")
    return result


# Data structure: Point (for day 9)
@dataclass
class Point:
    x: int
    y: int

def point_incx(p: Point) -> Point:
    return Point(p.x + 1, p.y)

def point_decx(p: Point) -> Point:
    return Point(p.x - 1, p.y)

def point_incy(p: Point) -> Point:
    return Point(p.x, p.y + 1)

def point_decy(p: Point) -> Point:
    return Point(p.x, p.y - 1)


# Data structure: Vec2D (for day 9)
# 2D structure using flat list with ROW-MAJOR indexing
# Matches day09.el exactly:
#   - (x, y) where x=column, y=row, origin at top-left
#   - y increases downward, x increases rightward
#   - index = row * num_cols + col = y * cols + x
class Vec2D:
    """2D structure using flat list with manual ROW-MAJOR indexing."""

    def __init__(self, rows: int, cols: int, initial_value=None):
        self.rows = rows
        self.cols = cols
        self.data = [initial_value] * (rows * cols)

    def inbounds(self, p: Point) -> bool:
        return (p.x >= 0 and p.y >= 0 and
                p.x < self.cols and p.y < self.rows)

    def index(self, p: Point) -> int:
        if not self.inbounds(p):
            raise IndexError("out of bounds")
        # row-major: index = row * num_cols + col = y * cols + x
        return p.y * self.cols + p.x

    def ref(self, p: Point):
        return self.data[self.index(p)]

    def set(self, p: Point, value):
        self.data[self.index(p)] = value


# Data structure: Mat (for days 10, 11)
# Matrix using flat list with COLUMN-MAJOR indexing
# Matches day10.el and day11.el exactly:
#   - index = x + y * rows (column-major)
class Mat:
    """Matrix using flat list with manual COLUMN-MAJOR indexing."""

    def __init__(self, rows: int, cols: int, initial_value=None):
        self.rows = rows
        self.cols = cols
        self.data = [initial_value] * (rows * cols)

    def inbounds(self, x: int, y: int) -> bool:
        return x >= 0 and y >= 0 and x < self.rows and y < self.cols

    def index(self, x: int, y: int) -> int:
        if not self.inbounds(x, y):
            raise IndexError("out of bounds")
        # column-major: index = x + y * rows
        return x + y * self.rows

    def ref(self, x: int, y: int):
        return self.data[self.index(x, y)]

    def set(self, x: int, y: int, value):
        self.data[self.index(x, y)] = value


# Math utilities (for day 10)
def gcd(a: int, b: int) -> int:
    """Compute greatest common divisor using Euclidean algorithm."""
    a, b = abs(a), abs(b)
    while b != 0:
        a, b = b, a % b
    return a

def row_gcd(mat: Mat, row_idx: int) -> int:
    """Get GCD of all non-zero values in a row."""
    vals = [abs(mat.ref(row_idx, c))
            for c in range(mat.cols)
            if mat.ref(row_idx, c) != 0]
    if not vals:
        return 1
    return functools.reduce(gcd, vals[1:], vals[0])

def normalize_row(mat: Mat, row_idx: int):
    """Divide row by GCD of its coefficients to keep values bounded."""
    g = row_gcd(mat, row_idx)
    if g > 1:
        for c in range(mat.cols):
            mat.set(row_idx, c, mat.ref(row_idx, c) // g)


# Data structure: RowMajorMat (for day 11)
# Matrix using flat list with ROW-MAJOR indexing
# Matches day11.el exactly (optimization for row-wise access):
#   - index = x * cols + y (row-major)
class RowMajorMat:
    """Matrix using flat list with manual ROW-MAJOR indexing.
    Matches day11.el exactly: index = x * cols + y"""

    def __init__(self, rows: int, cols: int, initial_value=None):
        self.rows = rows
        self.cols = cols
        self.data = [initial_value] * (rows * cols)

    def inbounds(self, x: int, y: int) -> bool:
        return x >= 0 and y >= 0 and x < self.rows and y < self.cols

    def index(self, x: int, y: int) -> int:
        if not self.inbounds(x, y):
            raise IndexError("out of bounds")
        # row-major: index = x * cols + y
        return x * self.cols + y

    def ref(self, x: int, y: int):
        return self.data[self.index(x, y)]

    def set(self, x: int, y: int, value):
        self.data[self.index(x, y)] = value

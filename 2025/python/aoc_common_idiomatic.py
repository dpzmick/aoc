"""
Common utilities for idiomatic Python AOC solutions.
Uses standard library and NumPy patterns.
"""
import os
from pathlib import Path

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

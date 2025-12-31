"""
Day 10: Linear Algebra with Integer-only Gaussian Elimination
Direct port from day10.el maintaining same data structures and algorithms.
"""
import sys
import re
from collections import deque
sys.path.insert(0, '/Users/dpzmick/programming/elisp-aoc/python')

from aoc_common import (
    Mat, gcd, row_gcd, normalize_row,
    read_input, read_sample, expect, check_result
)


# Part 1: BFS through state space with bit operations
class D10Line:
    def __init__(self, length, goal, toggles, joltage_goal):
        self.length = length
        self.goal = goal
        self.toggles = toggles
        self.joltage_goal = joltage_goal


def parse_goal(goal_str: str) -> int:
    """Parse goal string into bitmask."""
    result = 0
    for i, c in enumerate(goal_str):
        if c == '#':
            result |= (1 << i)
    return result


def parse_toggle(positions: list[int]) -> int:
    """Parse toggle positions into bitmask."""
    result = 0
    for pos in positions:
        result |= (1 << pos)
    return result


def parse_toggles(toggles_str: str) -> list[int]:
    """Parse all toggles from string."""
    toggle_regex = r'\(([0-9,]+)\)'
    toggles = []
    for match in re.finditer(toggle_regex, toggles_str):
        positions = [int(x) for x in match.group(1).split(',')]
        toggles.append(parse_toggle(positions))
    return toggles


def extract_line_parts(line: str) -> tuple[str, str, str]:
    """Extract goal, toggles, and joltage from line."""
    line_regex = r'^\[([.#]+)\] (.*?) \{([0-9,]+)\}$'
    match = re.match(line_regex, line)
    if not match:
        raise ValueError(f"Failed to parse line: {line}")
    return match.group(1), match.group(2), match.group(3)


def parse_line(line: str) -> D10Line:
    """Parse a line into D10Line structure."""
    goal_str, toggles_str, jolts_str = extract_line_parts(line)
    return D10Line(
        length=len(goal_str),
        goal=parse_goal(goal_str),
        toggles=parse_toggles(toggles_str),
        joltage_goal=[int(x) for x in jolts_str.split(',')]
    )


def process_input(input_str: str) -> list[D10Line]:
    """Parse all lines."""
    return [parse_line(line) for line in input_str.strip().split('\n') if line]


def apply_toggle(state: int, toggle: int) -> int:
    """Apply toggle using XOR."""
    return state ^ toggle


def solve_line_bfs(line: D10Line) -> int:
    """Solve using BFS through state space."""
    q = deque([(0, 0)])  # (node, distance)
    visited = set()
    goal = line.goal
    toggles = line.toggles

    while q:
        node, dist = q.popleft()

        if node == goal:
            return dist

        if node not in visited:
            visited.add(node)
            for toggle in toggles:
                next_node = apply_toggle(node, toggle)
                q.append((next_node, dist + 1))

    return -1  # No solution found


expect('day10_part1', '7')
def day10_part1(input_str: str) -> str:
    lines = process_input(input_str)
    total = sum(solve_line_bfs(line) for line in lines)
    return str(total)


# Part 2: Linear algebra with integer-only Gaussian elimination
class D10LineP2:
    def __init__(self, toggles, joltage_goal):
        self.toggles = toggles
        self.joltage_goal = joltage_goal


def parse_toggles_p2(toggles_str: str) -> list[list[int]]:
    """Parse toggles as lists of positions (not bitmasks)."""
    toggle_regex = r'\(([0-9,]+)\)'
    toggles = []
    for match in re.finditer(toggle_regex, toggles_str):
        positions = [int(x) for x in match.group(1).split(',')]
        toggles.append(positions)
    return toggles


def parse_line_p2(line: str) -> D10LineP2:
    """Parse line for part 2."""
    _, toggles_str, jolts_str = extract_line_parts(line)
    return D10LineP2(
        toggles=parse_toggles_p2(toggles_str),
        joltage_goal=[int(x) for x in jolts_str.split(',')]
    )


def make_toggles_mat(toggles: list[list[int]], jolts: list[int]) -> Mat:
    """Create augmented matrix A|b for system Ax=b."""
    num_counters = len(jolts)
    num_buttons = len(toggles)
    rows = num_counters
    cols = num_buttons + 1  # +1 for RHS

    m = Mat(rows, cols, initial_value=0)

    # For each button, mark which counters it affects
    for button_idx in range(num_buttons):
        for counter_idx in toggles[button_idx]:
            m.set(counter_idx, button_idx, 1)

    # RHS column (target joltages)
    for counter_idx in range(num_counters):
        m.set(counter_idx, num_buttons, jolts[counter_idx])

    return m


# Matrix operations for Gaussian elimination
def swap_rows(m: Mat, r1: int, r2: int):
    """Swap two rows."""
    if r1 == r2:
        return
    for c in range(m.cols):
        tmp = m.ref(r1, c)
        m.set(r1, c, m.ref(r2, c))
        m.set(r2, c, tmp)


def add_rows(m: Mat, r1: int, r2: int, factor1: int, factor2: int):
    """Add scaled rows: r2 = factor1 * r1 + factor2 * r2."""
    for c in range(m.cols):
        r1_val = m.ref(r1, c)
        r2_val = m.ref(r2, c)
        m.set(r2, c, factor1 * r1_val + factor2 * r2_val)


def nonzero_row_idx_for_col(m: Mat, c: int, st: int = 0):
    """Find first row (>= st) with nonzero value in column c."""
    for r in range(st, m.rows):
        if m.ref(r, c) != 0:
            return r
    return None


def eliminate_down(m: Mat, pivot_r: int, c: int):
    """Eliminate all values below pivot."""
    for r in range(pivot_r + 1, m.rows):
        factor1 = m.ref(r, c)  # scale by our val
        factor2 = -m.ref(pivot_r, c)  # scale by pivot (negated)
        add_rows(m, pivot_r, r, factor1, factor2)
        normalize_row(m, r)


def eliminate_up(m: Mat, pivot_r: int, c: int):
    """Eliminate all values above pivot."""
    for r in range(pivot_r):
        target_val = m.ref(r, c)
        pivot_val = m.ref(pivot_r, c)
        if target_val != 0:
            # r = pivot_val * r - target_val * pivot_row
            add_rows(m, pivot_r, r, -target_val, pivot_val)
            normalize_row(m, r)


def eliminate(m: Mat) -> list[tuple[int, int]]:
    """Perform Gaussian elimination, return pivot columns."""
    pivot_row = 0
    pivot_cols = []

    # Forward elimination (create upper triangular)
    for c in range(m.cols):
        tgt_r = nonzero_row_idx_for_col(m, c, pivot_row)
        if tgt_r is not None:
            swap_rows(m, pivot_row, tgt_r)
            eliminate_down(m, pivot_row, c)
            pivot_cols.append((pivot_row, c))
            pivot_row += 1

    # Back substitution (reduced row echelon form)
    for pr, pc in pivot_cols:
        eliminate_up(m, pr, pc)

    return pivot_cols


def rhs_col(m: Mat) -> int:
    """Column index of RHS."""
    return m.cols - 1


def num_vars(m: Mat) -> int:
    """Number of variables (excludes RHS column)."""
    return m.cols - 1


def pivot_col_set(pivot_cols: list[tuple[int, int]]) -> list[int]:
    """Extract column indices from pivot list."""
    return [pc for _, pc in pivot_cols]


def free_var_cols(m: Mat, pivot_cols: list[tuple[int, int]]) -> list[int]:
    """Find columns that are free variables."""
    pcols = pivot_col_set(pivot_cols)
    return [c for c in range(num_vars(m)) if c not in pcols]


def pivot_row_effective_rhs(m: Mat, pivot_row: int, free_vals: list[tuple[int, int]]) -> int:
    """Compute effective RHS after plugging in free variables."""
    adjustment = sum(m.ref(pivot_row, fc) * fv for fc, fv in free_vals)
    return m.ref(pivot_row, rhs_col(m)) - adjustment


def solve_pivot_var(m: Mat, pivot_row: int, pivot_col: int, free_vals: list[tuple[int, int]]) -> int:
    """Solve for a pivot variable given free variable values."""
    eff_rhs = pivot_row_effective_rhs(m, pivot_row, free_vals)
    pivot_val = m.ref(pivot_row, pivot_col)
    return eff_rhs // pivot_val


def solve_system(m: Mat, pivot_cols: list[tuple[int, int]], free_vals: list[tuple[int, int]]) -> list[tuple[int, int]]:
    """Solve system given free variable values."""
    solution = list(free_vals)
    for pr, pc in pivot_cols:
        solution.append((pc, solve_pivot_var(m, pr, pc, free_vals)))
    return solution


def all_combinations(ranges: list[int]) -> list[list[int]]:
    """Generate all combinations of values in ranges."""
    if not ranges:
        return [[]]

    result = [[]]
    for r in ranges:
        new_result = []
        for i in range(r + 1):
            for combo in result:
                new_result.append(combo + [i])
        result = new_result

    return result


def get_button_max(button_idx: int, toggles: list[list[int]], jolts: list[int]) -> int:
    """Get maximum value for a button (free variable)."""
    affected_counters = toggles[button_idx]
    if affected_counters:
        return min(jolts[c] for c in affected_counters)
    else:
        return max(jolts)


def get_maxes(free_cols: list[int], toggles: list[list[int]], jolts: list[int]) -> list[int]:
    """Get maximum values for all free variables."""
    return [get_button_max(fc, toggles, jolts) for fc in free_cols]


def solution_sum(solution: list[tuple[int, int]]) -> int:
    """Sum all variable values in solution."""
    return sum(v for _, v in solution)


def solution_non_negative_p(sol: list[tuple[int, int]]) -> bool:
    """Check if all solution values are non-negative."""
    return all(v >= 0 for _, v in sol)


def integer_solution_p(m: Mat, pivot_cols: list[tuple[int, int]], free_vals: list[tuple[int, int]]) -> bool:
    """Check if free_vals yields an integer solution."""
    for pr, pc in pivot_cols:
        pivot_val = m.ref(pr, pc)
        eff_rhs = pivot_row_effective_rhs(m, pr, free_vals)
        # Check divisibility using absolute value to handle negative pivots
        if eff_rhs % abs(pivot_val) != 0:
            return False
    return True


def solve_line_p2(line: D10LineP2) -> int:
    """Solve part 2 using linear algebra."""
    toggles = line.toggles
    jolts = line.joltage_goal
    m = make_toggles_mat(toggles, jolts)
    pivots = eliminate(m)
    fvc = free_var_cols(m, pivots)
    fv_maxes = get_maxes(fvc, toggles, jolts)
    all_fv_values = all_combinations(fv_maxes)

    best = None
    for comb in all_fv_values:
        fv_vals = list(zip(fvc, comb))
        if integer_solution_p(m, pivots, fv_vals):
            sol = solve_system(m, pivots, fv_vals)
            if solution_non_negative_p(sol):
                total = solution_sum(sol)
                if best is None or total < best:
                    best = total

    return best if best is not None else -1


expect('day10_part2', '33')
def day10_part2(input_str: str) -> str:
    lines = [parse_line_p2(line) for line in input_str.strip().split('\n') if line]
    total = sum(solve_line_p2(line) for line in lines)
    return str(total)


if __name__ == '__main__':
    # Test on sample input
    sample = read_sample(10)
    print("Testing Part 1 on sample:")
    result1 = day10_part1(sample)
    check_result('day10_part1', result1, 'sample')

    print("\nTesting Part 2 on sample:")
    result2 = day10_part2(sample)
    check_result('day10_part2', result2, 'sample')

    # Run on real input
    real = read_input(10)
    print("\nRunning Part 1 on real input:")
    print(day10_part1(real))

    print("\nRunning Part 2 on real input:")
    print(day10_part2(real))

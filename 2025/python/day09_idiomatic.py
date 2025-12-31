"""
Day 9: 2D Geometry with Coordinate Compression
Idiomatic Python version using NumPy and standard library patterns.
"""
import sys
sys.path.insert(0, '/Users/dpzmick/programming/elisp-aoc/python')

import numpy as np
from collections import deque
from aoc_common_idiomatic import read_input, read_sample, expect, check_result


def process_input(input_str: str) -> list[tuple[int, int]]:
    """Parse input into list of (x, y) tuples."""
    points = []
    for line in input_str.strip().split('\n'):
        if not line:
            continue
        parts = line.split(',')
        points.append((int(parts[0]), int(parts[1])))
    return points


def area_between(a: tuple[int, int], b: tuple[int, int]) -> int:
    """Calculate area of rectangle between two points."""
    return (1 + abs(a[0] - b[0])) * (1 + abs(a[1] - b[1]))


# Part 1: Brute force all pairs
expect('day9_part1', '50')
def day9_part1(input_str: str) -> str:
    pts = process_input(input_str)
    n = len(pts)
    best = 0

    for i in range(n):
        for j in range(i + 1, n):
            area = area_between(pts[i], pts[j])
            if area > best:
                best = area

    return str(best)


# Part 2: Coordinate compression and prefix sums
def compress_points(points: list[tuple[int, int]]) -> tuple[list[tuple[int, int]], list[int], list[int]]:
    """
    Compress coordinates to remove gaps.
    Returns: (compressed_points, x_values, y_values) for uncompression.
    """
    xs = sorted(set(x for x, y in points))
    ys = sorted(set(y for x, y in points))
    x_map = {v: i for i, v in enumerate(xs)}
    y_map = {v: i for i, v in enumerate(ys)}

    compressed = [(x_map[x], y_map[y]) for x, y in points]
    return compressed, xs, ys


def add_padding(cpoints: list[tuple[int, int]]) -> list[tuple[int, int]]:
    """Add 1 to each coordinate for padding."""
    return [(x + 1, y + 1) for x, y in cpoints]


def uncompress_point(p: tuple[int, int], xs: list[int], ys: list[int]) -> tuple[int, int]:
    """Uncompress a point back to original coordinates."""
    return (xs[p[0]], ys[p[1]])


def make_init_grid(cpoints: list[tuple[int, int]]) -> np.ndarray:
    """
    Create initial grid with compressed points marked.
    Grid uses standard NumPy indexing: grid[y, x]
    Returns grid with shape (rows, cols) filled with -1, with points marked as 1.
    """
    max_x = max(x for x, y in cpoints)
    max_y = max(y for x, y in cpoints)

    # Create grid: -1 = unvisited, 0 = exterior, 1 = interior/edge
    grid = np.full((max_y + 2, max_x + 2), -1, dtype=np.int32)

    for x, y in cpoints:
        grid[y, x] = 1

    return grid


def draw_edge(grid: np.ndarray, p1: tuple[int, int], p2: tuple[int, int]):
    """Draw edge between two points."""
    x1, y1 = p1
    x2, y2 = p2

    if x1 == x2:  # vertical line
        y_start, y_end = min(y1, y2), max(y1, y2)
        grid[y_start:y_end+1, x1] = 1
    elif y1 == y2:  # horizontal line
        x_start, x_end = min(x1, x2), max(x1, x2)
        grid[y1, x_start:x_end+1] = 1


def draw_all_edges(grid: np.ndarray, cpoints: list[tuple[int, int]]):
    """Draw edges connecting all consecutive points in a loop."""
    n = len(cpoints)
    for i in range(n):
        draw_edge(grid, cpoints[i], cpoints[(i + 1) % n])


def flood_fill_exterior(grid: np.ndarray):
    """Flood fill from (0,0) to mark all exterior cells as 0."""
    rows, cols = grid.shape
    queue = deque([(0, 0)])

    while queue:
        x, y = queue.popleft()

        if 0 <= x < cols and 0 <= y < rows and grid[y, x] == -1:
            grid[y, x] = 0  # mark as exterior
            queue.extend([(x+1, y), (x-1, y), (x, y+1), (x, y-1)])


def fill_rest(grid: np.ndarray):
    """Fill remaining -1 cells with 1 (interior)."""
    grid[grid == -1] = 1


def prefix_sum(grid: np.ndarray) -> np.ndarray:
    """
    Compute prefix sums along each row (x direction).
    Returns a new array with cumulative sums.
    """
    return np.cumsum(grid, axis=1)


def pair_forms_rect(a: tuple[int, int], b: tuple[int, int], prefix_grid: np.ndarray) -> bool:
    """Check if two points form a valid filled rectangle using prefix sums."""
    ax, ay = a
    bx, by = b
    min_x = min(ax, bx)
    max_x = max(ax, bx)
    dx = max_x - min_x
    min_y = min(ay, by)
    max_y = max(ay, by)

    # Check if all rows between min_y and max_y have the expected prefix sum
    for y in range(min_y, max_y + 1):
        expected = dx + 1  # dx+1 cells should all be 1
        if min_x > 0:
            actual = prefix_grid[y, max_x] - prefix_grid[y, min_x - 1]
        else:
            actual = prefix_grid[y, max_x]

        if actual != expected:
            return False

    return True


def all_valid_rects(cpoints: list[tuple[int, int]], prefix_grid: np.ndarray) -> list[tuple[tuple[int, int], tuple[int, int]]]:
    """Find all pairs of points that form valid rectangles."""
    rects = []
    n = len(cpoints)

    for i in range(n):
        for j in range(i + 1, n):
            if pair_forms_rect(cpoints[i], cpoints[j], prefix_grid):
                rects.append((cpoints[i], cpoints[j]))

    return rects


def largest_rect(all_rects: list[tuple[tuple[int, int], tuple[int, int]]],
                 xs: list[int], ys: list[int]) -> tuple[tuple[int, int], tuple[int, int]]:
    """Find the rectangle with largest area in original coordinates."""
    rects_uncompressed = [
        (uncompress_point((p1[0] - 1, p1[1] - 1), xs, ys),
         uncompress_point((p2[0] - 1, p2[1] - 1), xs, ys))
        for p1, p2 in all_rects
    ]

    return max(rects_uncompressed, key=lambda pair: area_between(pair[0], pair[1]))


expect('day9_part2', '24')
def day9_part2(input_str: str) -> str:
    points = process_input(input_str)
    cpoints, xs, ys = compress_points(points)
    cpoints = add_padding(cpoints)
    grid = make_init_grid(cpoints)

    draw_all_edges(grid, cpoints)
    flood_fill_exterior(grid)
    fill_rest(grid)
    prefix_grid = prefix_sum(grid)

    all_rects = all_valid_rects(cpoints, prefix_grid)
    best = largest_rect(all_rects, xs, ys)

    return str(area_between(best[0], best[1]))


if __name__ == '__main__':
    # Test on sample input
    sample = read_sample(9)
    print("Testing Part 1 on sample:")
    result1 = day9_part1(sample)
    check_result('day9_part1', result1, 'sample')

    print("\nTesting Part 2 on sample:")
    result2 = day9_part2(sample)
    check_result('day9_part2', result2, 'sample')

    # Run on real input
    real = read_input(9)
    print("\nRunning Part 1 on real input:")
    print(day9_part1(real))

    print("\nRunning Part 2 on real input:")
    print(day9_part2(real))

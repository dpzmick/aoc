"""
Day 9: 2D Geometry with Coordinate Compression
Direct port from day09.el maintaining same data structures and algorithms.
"""
import sys
sys.path.insert(0, '/Users/dpzmick/programming/elisp-aoc/python')

from aoc_common import (
    Point, Vec2D, point_incx, point_decx, point_incy, point_decy,
    read_input, read_sample, expect, check_result
)


def process_input(input_str: str) -> list[Point]:
    """Parse input into list of Points."""
    points = []
    for line in input_str.strip().split('\n'):
        if not line:
            continue
        parts = line.split(',')
        points.append(Point(
            x=int(parts[0]),
            y=int(parts[1])
        ))
    return points


def area_between(a: Point, b: Point) -> int:
    """Calculate area of rectangle between two points."""
    return (1 + abs(a.x - b.x)) * (1 + abs(a.y - b.y))


# Part 1: Brute force all pairs
expect('day9_part1', '50')
def day9_part1(input_str: str) -> str:
    pts = process_input(input_str)
    n = len(pts)
    best = 0

    for i in range(n):
        a = pts[i]
        for j in range(i + 1, n):
            b = pts[j]
            area = area_between(a, b)
            if area > best:
                best = area

    return str(best)


# Part 2: Coordinate compression and prefix sums
def compressed_axis(points: list[Point], getter) -> list[int]:
    """Get sorted unique values along an axis."""
    return sorted(set(getter(p) for p in points))


def make_index_map(sorted_values: list[int]) -> dict[int, int]:
    """Create mapping from value to compressed index."""
    return {v: i for i, v in enumerate(sorted_values)}


def compress_points(points: list[Point]) -> list[Point]:
    """Compress coordinates to remove gaps."""
    xs = compressed_axis(points, lambda p: p.x)
    ys = compressed_axis(points, lambda p: p.y)
    x_map = make_index_map(xs)
    y_map = make_index_map(ys)

    return [Point(x=x_map[p.x], y=y_map[p.y]) for p in points]


def add_padding(cpoints: list[Point]) -> list[Point]:
    """Add 1 to each coordinate for padding."""
    return [Point(x=p.x + 1, y=p.y + 1) for p in cpoints]


def remove_padding(p: Point) -> Point:
    """Remove padding from a point."""
    return Point(x=p.x - 1, y=p.y - 1)


def uncompress_point(p: Point, points: list[Point]) -> Point:
    """Uncompress a single point back to original coordinates."""
    xs = compressed_axis(points, lambda p: p.x)
    ys = compressed_axis(points, lambda p: p.y)
    return Point(x=xs[p.x], y=ys[p.y])


def make_init_grid(cpoints: list[Point]) -> Vec2D:
    """Create initial grid with compressed points marked."""
    max_x = max(p.x for p in cpoints)
    max_y = max(p.y for p in cpoints)
    v = Vec2D(max_y + 2, max_x + 2, initial_value=None)

    for cp in cpoints:
        v.set(cp, 1)

    return v


def draw_edge(v: Vec2D, p1: Point, p2: Point):
    """Draw edge between two points."""
    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y

    if x1 == x2:  # vertical line
        for y in range(min(y1, y2), max(y1, y2) + 1):
            v.set(Point(x=x1, y=y), 1)
    elif y1 == y2:  # horizontal line
        for x in range(min(x1, x2), max(x1, x2) + 1):
            v.set(Point(x=x, y=y1), 1)


def draw_all_edges(v: Vec2D, cpoints: list[Point]):
    """Draw edges connecting all consecutive points in a loop."""
    n = len(cpoints)
    for i in range(n):
        draw_edge(v, cpoints[i], cpoints[(i + 1) % n])


def flood_fill_exterior(v: Vec2D):
    """Flood fill from (0,0) to mark all exterior cells as 0."""
    queue = [Point(x=0, y=0)]

    while queue:
        p = queue.pop(0)

        if v.inbounds(p) and v.ref(p) is None:
            v.set(p, 0)  # mark as exterior
            queue.append(point_incx(p))
            queue.append(point_decx(p))
            queue.append(point_incy(p))
            queue.append(point_decy(p))


def fill_rest(v: Vec2D):
    """Fill remaining None cells with 1 (interior)."""
    for x in range(v.cols):
        for y in range(v.rows):
            p = Point(x=x, y=y)
            if v.ref(p) is None:
                v.set(p, 1)


def prefix_sum(v: Vec2D):
    """Compute prefix sums along each row (x direction)."""
    for y in range(v.rows):
        s = 0
        for x in range(v.cols):
            p = Point(x=x, y=y)
            s += v.ref(p)
            v.set(p, s)


def pair_forms_rect(a: Point, b: Point, v: Vec2D) -> bool:
    """Check if two points form a valid filled rectangle."""
    ax, ay = a.x, a.y
    bx, by = b.x, b.y
    min_x = min(ax, bx)
    max_x = max(ax, bx)
    dx = max_x - min_x
    min_y = min(ay, by)
    max_y = max(ay, by)

    # Check if all rows between min_y and max_y have the expected prefix sum
    for y in range(min_y, max_y + 1):
        p_max = Point(x=max_x, y=y)
        p_before_min = Point(x=min_x - 1, y=y) if min_x > 0 else None

        expected = dx + 1  # dx+1 cells should all be 1
        if p_before_min is None or not v.inbounds(p_before_min):
            actual = v.ref(p_max)
        else:
            actual = v.ref(p_max) - v.ref(p_before_min)

        if actual != expected:
            return False

    return True


def all_valid_rects(cpoints: list[Point], v: Vec2D) -> list[tuple[Point, Point]]:
    """Find all pairs of points that form valid rectangles."""
    rects = []
    n = len(cpoints)

    for i in range(n):
        for j in range(i + 1, n):
            if pair_forms_rect(cpoints[i], cpoints[j], v):
                rects.append((cpoints[i], cpoints[j]))

    return rects


def largest_rect(all_rects: list[tuple[Point, Point]], points: list[Point]) -> tuple[Point, Point]:
    """Find the rectangle with largest area in original coordinates."""
    rects_uncompressed = [
        (uncompress_point(remove_padding(pair[0]), points),
         uncompress_point(remove_padding(pair[1]), points))
        for pair in all_rects
    ]

    return max(rects_uncompressed,
               key=lambda pair: area_between(pair[0], pair[1]))


expect('day9_part2', '24')
def day9_part2(input_str: str) -> str:
    points = process_input(input_str)
    cpoints = add_padding(compress_points(points))
    v = make_init_grid(cpoints)

    draw_all_edges(v, cpoints)
    flood_fill_exterior(v)
    fill_rest(v)
    prefix_sum(v)

    all_rects = all_valid_rects(cpoints, v)
    best = largest_rect(all_rects, points)

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

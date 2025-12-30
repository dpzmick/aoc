#!/usr/bin/env python3
"""
Profile Day 9 Part 2 to see where time is spent.
"""
import sys
import time
sys.path.insert(0, 'python')

from day09 import (
    process_input, read_input, compress_points, add_padding,
    make_init_grid, draw_all_edges, flood_fill_exterior,
    fill_rest, prefix_sum, all_valid_rects, largest_rect
)

def profile_day9_part2():
    input_str = read_input(9)

    # Run once to get timings
    start = time.perf_counter()
    points = process_input(input_str)
    t1 = time.perf_counter()

    cpoints = add_padding(compress_points(points))
    t2 = time.perf_counter()

    v = make_init_grid(cpoints)
    t3 = time.perf_counter()

    draw_all_edges(v, cpoints)
    t4 = time.perf_counter()

    flood_fill_exterior(v)
    t5 = time.perf_counter()

    fill_rest(v)
    t6 = time.perf_counter()

    prefix_sum(v)
    t7 = time.perf_counter()

    all_rects = all_valid_rects(cpoints, v)
    t8 = time.perf_counter()

    best = largest_rect(all_rects, points)
    t9 = time.perf_counter()

    total = t9 - start

    print("=" * 80)
    print("DAY 9 PART 2 PROFILING (Python-elisp)")
    print("=" * 80)
    print()
    print(f"{'Step':<30} {'Time (ms)':>12} {'% of total':>12}")
    print("-" * 80)
    print(f"{'Parse input':<30} {(t1-start)*1000:>12.2f} {(t1-start)/total*100:>11.1f}%")
    print(f"{'Compress points':<30} {(t2-t1)*1000:>12.2f} {(t2-t1)/total*100:>11.1f}%")
    print(f"{'Make init grid':<30} {(t3-t2)*1000:>12.2f} {(t3-t2)/total*100:>11.1f}%")
    print(f"{'Draw edges':<30} {(t4-t3)*1000:>12.2f} {(t4-t3)/total*100:>11.1f}%")
    print(f"{'Flood fill':<30} {(t5-t4)*1000:>12.2f} {(t5-t4)/total*100:>11.1f}%")
    print(f"{'Fill rest':<30} {(t6-t5)*1000:>12.2f} {(t6-t5)/total*100:>11.1f}%")
    print(f"{'PREFIX SUM':<30} {(t7-t6)*1000:>12.2f} {(t7-t6)/total*100:>11.1f}%")
    print(f"{'Find all valid rects':<30} {(t8-t7)*1000:>12.2f} {(t8-t7)/total*100:>11.1f}%")
    print(f"{'Find largest rect':<30} {(t9-t8)*1000:>12.2f} {(t9-t8)/total*100:>11.1f}%")
    print("-" * 80)
    print(f"{'TOTAL':<30} {total*1000:>12.2f} {100:>11.1f}%")
    print()

    # Now test idiomatic version
    from day09_idiomatic import (
        process_input as process_input_idiomatic,
        compress_points as compress_points_idiomatic,
        add_padding as add_padding_idiomatic,
        make_init_grid as make_init_grid_idiomatic,
        draw_all_edges as draw_all_edges_idiomatic,
        flood_fill_exterior as flood_fill_exterior_idiomatic,
        fill_rest as fill_rest_idiomatic,
        prefix_sum as prefix_sum_idiomatic,
        all_valid_rects as all_valid_rects_idiomatic,
        largest_rect as largest_rect_idiomatic,
    )

    start = time.perf_counter()
    points = process_input_idiomatic(input_str)
    t1 = time.perf_counter()

    cpoints, xs, ys = compress_points_idiomatic(points)
    t2 = time.perf_counter()

    cpoints = add_padding_idiomatic(cpoints)
    grid = make_init_grid_idiomatic(cpoints)
    t3 = time.perf_counter()

    draw_all_edges_idiomatic(grid, cpoints)
    t4 = time.perf_counter()

    flood_fill_exterior_idiomatic(grid)
    t5 = time.perf_counter()

    fill_rest_idiomatic(grid)
    t6 = time.perf_counter()

    prefix_grid = prefix_sum_idiomatic(grid)
    t7 = time.perf_counter()

    all_rects = all_valid_rects_idiomatic(cpoints, prefix_grid)
    t8 = time.perf_counter()

    best = largest_rect_idiomatic(all_rects, xs, ys)
    t9 = time.perf_counter()

    total_idiomatic = t9 - start

    print()
    print("=" * 80)
    print("DAY 9 PART 2 PROFILING (Python-idiomatic)")
    print("=" * 80)
    print()
    print(f"{'Step':<30} {'Time (ms)':>12} {'% of total':>12}")
    print("-" * 80)
    print(f"{'Parse input':<30} {(t1-start)*1000:>12.2f} {(t1-start)/total_idiomatic*100:>11.1f}%")
    print(f"{'Compress points':<30} {(t2-t1)*1000:>12.2f} {(t2-t1)/total_idiomatic*100:>11.1f}%")
    print(f"{'Make init grid':<30} {(t3-t2)*1000:>12.2f} {(t3-t2)/total_idiomatic*100:>11.1f}%")
    print(f"{'Draw edges':<30} {(t4-t3)*1000:>12.2f} {(t4-t3)/total_idiomatic*100:>11.1f}%")
    print(f"{'Flood fill':<30} {(t5-t4)*1000:>12.2f} {(t5-t4)/total_idiomatic*100:>11.1f}%")
    print(f"{'Fill rest':<30} {(t6-t5)*1000:>12.2f} {(t6-t5)/total_idiomatic*100:>11.1f}%")
    print(f"{'PREFIX SUM (np.cumsum)':<30} {(t7-t6)*1000:>12.2f} {(t7-t6)/total_idiomatic*100:>11.1f}%")
    print(f"{'Find all valid rects':<30} {(t8-t7)*1000:>12.2f} {(t8-t7)/total_idiomatic*100:>11.1f}%")
    print(f"{'Find largest rect':<30} {(t9-t8)*1000:>12.2f} {(t9-t8)/total_idiomatic*100:>11.1f}%")
    print("-" * 80)
    print(f"{'TOTAL':<30} {total_idiomatic*1000:>12.2f} {100:>11.1f}%")
    print()
    print("=" * 80)
    print(f"Python-elisp total:    {total*1000:>8.2f}ms")
    print(f"Python-idiomatic total: {total_idiomatic*1000:>8.2f}ms")
    print(f"Speedup: {total/total_idiomatic:.2f}x")
    print("=" * 80)

if __name__ == '__main__':
    profile_day9_part2()

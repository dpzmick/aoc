#!/usr/bin/env python3
"""
Test to show the cache locality difference between column-major and row-major.
"""

# Elisp uses: index = x + y * rows (COLUMN-MAJOR)
# To access row x, columns 0..n: indices are x, x+rows, x+2*rows, ... (NOT CONTIGUOUS!)
#
# NumPy uses: index = y * cols + x (ROW-MAJOR)
# To access row y, columns 0..n: indices are y*cols, y*cols+1, y*cols+2, ... (CONTIGUOUS!)

def demonstrate_memory_layout():
    rows = 4
    cols = 4

    print("=" * 80)
    print("MEMORY LAYOUT ISSUE IN ELISP DAY 11")
    print("=" * 80)
    print()

    print("Consider a 4x4 adjacency matrix:")
    print("     Col0  Col1  Col2  Col3")
    print("Row0:  0     1     2     3")
    print("Row1:  4     5     6     7")
    print("Row2:  8     9    10    11")
    print("Row3: 12    13    14    15")
    print()

    print("ELISP (Column-major: index = x + y * rows)")
    print("-" * 80)
    print("Storage order: (0,0), (1,0), (2,0), (3,0), (0,1), (1,1), ...")
    print("In memory:      0     4     8    12     1     5    ...")
    print()
    print("To find neighbors of Row 1 (node 1 → all destinations):")
    print("  Access: (1,0), (1,1), (1,2), (1,3)")
    print("  Indices: 1, 1+4=5, 1+8=9, 1+12=13")
    print("  In memory: [0] [1] [2] [3] [4] [5] [6] [7] [8] [9] [10] [11] [12] [13]")
    print("                 ^           ^           ^               ^")
    print("  ❌ NOT CONTIGUOUS! Jumps by 4 (rows) each time")
    print()

    print("NUMPY (Row-major: index = y * cols + x)")
    print("-" * 80)
    print("Storage order: (0,0), (0,1), (0,2), (0,3), (1,0), (1,1), ...")
    print("In memory:      0     1     2     3     4     5    ...")
    print()
    print("To find neighbors of Row 1:")
    print("  Access: (1,0), (1,1), (1,2), (1,3)")
    print("  Indices: 4, 5, 6, 7")
    print("  In memory: [0] [1] [2] [3] [4] [5] [6] [7] [8] [9] [10] [11] [12] [13]")
    print("                             ^   ^   ^   ^")
    print("  ✅ CONTIGUOUS! Sequential access")
    print()

    print("=" * 80)
    print("IMPACT ON DAY 11 (584x584 matrix)")
    print("=" * 80)
    print()

    n = 584
    cache_line = 64  # bytes
    element_size = 8  # bytes (assuming 64-bit integers)

    print(f"Graph size: {n}x{n} = {n*n:,} elements")
    print(f"Memory size: {n*n*element_size:,} bytes = {n*n*element_size/1024/1024:.1f} MB")
    print(f"Cache line size: {cache_line} bytes = {cache_line//element_size} elements")
    print()

    print("ELISP - Column-major access pattern:")
    stride_bytes = n * element_size
    print(f"  Stride between accesses: {stride_bytes:,} bytes ({stride_bytes/cache_line:.1f} cache lines)")
    print(f"  To check 584 neighbors: jumps {stride_bytes:,} bytes each time")
    print(f"  Result: ~{n} cache misses per DFS call (worst case)")
    print()

    print("NUMPY - Row-major access pattern:")
    print(f"  Stride between accesses: {element_size} bytes (sequential)")
    print(f"  To check 584 neighbors: sequential memory access")
    print(f"  Result: ~{n*element_size//cache_line} cache misses per DFS call")
    print()

    print(f"Cache miss ratio: {n / (n*element_size//cache_line):.1f}x worse in column-major!")
    print()

    print("=" * 80)
    print("CONCLUSION")
    print("=" * 80)
    print()
    print("The Elisp code has the WRONG memory order for the access pattern!")
    print()
    print("Fix: Change Elisp to use row-major indexing:")
    print("  OLD: index = x + y * rows  (column-major)")
    print("  NEW: index = y * cols + x  (row-major)")
    print()
    print("Or: Change the access pattern to iterate over columns instead of rows")
    print("    (but this requires transposing the graph logic)")

if __name__ == '__main__':
    demonstrate_memory_layout()

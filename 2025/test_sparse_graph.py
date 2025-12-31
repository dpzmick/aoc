#!/usr/bin/env python3
"""
Test to show the performance difference on sparse graphs.
"""
import time
import numpy as np
from functools import lru_cache

# Simulate the Mat class from elisp-mirrored
class Mat:
    def __init__(self, rows, cols):
        self.rows = rows
        self.cols = cols
        self.data = [0] * (rows * cols)

    def ref(self, x, y):
        return self.data[x + y * self.rows]

    def set(self, x, y, value):
        self.data[x + y * self.rows] = value


def create_sparse_graph(n_nodes, avg_edges):
    """Create a sparse directed graph."""
    # Create both Mat and NumPy version
    mat = Mat(n_nodes, n_nodes)
    np_mat = np.zeros((n_nodes, n_nodes), dtype=np.int32)

    # Add edges (simple chain-like structure for testing)
    for i in range(n_nodes - 1):
        for j in range(min(avg_edges, n_nodes - i - 1)):
            mat.set(i, i + j + 1, 1)
            np_mat[i, i + j + 1] = 1

    return mat, np_mat


def count_neighbors_manual(mat, node, iterations):
    """Count neighbors using Mat.ref() - elisp-mirrored style."""
    start = time.perf_counter()
    total = 0
    for _ in range(iterations):
        count = 0
        for i in range(mat.cols):
            if mat.ref(node, i) == 1:
                count += 1
        total += count
    elapsed = (time.perf_counter() - start) * 1000
    return elapsed, total


def count_neighbors_numpy(np_mat, node, iterations):
    """Count neighbors using NumPy - idiomatic style."""
    start = time.perf_counter()
    total = 0
    for _ in range(iterations):
        children = np.where(np_mat[node] == 1)[0]
        count = len(children)
        total += count
    elapsed = (time.perf_counter() - start) * 1000
    return elapsed, total


def main():
    n_nodes = 584  # Same as Day 11
    avg_edges = 3  # Same as Day 11
    iterations = 100000  # Simulate many recursive calls

    print(f"Sparse graph: {n_nodes} nodes, ~{avg_edges} edges per node\n")

    mat, np_mat = create_sparse_graph(n_nodes, avg_edges)

    # Test from a node with edges
    test_node = 10

    print(f"Testing {iterations:,} iterations of finding neighbors...\n")

    elapsed_manual, total1 = count_neighbors_manual(mat, test_node, iterations)
    elapsed_numpy, total2 = count_neighbors_numpy(np_mat, test_node, iterations)

    print(f"Mat.ref() approach (elisp-mirrored):")
    print(f"  Total time: {elapsed_manual:.2f}ms")
    print(f"  Per iteration: {elapsed_manual/iterations:.6f}ms")
    print()
    print(f"NumPy where approach (idiomatic):")
    print(f"  Total time: {elapsed_numpy:.2f}ms")
    print(f"  Per iteration: {elapsed_numpy/iterations:.6f}ms")
    print()
    print(f"Speedup: {elapsed_manual/elapsed_numpy:.2f}x")
    print()
    print(f"WHY: Mat.ref() loops {n_nodes} times per call,")
    print(f"     but NumPy where + iteration only touches ~{avg_edges} children")


if __name__ == '__main__':
    main()

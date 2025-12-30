"""
Day 11: Graph Path Counting with Memoized DFS
Idiomatic Python version using NumPy and @lru_cache.
"""
import sys
sys.path.insert(0, '/Users/dpzmick/programming/elisp-aoc/python')

from functools import lru_cache
import numpy as np
from aoc_common_idiomatic import read_input, read_sample, expect, check_result


def parse_line(line: str) -> tuple[str, list[str]]:
    """Parse line into source and destinations."""
    parts = line.split(':')
    src = parts[0]
    dsts = parts[1].strip().split()
    return src, dsts


def make_node_table(input_str: str) -> dict[str, int]:
    """Create mapping from node names to IDs."""
    nodes = set()
    for line in input_str.strip().split('\n'):
        if not line:
            continue
        src, dsts = parse_line(line)
        nodes.add(src)
        nodes.update(dsts)

    return {name: i for i, name in enumerate(sorted(nodes))}


def process_input(input_str: str) -> tuple[dict[str, int], np.ndarray]:
    """Parse input into node table and adjacency matrix."""
    table = make_node_table(input_str)
    n = len(table)
    adj = np.zeros((n, n), dtype=np.int32)

    for line in input_str.strip().split('\n'):
        if not line:
            continue
        src, dsts = parse_line(line)
        row = table[src]
        for dst in dsts:
            col = table[dst]
            adj[row, col] = 1

    return table, adj


# Part 1: Simple DFS to count all paths
def dfs(from_node: int, to_node: int, adj: np.ndarray) -> int:
    """Count all paths from from_node to to_node."""
    if from_node == to_node:
        return 1

    # Sum paths from all children
    children = np.where(adj[from_node] == 1)[0]
    return sum(dfs(child, to_node, adj) for child in children)


expect('day11_part1', '5')
def day11_part1(input_str: str) -> str:
    table, adj = process_input(input_str)
    you = table['you']
    out = table['out']
    return str(dfs(you, out, adj))


# Part 2: DFS with memoization tracking specific nodes visited
def dfs_p2(from_node: int, to_node: int, dac_id: int, fft_id: int, adj: np.ndarray) -> int:
    """Count paths that visit both dac and fft nodes."""

    @lru_cache(maxsize=None)
    def dfs_inner(from_node: int, seen_dac: bool, seen_fft: bool) -> int:
        # Update seen flags
        seen_dac = seen_dac or (from_node == dac_id)
        seen_fft = seen_fft or (from_node == fft_id)

        # Base case
        if from_node == to_node:
            return 1 if (seen_dac and seen_fft) else 0

        # Recursive case: sum paths from children
        children = np.where(adj[from_node] == 1)[0]
        return sum(dfs_inner(child, seen_dac, seen_fft) for child in children)

    return dfs_inner(from_node, False, False)


expect('day11_part2', '2')
def day11_part2(input_str: str) -> str:
    table, adj = process_input(input_str)
    svr = table['svr']
    out = table['out']
    dac = table['dac']
    fft = table['fft']
    return str(dfs_p2(svr, out, dac, fft, adj))


if __name__ == '__main__':
    # Test on sample input (sample only has part 2 data)
    sample = read_sample(11)
    print("Testing Part 2 on sample:")
    result2 = day11_part2(sample)
    check_result('day11_part2', result2, 'sample')

    # Run on real input
    real = read_input(11)
    print("\nRunning Part 1 on real input:")
    print(day11_part1(real))

    print("\nRunning Part 2 on real input:")
    print(day11_part2(real))

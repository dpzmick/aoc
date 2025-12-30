"""
Multi-language benchmark comparison script.
Displays results side-by-side for Elisp, Python, Python+NumPy, and LuaJIT.
"""
import json
from pathlib import Path
from collections import defaultdict

def load_results():
    """Load all benchmark result files."""
    results = {}

    # Load Elisp results
    try:
        elisp = json.loads(Path('elisp_results.json').read_text())
        results['elisp'] = elisp
    except FileNotFoundError:
        print("Warning: elisp_results.json not found")
        results['elisp'] = []

    # Load Python results
    try:
        python = json.loads(Path('python/python_results.json').read_text())
        results['python'] = python
    except FileNotFoundError:
        print("Warning: python/python_results.json not found")
        results['python'] = []

    # Load Lua results
    try:
        lua = json.loads(Path('lua/lua_results.json').read_text())
        results['lua'] = lua
    except FileNotFoundError:
        print("Warning: lua/lua_results.json not found")
        results['lua'] = []

    # Load Julia results
    try:
        julia = json.loads(Path('julia/julia_results.json').read_text())
        results['julia'] = julia
    except FileNotFoundError:
        print("Warning: julia/julia_results.json not found")
        results['julia'] = []

    return results


def organize_by_problem(results):
    """Organize results by (day, part) for side-by-side comparison."""
    by_problem = defaultdict(dict)

    # Elisp results (no implementation field)
    for e in results['elisp']:
        key = (e['day'], e['part'])
        by_problem[key]['elisp'] = e

    # Python results (has implementation field)
    for p in results['python']:
        key = (p['day'], p['part'])
        impl = p.get('implementation', 'python')
        by_problem[key][impl] = p

    # Lua results (no implementation field)
    for l in results['lua']:
        key = (l['day'], l['part'])
        by_problem[key]['lua'] = l

    # Julia results (has implementation field like Python)
    for j in results['julia']:
        key = (j['day'], j['part'])
        impl = j.get('implementation', 'julia')
        by_problem[key][impl] = j

    return by_problem


def print_per_problem_results(by_problem):
    """Print side-by-side comparison for each problem."""
    print("=" * 90)
    print("PER-PROBLEM RESULTS")
    print("=" * 90)
    print()

    for (day, part) in sorted(by_problem.keys()):
        impls = by_problem[(day, part)]
        print(f"Day {day:02d} Part {part}:")
        print(f"{'Implementation':<20} {'Mean (ms)':>12} {'±Std (ms)':>12} {'Ratio':>10}")
        print("-" * 57)

        # Get baseline (Python elisp-mirrored) for ratio calculation
        baseline = impls.get('python-elisp', impls.get('python', {})).get('mean_ms', None)

        # Display in order: elisp, python-elisp, python-idiomatic, lua, julia-elisp, julia-idiomatic
        for impl_name in ['elisp', 'python', 'python-elisp', 'python-idiomatic', 'lua', 'julia-elisp', 'julia-idiomatic']:
            if impl_name in impls:
                impl = impls[impl_name]
                mean = impl['mean_ms']
                std = impl['std_dev_ms']
                ratio = mean / baseline if baseline else 1.0
                print(f"{impl_name:<20} {mean:>12.2f} {std:>12.2f} {ratio:>10.2f}x")

        print()


def print_summary(by_problem):
    """Print summary statistics across all solutions."""
    print("=" * 90)
    print("SUMMARY: Total Time for All Solutions")
    print("=" * 90)
    print()

    totals = defaultdict(float)
    for (day, part), impls in by_problem.items():
        for impl_name, impl_data in impls.items():
            totals[impl_name] += impl_data['mean_ms']

    print(f"{'Implementation':<20} {'Total (ms)':>15} {'Ratio to Python':>20}")
    print("-" * 57)

    python_total = totals.get('python-elisp', totals.get('python', 0))
    for impl_name in ['elisp', 'python', 'python-elisp', 'python-idiomatic', 'lua', 'julia-elisp', 'julia-idiomatic']:
        if impl_name in totals:
            total = totals[impl_name]
            ratio = total / python_total if python_total else 1.0
            print(f"{impl_name:<20} {total:>15.2f} {ratio:>20.2f}x")

    print()


def print_fastest_per_problem(by_problem):
    """Show which implementation was fastest for each problem."""
    print("=" * 90)
    print("FASTEST IMPLEMENTATION PER PROBLEM")
    print("=" * 90)
    print()

    for (day, part) in sorted(by_problem.keys()):
        impls = by_problem[(day, part)]
        fastest_name = min(impls.keys(), key=lambda k: impls[k]['mean_ms'])
        fastest = impls[fastest_name]

        print(f"Day {day:02d} Part {part}: {fastest_name:<10} ({fastest['mean_ms']:>8.2f} ms)")

    print()


def print_speedup_matrix(by_problem):
    """Print matrix showing speedup of each implementation vs others."""
    print("=" * 90)
    print("SPEEDUP MATRIX (row / column)")
    print("=" * 90)
    print()

    # Calculate totals
    totals = defaultdict(float)
    for (day, part), impls in by_problem.items():
        for impl_name, impl_data in impls.items():
            totals[impl_name] += impl_data['mean_ms']

    # Filter to implementations that exist
    impl_names = [n for n in ['elisp', 'python', 'python-elisp', 'python-idiomatic', 'lua', 'julia-elisp', 'julia-idiomatic'] if n in totals]

    # Print header
    print(f"{'':>20}", end='')
    for impl in impl_names:
        print(f"{impl:>20}", end='')
    print()

    # Print rows
    for impl1 in impl_names:
        print(f"{impl1:>20}", end='')
        for impl2 in impl_names:
            if impl1 == impl2:
                print(f"{'1.00x':>20}", end='')
            else:
                ratio = totals[impl1] / totals[impl2]
                print(f"{ratio:>19.2f}x", end='')
        print()

    print()


def print_notes():
    """Print implementation notes."""
    print("=" * 90)
    print("NOTES")
    print("=" * 90)
    print()
    print("- 'Ratio' shows performance relative to Python (1.00x = same speed)")
    print("- Lower ratios are faster (0.50x = 2x faster than Python)")
    print("- Higher ratios are slower (2.00x = 2x slower than Python)")
    print("- LuaJIT performance best measured on Linux (macOS may show reduced performance)")
    print()
    print("=" * 90)


def main():
    """Main comparison routine."""
    results = load_results()
    by_problem = organize_by_problem(results)

    if not by_problem:
        print("No benchmark results found!")
        return

    print_per_problem_results(by_problem)
    print_summary(by_problem)
    print_fastest_per_problem(by_problem)
    print_speedup_matrix(by_problem)
    print_notes()


if __name__ == '__main__':
    main()

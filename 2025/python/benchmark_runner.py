"""
Benchmark runner for Python AOC solutions.
Runs solutions multiple times and outputs statistics to JSON.
"""
import sys
import time
import json
import statistics
import importlib
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from aoc_common import read_input


def benchmark_function(func, input_data, warmup=5, iterations=20):
    """Benchmark a single function with warmup and timed runs."""
    slow_threshold_ms = 1000.0  # If iteration takes >1s, reduce iterations
    min_iterations = 3

    # Warm-up runs - check if first warmup is too slow
    for i in range(warmup):
        start = time.perf_counter()
        func(input_data)
        elapsed_ms = (time.perf_counter() - start) * 1000

        if i == 0 and elapsed_ms > slow_threshold_ms:
            iterations = min_iterations
            print(f"  First warmup took {elapsed_ms:.2f}ms, reducing iterations to {iterations}",
                  file=sys.stderr)
            break  # Skip remaining warmup iterations

    # Timed runs
    times = []
    result = None
    for i in range(iterations):
        start = time.perf_counter()
        result = func(input_data)
        end = time.perf_counter()
        elapsed_ms = (end - start) * 1000
        times.append(elapsed_ms)

        # After first timed run, check if we should reduce iterations
        if i == 0 and elapsed_ms > slow_threshold_ms and iterations > min_iterations:
            iterations = min_iterations
            print(f"  First iteration took {elapsed_ms:.2f}ms, reducing remaining iterations to {iterations-1}",
                  file=sys.stderr)

    return times, result


def benchmark_day(day, warmup=5, iterations=20):
    """Benchmark both parts of a day for all available implementations."""
    input_data = read_input(day)
    results = []

    # Try both elisp-mirrored and idiomatic implementations
    implementations = [
        ('', 'python-elisp'),              # day09.py
        ('_idiomatic', 'python-idiomatic') # day09_idiomatic.py
    ]

    for module_suffix, impl_name in implementations:
        module_name = f'day{day:02d}{module_suffix}'
        try:
            module = importlib.import_module(module_name)
        except ImportError:
            continue  # Skip if this implementation doesn't exist

        for part in [1, 2]:
            func_name = f'day{day}_part{part}'
            if hasattr(module, func_name):
                func = getattr(module, func_name)
                times, result = benchmark_function(func, input_data, warmup, iterations)

                results.append({
                    'day': day,
                    'part': part,
                    'implementation': impl_name,  # Track implementation type
                    'mean_ms': statistics.mean(times),
                    'median_ms': statistics.median(times),
                    'std_dev_ms': statistics.stdev(times) if len(times) > 1 else 0,
                    'min_ms': min(times),
                    'max_ms': max(times),
                    'iterations': len(times),
                    'result': result
                })

    return results


def run_all_benchmarks(days, warmup=5, iterations=20):
    """Run benchmarks for specified days."""
    all_results = []

    for day in days:
        print(f"Benchmarking day {day:02d}...", file=sys.stderr)
        try:
            results = benchmark_day(day, warmup, iterations)
            all_results.extend(results)
            for r in results:
                print(f"  {r['implementation']} Part {r['part']}: {r['mean_ms']:.2f}ms "
                      f"(±{r['std_dev_ms']:.2f}ms)", file=sys.stderr)
        except Exception as e:
            print(f"  Error: {e}", file=sys.stderr)
            import traceback
            traceback.print_exc(file=sys.stderr)

    return all_results


def save_results(results, filename):
    """Save benchmark results to JSON."""
    with open(filename, 'w') as f:
        json.dump(results, f, indent=2)


if __name__ == '__main__':
    days = [9, 10, 11]
    warmup = 5
    iterations = 20

    print(f"Running Python benchmarks ({warmup} warmup, {iterations} timed iterations)...",
          file=sys.stderr)
    results = run_all_benchmarks(days, warmup=warmup, iterations=iterations)

    output_file = 'python_results.json'
    save_results(results, output_file)
    print(f"\nResults saved to {output_file}", file=sys.stderr)
    print(f"Benchmarked {len(results)} solutions", file=sys.stderr)

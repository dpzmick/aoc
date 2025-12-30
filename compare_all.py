#!/usr/bin/env python3
"""
Comprehensive comparison of all implementations: Elisp, Python-elisp, Python-idiomatic, Lua, and Julia.
"""
import json
from pathlib import Path
from collections import defaultdict

# Load all results
elisp = json.loads(Path('elisp_results.json').read_text())
python_new = json.loads(Path('python_results.json').read_text())
lua = json.loads(Path('lua/lua_results.json').read_text())
julia = json.loads(Path('julia/julia_results.json').read_text())

# Organize by problem
by_problem = defaultdict(dict)

# Elisp
for e in elisp:
    key = (e['day'], e['part'])
    by_problem[key]['elisp'] = e

# Python (with implementation split)
for p in python_new:
    key = (p['day'], p['part'])
    impl = p.get('implementation', 'python')
    by_problem[key][impl] = p

# Lua
for l in lua:
    key = (l['day'], l['part'])
    by_problem[key]['lua'] = l

# Julia
for j in julia:
    key = (j['day'], j['part'])
    impl = j.get('implementation', 'julia')
    by_problem[key][impl] = j

# Print comprehensive table
print('=' * 140)
print('COMPREHENSIVE PERFORMANCE COMPARISON: All Implementations')
print('=' * 140)
print()
print(f"{'Problem':<12} {'Elisp':>12} {'Python':>12} {'Py-Idiom':>12} {'Lua':>12} {'Julia':>12} {'Jl-Idiom':>12} {'Best':>12} {'Winner':<10}")
print(f"{'':12} {'(ms)':>12} {'(elisp)':>12} {'(ms)':>12} {'(ms)':>12} {'(elisp)':>12} {'(ms)':>12} {'':>12} {'':10}")
print('-' * 140)

totals = defaultdict(float)
for (day, part) in sorted(by_problem.keys()):
    impls = by_problem[(day, part)]

    # Get times
    elisp_time = impls.get('elisp', {}).get('mean_ms', float('inf'))
    python_elisp_time = impls.get('python-elisp', {}).get('mean_ms', float('inf'))
    python_idiomatic_time = impls.get('python-idiomatic', {}).get('mean_ms', float('inf'))
    lua_time = impls.get('lua', {}).get('mean_ms', float('inf'))
    julia_time = impls.get('julia-elisp', {}).get('mean_ms', float('inf'))
    julia_idiomatic_time = impls.get('julia-idiomatic', {}).get('mean_ms', float('inf'))

    # Track totals
    if elisp_time != float('inf'):
        totals['elisp'] += elisp_time
    if python_elisp_time != float('inf'):
        totals['python-elisp'] += python_elisp_time
    if python_idiomatic_time != float('inf'):
        totals['python-idiomatic'] += python_idiomatic_time
    if lua_time != float('inf'):
        totals['lua'] += lua_time
    if julia_time != float('inf'):
        totals['julia-elisp'] += julia_time
    if julia_idiomatic_time != float('inf'):
        totals['julia-idiomatic'] += julia_idiomatic_time

    # Find winner
    times = {
        'Elisp': elisp_time,
        'Py-Elisp': python_elisp_time,
        'Py-Idiom': python_idiomatic_time,
        'Lua': lua_time,
        'Julia': julia_time,
        'Jl-Idiom': julia_idiomatic_time
    }
    winner = min(times.items(), key=lambda x: x[1])

    prob = f"Day {day:02d} Part {part}"
    elisp_str = f"{elisp_time:.2f}" if elisp_time != float('inf') else "---"
    python_elisp_str = f"{python_elisp_time:.2f}" if python_elisp_time != float('inf') else "---"
    python_idiomatic_str = f"{python_idiomatic_time:.2f}" if python_idiomatic_time != float('inf') else "---"
    lua_str = f"{lua_time:.2f}" if lua_time != float('inf') else "---"
    julia_str = f"{julia_time:.2f}" if julia_time != float('inf') else "---"
    julia_idiomatic_str = f"{julia_idiomatic_time:.2f}" if julia_idiomatic_time != float('inf') else "---"

    print(f"{prob:<12} {elisp_str:>12} {python_elisp_str:>12} {python_idiomatic_str:>12} {lua_str:>12} {julia_str:>12} {julia_idiomatic_str:>12} {winner[1]:>12.2f} {winner[0]:<10}")

print()
print('-' * 140)
print(f"{'TOTALS':<12} {totals['elisp']:>12.2f} {totals['python-elisp']:>12.2f} {totals['python-idiomatic']:>12.2f} {totals['lua']:>12.2f} {totals['julia-elisp']:>12.2f} {totals['julia-idiomatic']:>12.2f}")
print('=' * 140)
print()

# Speedup analysis
print()
print('=' * 140)
print('SPEEDUP ANALYSIS (relative to Elisp - reference implementation)')
print('=' * 140)
print()
print(f"{'Problem':<12} {'Py-Elisp':>12} {'Py-Idiom':>12} {'Lua':>12} {'Julia':>12} {'Jl-Idiom':>12}")
print('-' * 140)

for (day, part) in sorted(by_problem.keys()):
    impls = by_problem[(day, part)]

    elisp_time = impls.get('elisp', {}).get('mean_ms')
    if not elisp_time:
        continue

    python_elisp_time = impls.get('python-elisp', {}).get('mean_ms')
    python_idiomatic_time = impls.get('python-idiomatic', {}).get('mean_ms')
    lua_time = impls.get('lua', {}).get('mean_ms')
    julia_time = impls.get('julia-elisp', {}).get('mean_ms')
    julia_idiomatic_time = impls.get('julia-idiomatic', {}).get('mean_ms')

    prob = f"Day {day:02d} Part {part}"

    python_elisp_speedup = elisp_time / python_elisp_time if python_elisp_time else 0
    idiomatic_speedup = elisp_time / python_idiomatic_time if python_idiomatic_time else 0
    lua_speedup = elisp_time / lua_time if lua_time else 0
    julia_speedup = elisp_time / julia_time if julia_time else 0
    julia_idiomatic_speedup = elisp_time / julia_idiomatic_time if julia_idiomatic_time else 0

    print(f"{prob:<12} {python_elisp_speedup:>11.2f}x {idiomatic_speedup:>11.2f}x {lua_speedup:>11.2f}x {julia_speedup:>11.2f}x {julia_idiomatic_speedup:>11.2f}x")

print()
print('-' * 140)
baseline = totals['elisp']
julia_overall = baseline/totals['julia-elisp'] if totals['julia-elisp'] > 0 else 0
julia_idiomatic_overall = baseline/totals['julia-idiomatic'] if totals['julia-idiomatic'] > 0 else 0
print(f"{'OVERALL':<12} {baseline/totals['python-elisp']:>11.2f}x {baseline/totals['python-idiomatic']:>11.2f}x {baseline/totals['lua']:>11.2f}x {julia_overall:>11.2f}x {julia_idiomatic_overall:>11.2f}x")
print('=' * 140)
print()

# Key insights
print()
print("=" * 80)
print("KEY INSIGHTS")
print("=" * 80)
print()

print("1. OVERALL PERFORMANCE (Total time for all problems):")

# Determine the fastest implementation
perf_order = [
    ('Lua (LuaJIT)', totals['lua'], baseline/totals['lua'] if totals['lua'] > 0 else 0),
    ('Julia (JIT)', totals['julia-elisp'], baseline/totals['julia-elisp'] if totals['julia-elisp'] > 0 else 0),
    ('Julia Idiomatic', totals['julia-idiomatic'], baseline/totals['julia-idiomatic'] if totals['julia-idiomatic'] > 0 else 0),
    ('Elisp (native)', totals['elisp'], 1.0),
    ('Python Idiomatic', totals['python-idiomatic'], baseline/totals['python-idiomatic'] if totals['python-idiomatic'] > 0 else 0),
    ('Python Elisp-mirrored', totals['python-elisp'], baseline/totals['python-elisp'] if totals['python-elisp'] > 0 else 0)
]
perf_order.sort(key=lambda x: x[1])

for i, (name, time, ratio) in enumerate(perf_order):
    if time == 0:
        continue
    if i == 0:
        print(f"   - {name:<22} {time:>6,.0f} ms  (FASTEST - {ratio:.1f}x faster than Elisp)")
    elif name.startswith('Elisp'):
        print(f"   - {name:<22} {time:>6,.0f} ms  (REFERENCE IMPLEMENTATION)")
    elif ratio < 1.0:
        print(f"   - {name:<22} {time:>6,.0f} ms  ({1.0/ratio:.1f}x slower than Elisp)")
    else:
        print(f"   - {name:<22} {time:>6,.0f} ms  ({ratio:.1f}x faster than Elisp)")
print()

print("2. PYTHON IDIOMATIC vs ELISP (reference):")
for (day, part) in sorted(by_problem.keys()):
    impls = by_problem[(day, part)]
    elisp_time = impls.get('elisp', {}).get('mean_ms')
    python_idiomatic_time = impls.get('python-idiomatic', {}).get('mean_ms')
    if elisp_time and python_idiomatic_time:
        ratio = python_idiomatic_time / elisp_time
        if ratio < 1.0:
            speedup = 1.0 / ratio
            print(f"   Day {day:02d} Part {part}: {speedup:>5.2f}x faster than Elisp")
        else:
            print(f"   Day {day:02d} Part {part}: {ratio:>5.2f}x slower than Elisp")
print()

print("3. WINNER BY PROBLEM:")
for (day, part) in sorted(by_problem.keys()):
    impls = by_problem[(day, part)]
    times = {
        'Elisp': impls.get('elisp', {}).get('mean_ms', float('inf')),
        'Py-Elisp': impls.get('python-elisp', {}).get('mean_ms', float('inf')),
        'Py-Idiom': impls.get('python-idiomatic', {}).get('mean_ms', float('inf')),
        'Lua': impls.get('lua', {}).get('mean_ms', float('inf')),
        'Julia': impls.get('julia-elisp', {}).get('mean_ms', float('inf')),
        'Jl-Idiom': impls.get('julia-idiomatic', {}).get('mean_ms', float('inf'))
    }
    winner = min(times.items(), key=lambda x: x[1])
    print(f"   Day {day:02d} Part {part}: {winner[0]:<10} ({winner[1]:>8.2f} ms)")
print()

print("=" * 80)

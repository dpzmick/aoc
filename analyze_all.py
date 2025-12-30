import json

# Load all results
with open('lua/lua_results.json') as f:
    lua_results = json.load(f)

with open('python/python_results.json') as f:
    python_results = json.load(f)

with open('elisp_results.json') as f:
    elisp_results = json.load(f)

# Organize by day/part
results = {}
for r in lua_results:
    key = (r['day'], r['part'])
    results[key] = {'lua': r['mean_ms']}

for r in python_results:
    key = (r['day'], r['part'])
    if key not in results:
        results[key] = {}
    results[key]['python'] = r['mean_ms']

for r in elisp_results:
    key = (r['day'], r['part'])
    if key not in results:
        results[key] = {}
    results[key]['elisp'] = r['mean_ms']

# Print comprehensive comparison
print("=" * 100)
print("COMPREHENSIVE BENCHMARK ANALYSIS: Elisp Native vs Python vs LuaJIT (interpreter only)")
print("=" * 100)
print()

for day in sorted(set(k[0] for k in results.keys())):
    print(f"\n{'=' * 100}")
    print(f"DAY {day}")
    print('=' * 100)

    for part in [1, 2]:
        key = (day, part)
        if key not in results:
            continue

        r = results[key]
        elisp_ms = r.get('elisp', 0)
        python_ms = r.get('python', 0)
        lua_ms = r.get('lua', 0)

        print(f"\nPart {part}:")
        print(f"  Elisp (native):  {elisp_ms:>10.2f} ms")
        print(f"  Python:          {python_ms:>10.2f} ms")
        print(f"  LuaJIT:          {lua_ms:>10.2f} ms")

        # Calculate speedups
        if lua_ms > 0:
            print(f"\n  Speedup vs Python:  {python_ms/lua_ms:>6.1f}x")
            print(f"  Speedup vs Elisp:   {elisp_ms/lua_ms:>6.1f}x")

        if elisp_ms > 0 and python_ms > 0:
            print(f"  Elisp vs Python:    {python_ms/elisp_ms:>6.1f}x")

# Summary statistics
print("\n" + "=" * 100)
print("SUMMARY STATISTICS")
print("=" * 100)

total_elisp = sum(results[k].get('elisp', 0) for k in results.keys())
total_python = sum(results[k].get('python', 0) for k in results.keys())
total_lua = sum(results[k].get('lua', 0) for k in results.keys())

print(f"\nTotal time across all solutions:")
print(f"  Elisp:   {total_elisp:>10.2f} ms")
print(f"  Python:  {total_python:>10.2f} ms")
print(f"  LuaJIT:  {total_lua:>10.2f} ms")

print(f"\nOverall speedups:")
print(f"  LuaJIT vs Python:  {total_python/total_lua:.2f}x faster")
print(f"  LuaJIT vs Elisp:   {total_elisp/total_lua:.2f}x faster")
print(f"  Elisp vs Python:   {total_python/total_elisp:.2f}x faster")

# Winner count
print(f"\n{'=' * 100}")
print("WINNER BY PROBLEM")
print('=' * 100)

lua_wins = 0
elisp_wins = 0
python_wins = 0

for key in sorted(results.keys()):
    day, part = key
    r = results[key]
    times = [(v, k) for k, v in r.items()]
    times.sort()
    fastest_time, fastest = times[0]

    if fastest == 'lua':
        lua_wins += 1
        winner = "LuaJIT"
    elif fastest == 'elisp':
        elisp_wins += 1
        winner = "Elisp"
    else:
        python_wins += 1
        winner = "Python"

    print(f"Day {day:2d} Part {part}: {winner:8s} ({fastest_time:>8.2f} ms)")

print(f"\nWin count:")
print(f"  LuaJIT: {lua_wins}/6")
print(f"  Elisp:  {elisp_wins}/6")
print(f"  Python: {python_wins}/6")

# Performance characteristics
print(f"\n{'=' * 100}")
print("PERFORMANCE CHARACTERISTICS BY PROBLEM TYPE")
print('=' * 100)

print("\nDay 9 - Geometry & 2D Grid Operations:")
print("  Part 1 (nested loops, arithmetic): LuaJIT >> Elisp >> Python")
print("  Part 2 (flood fill, prefix sums):  LuaJIT >> Elisp >> Python")
print("  → LuaJIT's tight loops and cache-friendly data structures dominate")

print("\nDay 10 - Linear Algebra (Gaussian Elimination):")
print("  Part 1 (BFS + bit operations):     Python ≈ Elisp > LuaJIT")
print("  Part 2 (integer-only matrix ops):  LuaJIT >> Elisp >> Python")
print("  → Native compilation helps Elisp compete, but LuaJIT still wins overall")

print("\nDay 11 - Graph Traversal (Recursive DFS + Memoization):")
print("  Part 1 & 2: LuaJIT >>> Elisp >> Python")
print("  → LuaJIT's fast function calls and table lookups shine here")

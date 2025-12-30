import json
from pathlib import Path

# Load results
elisp = json.loads(Path('elisp_results.json').read_text())
python = json.loads(Path('python/python_results.json').read_text())

# Print comparison
for e, p in zip(elisp, python):
    day, part = e['day'], e['part']
    e_mean, p_mean = e['mean_ms'], p['mean_ms']
    e_std, p_std = e['std_dev_ms'], p['std_dev_ms']
    speedup = p_mean / e_mean

    print(f'Day {day:02d} Part {part}:')
    print(f'  Elisp:  {e_mean:8.2f} ms  ±{e_std:7.2f} ms')
    print(f'  Python: {p_mean:8.2f} ms  ±{p_std:7.2f} ms')
    print(f'  Ratio:  {speedup:8.2f}x   (Python / Elisp)')
    print()

# Summary statistics
print('----------------------------------------------------------------------')
print('SUMMARY:')
print()

# Calculate total times
elisp_total = sum(e['mean_ms'] for e in elisp)
python_total = sum(p['mean_ms'] for p in python)
overall_speedup = python_total / elisp_total

print(f'Total time (all solutions):')
print(f'  Elisp:  {elisp_total:8.2f} ms')
print(f'  Python: {python_total:8.2f} ms')
print(f'  Overall ratio: {overall_speedup:.2f}x')
print()

# Find min/max ratios
ratios = [p['mean_ms'] / e['mean_ms'] for e, p in zip(elisp, python)]
min_ratio_idx = ratios.index(min(ratios))
max_ratio_idx = ratios.index(max(ratios))

print(f'Smallest ratio: {ratios[min_ratio_idx]:.2f}x (Day {elisp[min_ratio_idx]["day"]:02d} Part {elisp[min_ratio_idx]["part"]})')
print(f'Largest ratio:  {ratios[max_ratio_idx]:.2f}x (Day {elisp[max_ratio_idx]["day"]:02d} Part {elisp[max_ratio_idx]["part"]})')
print()

print('======================================================================')

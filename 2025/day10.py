#!/bin/python3

import sys
from typing import List, Tuple
import functools
import z3

sys.setrecursionlimit(100000)
FILE = sys.argv[1] if len(sys.argv) > 1 else "input.txt"


def read_lines_to_list() -> List[str]:
    lines: List[str] = []
    with open(FILE, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            lines.append(line)

    return lines


def parse_lines(lines):
    machines = []

    for line in lines:
        split = line.split(" ")
        indicator = split[0][1:-1]
        schematics = []

        for s in split[1:-1]:
            if s.startswith("("):
                inner = s[1:-1]
                inner = inner.split(",")

                schematics.append([int(x) for x in inner])
            else:
                print("something wrong")
                exit(1)

        joltage = [int(x) for x in split[-1][1:-1].split(",")]

        machines.append((indicator, schematics, joltage))

    return machines


def part_one():
    lines = read_lines_to_list()
    answer = 0

    machines = parse_lines(lines)

    for line_num, machine in enumerate(machines):
        (target, buttons, _) = machine

        @functools.cache
        def solve(state: str, pressed: int):
            if state == target:
                return 0

            if pressed > len(state):
                return 10000000000000

            best = 10000000000000

            for button in buttons:
                new_state = list(state)

                for val in button:
                    if new_state[val] == ".":
                        new_state[val] = "#"
                    else:
                        new_state[val] = "."

                result = 1 + solve("".join(new_state), pressed + 1)
                if result < best:
                    best = result

            return best

        result = solve("." * len(target), 0)
        print(f"P1 Line {line_num:3d} result={result}")
        answer += result

    print(f"Part 1: {answer}")


def part_two():
    lines = read_lines_to_list()
    answer = 0

    machines = parse_lines(lines)

    for line_num, machine in enumerate(machines):
        (_, buttons, joltages) = machine

        bs = [z3.Int(f"b{i}") for i in range(len(buttons))]
        optimizer = z3.Optimize()

        optimizer.add(
            [
                z3.Sum(bs[b] for b, button in enumerate(buttons) if j in button)
                == joltage
                for (j, joltage) in enumerate(joltages)
            ]
        )
        optimizer.add([b >= 0 for b in bs])

        optimizer.minimize(z3.Sum(bs))
        assert optimizer.check() == z3.sat
        model = optimizer.model()

        result = sum(model[b].as_long() for b in bs)
        print(f"P2 Line {line_num:3d} result={result}")
        answer += result

    print(f"Part 2: {answer}")


part_one()
part_two()

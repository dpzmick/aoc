# Day 10: Linear Algebra with Integer-only Gaussian Elimination
# Idiomatic Julia version using native arrays and standard library patterns

include("aoc_common_idiomatic.jl")

# This file is largely similar to day10.jl since the algorithm is already fairly idiomatic
# The main differences are using native Matrix instead of custom Mat struct where beneficial
# For this problem, the custom indexing doesn't add much value in idiomatic version

# Part 1: BFS through state space - same as direct port
struct D10Line
    length::Int
    goal::Int
    toggles::Vector{Int}
    joltage_goal::Vector{Int}
end

function parse_goal(goal_str::String)::Int
    result = 0
    for (i, c) in enumerate(goal_str)
        if c == '#'
            result |= (1 << (i - 1))
        end
    end
    return result
end

function parse_toggle(positions::Vector{Int})::Int
    result = 0
    for pos in positions
        result |= (1 << pos)
    end
    return result
end

function parse_toggles(toggles_str::String)::Vector{Int}
    toggles = Int[]
    for m in eachmatch(r"\(([0-9,]+)\)", toggles_str)
        positions = [parse(Int, x) for x in split(m.captures[1], ',')]
        push!(toggles, parse_toggle(positions))
    end
    return toggles
end

function extract_line_parts(line::AbstractString)::Tuple{String, String, String}
    m = match(r"^\[([.#]+)\] (.*?) \{([0-9,]+)\}$", line)
    m === nothing && error("Failed to parse line: $line")
    return String(m.captures[1]), String(m.captures[2]), String(m.captures[3])
end

function parse_line(line::AbstractString)::D10Line
    goal_str, toggles_str, jolts_str = extract_line_parts(line)
    return D10Line(
        length(goal_str),
        parse_goal(goal_str),
        parse_toggles(toggles_str),
        [parse(Int, x) for x in split(jolts_str, ',')]
    )
end

function solve_line_bfs(line::D10Line)::Int
    q = [(0, 0)]
    visited = Set{Int}()

    while !isempty(q)
        node, dist = popfirst!(q)
        node == line.goal && return dist

        if node ∉ visited
            push!(visited, node)
            for toggle in line.toggles
                push!(q, (node ⊻ toggle, dist + 1))
            end
        end
    end

    return -1
end

expect("day10_part1", "7")
function day10_part1(input_str::String)::String
    lines = [parse_line(line) for line in split(strip(input_str), '\n') if !isempty(line)]
    return string(sum(solve_line_bfs(line) for line in lines))
end

# Part 2: Use native Matrix{Int} for cleaner code
struct D10LineP2
    toggles::Vector{Vector{Int}}
    joltage_goal::Vector{Int}
end

function parse_toggles_p2(toggles_str::String)::Vector{Vector{Int}}
    [begin
        [parse(Int, x) for x in split(m.captures[1], ',')]
    end for m in eachmatch(r"\(([0-9,]+)\)", toggles_str)]
end

function parse_line_p2(line::AbstractString)::D10LineP2
    _, toggles_str, jolts_str = extract_line_parts(line)
    D10LineP2(
        parse_toggles_p2(toggles_str),
        [parse(Int, x) for x in split(jolts_str, ',')]
    )
end

function make_toggles_mat(toggles::Vector{Vector{Int}}, jolts::Vector{Int})::Matrix{Int}
    """Create augmented matrix [A|b] using native Julia Matrix with 1-based indexing."""
    num_counters = length(jolts)
    num_buttons = length(toggles)
    m = zeros(Int, num_counters, num_buttons + 1)

    for (button_idx, toggle) in enumerate(toggles)
        for counter_idx in toggle
            m[counter_idx + 1, button_idx] = 1  # +1 for counter_idx (0-based to 1-based)
        end
    end

    m[:, end] = jolts
    return m
end

# Gaussian elimination using native matrix
function eliminate!(m::Matrix{Int})::Vector{Tuple{Int, Int}}
    rows, cols = size(m)
    pivot_row = 1
    pivot_cols = Tuple{Int, Int}[]

    for c in 1:cols
        # Find pivot
        tgt_r = findfirst(r -> m[r, c] != 0, pivot_row:rows)
        tgt_r === nothing && continue
        tgt_r += pivot_row - 1

        # Swap rows
        m[[pivot_row, tgt_r], :] = m[[tgt_r, pivot_row], :]

        # Eliminate down
        for r in pivot_row+1:rows
            factor1 = m[r, c]
            factor2 = -m[pivot_row, c]
            m[r, :] = factor1 * m[pivot_row, :] + factor2 * m[r, :]
            nonzero = m[r, :][m[r, :] .!= 0]
            if !isempty(nonzero)
                g = gcd(nonzero...)
                g > 1 && (m[r, :] .÷= g)
            end
        end

        push!(pivot_cols, (pivot_row, c))
        pivot_row += 1
    end

    # Back substitution
    for (pr, pc) in pivot_cols
        for r in 1:pr-1
            if m[r, pc] != 0
                target_val = m[r, pc]
                pivot_val = m[pr, pc]
                m[r, :] = pivot_val * m[r, :] - target_val * m[pr, :]
                nonzero = m[r, :][m[r, :] .!= 0]
                if !isempty(nonzero)
                    g = gcd(nonzero...)
                    g > 1 && (m[r, :] .÷= g)
                end
            end
        end
    end

    return pivot_cols
end

function solve_line_p2(line::D10LineP2)::Int
    toggles, jolts = line.toggles, line.joltage_goal
    m = make_toggles_mat(toggles, jolts)
    pivots = eliminate!(m)

    pivot_cols_set = Set(pc for (_, pc) in pivots)
    free_cols = [c for c in 1:size(m, 2)-1 if c ∉ pivot_cols_set]

    # Get maxes for free variables
    fv_maxes = [begin
        affected = toggles[fc]
        isempty(affected) ? maximum(jolts) : minimum(jolts[c+1] for c in affected)
    end for fc in free_cols]

    # Try all combinations
    best = nothing
    function try_combination(combo)
        # Check if this gives integer solution
        for (pr, pc) in pivots
            rhs = m[pr, end] - sum(m[pr, fc] * v for (fc, v) in zip(free_cols, combo); init=0)
            rhs % abs(m[pr, pc]) != 0 && return
        end

        # Compute solution
        solution = Dict(fc => v for (fc, v) in zip(free_cols, combo))
        for (pr, pc) in pivots
            rhs = m[pr, end] - sum(m[pr, fc] * get(solution, fc, 0) for fc in free_cols; init=0)
            solution[pc] = rhs ÷ m[pr, pc]
        end

        # Check non-negative
        all(v >= 0 for v in values(solution)) || return

        total = sum(values(solution))
        best = (best === nothing || total < best) ? total : best
    end

    # Generate all combinations
    function gen_combos(idx, current)
        if idx > length(fv_maxes)
            try_combination(current)
        else
            for v in 0:fv_maxes[idx]
                gen_combos(idx + 1, vcat(current, [v]))
            end
        end
    end

    gen_combos(1, Int[])
    return best === nothing ? -1 : best
end

expect("day10_part2", "33")
function day10_part2(input_str::String)::String
    lines = [parse_line_p2(line) for line in split(strip(input_str), '\n') if !isempty(line)]
    return string(sum(solve_line_p2(line) for line in lines))
end

# Main execution
if abspath(PROGRAM_FILE) == @__FILE__
    sample = read_sample(10)
    println("Testing Part 1 on sample:")
    check_result("day10_part1", day10_part1(sample), "sample")

    println("\nTesting Part 2 on sample:")
    check_result("day10_part2", day10_part2(sample), "sample")

    real = read_input(10)
    println("\nRunning Part 1 on real input:")
    println(day10_part1(real))

    println("\nRunning Part 2 on real input:")
    println(day10_part2(real))
end

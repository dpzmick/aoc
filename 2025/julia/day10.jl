# Day 10: Linear Algebra with Integer-only Gaussian Elimination
# Direct port from day10.el maintaining same data structures and algorithms

include("aoc_common.jl")

# Part 1: BFS through state space with bit operations
struct D10Line
    length::Int
    goal::Int
    toggles::Vector{Int}
    joltage_goal::Vector{Int}
end

function parse_goal(goal_str::String)::Int
    """Parse goal string into bitmask."""
    result = 0
    for (i, c) in enumerate(goal_str)
        if c == '#'
            result |= (1 << (i - 1))  # -1 for 0-based bit indexing
        end
    end
    return result
end

function parse_toggle(positions::Vector{Int})::Int
    """Parse toggle positions into bitmask."""
    result = 0
    for pos in positions
        result |= (1 << pos)
    end
    return result
end

function parse_toggles(toggles_str::String)::Vector{Int}
    """Parse all toggles from string."""
    toggles = Int[]
    for m in eachmatch(r"\(([0-9,]+)\)", toggles_str)
        positions = [parse(Int, x) for x in split(m.captures[1], ',')]
        push!(toggles, parse_toggle(positions))
    end
    return toggles
end

function extract_line_parts(line::AbstractString)::Tuple{String, String, String}
    """Extract goal, toggles, and joltage from line."""
    m = match(r"^\[([.#]+)\] (.*?) \{([0-9,]+)\}$", line)
    if m === nothing
        error("Failed to parse line: $line")
    end
    return String(m.captures[1]), String(m.captures[2]), String(m.captures[3])
end

function parse_line(line::AbstractString)::D10Line
    """Parse a line into D10Line structure."""
    goal_str, toggles_str, jolts_str = extract_line_parts(line)
    return D10Line(
        length(goal_str),
        parse_goal(goal_str),
        parse_toggles(toggles_str),
        [parse(Int, x) for x in split(jolts_str, ',')]
    )
end

function process_input(input_str::String)::Vector{D10Line}
    """Parse all lines."""
    return [parse_line(line) for line in split(strip(input_str), '\n') if !isempty(line)]
end

function apply_toggle(state::Int, toggle::Int)::Int
    """Apply toggle using XOR."""
    return state ⊻ toggle
end

function solve_line_bfs(line::D10Line)::Int
    """Solve using BFS through state space."""
    q = [(0, 0)]  # (node, distance)
    visited = Set{Int}()
    goal = line.goal
    toggles = line.toggles

    while !isempty(q)
        node, dist = popfirst!(q)

        if node == goal
            return dist
        end

        if node ∉ visited
            push!(visited, node)
            for toggle in toggles
                next_node = apply_toggle(node, toggle)
                push!(q, (next_node, dist + 1))
            end
        end
    end

    return -1  # No solution found
end

expect("day10_part1", "7")
function day10_part1(input_str::String)::String
    lines = process_input(input_str)
    total = sum(solve_line_bfs(line) for line in lines)
    return string(total)
end

# Part 2: Linear algebra with integer-only Gaussian elimination
struct D10LineP2
    toggles::Vector{Vector{Int}}
    joltage_goal::Vector{Int}
end

function parse_toggles_p2(toggles_str::String)::Vector{Vector{Int}}
    """Parse toggles as lists of positions (not bitmasks)."""
    toggles = Vector{Int}[]
    for m in eachmatch(r"\(([0-9,]+)\)", toggles_str)
        positions = [parse(Int, x) for x in split(m.captures[1], ',')]
        push!(toggles, positions)
    end
    return toggles
end

function parse_line_p2(line::AbstractString)::D10LineP2
    """Parse line for part 2."""
    _, toggles_str, jolts_str = extract_line_parts(line)
    return D10LineP2(
        parse_toggles_p2(toggles_str),
        [parse(Int, x) for x in split(jolts_str, ',')]
    )
end

function make_toggles_mat(toggles::Vector{Vector{Int}}, jolts::Vector{Int})::Mat
    """Create augmented matrix A|b for system Ax=b."""
    num_counters = length(jolts)
    num_buttons = length(toggles)
    rows = num_counters
    cols = num_buttons + 1  # +1 for RHS

    m = Mat(rows, cols, 0)

    # For each button, mark which counters it affects
    for (button_idx, toggle) in enumerate(toggles)
        for counter_idx in toggle
            m[counter_idx, button_idx - 1] = 1  # -1 because button_idx is 1-based, matrix is 0-based
        end
    end

    # RHS column (target joltages)
    for (counter_idx, jolt) in enumerate(jolts)
        m[counter_idx - 1, num_buttons] = jolt  # -1 for 0-based indexing
    end

    return m
end

# Matrix operations for Gaussian elimination
function swap_rows!(m::Mat, r1::Int, r2::Int)
    """Swap two rows."""
    if r1 == r2
        return
    end
    for c in 0:m.cols-1
        tmp = m[r1, c]
        m[r1, c] = m[r2, c]
        m[r2, c] = tmp
    end
end

function add_rows!(m::Mat, r1::Int, r2::Int, factor1::Int, factor2::Int)
    """Add scaled rows: r2 = factor1 * r1 + factor2 * r2."""
    for c in 0:m.cols-1
        r1_val = m[r1, c]
        r2_val = m[r2, c]
        m[r2, c] = factor1 * r1_val + factor2 * r2_val
    end
end

function nonzero_row_idx_for_col(m::Mat, c::Int, st::Int=0)::Union{Int, Nothing}
    """Find first row (>= st) with nonzero value in column c."""
    for r in st:m.rows-1
        if m[r, c] != 0
            return r
        end
    end
    return nothing
end

function eliminate_down!(m::Mat, pivot_r::Int, c::Int)
    """Eliminate all values below pivot."""
    for r in pivot_r+1:m.rows-1
        factor1 = m[r, c]  # scale by our val
        factor2 = -m[pivot_r, c]  # scale by pivot (negated)
        add_rows!(m, pivot_r, r, factor1, factor2)
        normalize_row!(m, r)
    end
end

function eliminate_up!(m::Mat, pivot_r::Int, c::Int)
    """Eliminate all values above pivot."""
    for r in 0:pivot_r-1
        target_val = m[r, c]
        pivot_val = m[pivot_r, c]
        if target_val != 0
            # r = pivot_val * r - target_val * pivot_row
            add_rows!(m, pivot_r, r, -target_val, pivot_val)
            normalize_row!(m, r)
        end
    end
end

function eliminate!(m::Mat)::Vector{Tuple{Int, Int}}
    """Perform Gaussian elimination, return pivot columns."""
    pivot_row = 0
    pivot_cols = Tuple{Int, Int}[]

    # Forward elimination (create upper triangular)
    for c in 0:m.cols-1
        tgt_r = nonzero_row_idx_for_col(m, c, pivot_row)
        if tgt_r !== nothing
            swap_rows!(m, pivot_row, tgt_r)
            eliminate_down!(m, pivot_row, c)
            push!(pivot_cols, (pivot_row, c))
            pivot_row += 1
        end
    end

    # Back substitution (reduced row echelon form)
    for (pr, pc) in pivot_cols
        eliminate_up!(m, pr, pc)
    end

    return pivot_cols
end

function rhs_col(m::Mat)::Int
    """Column index of RHS."""
    return m.cols - 1
end

function num_vars(m::Mat)::Int
    """Number of variables (excludes RHS column)."""
    return m.cols - 1
end

function pivot_col_set(pivot_cols::Vector{Tuple{Int, Int}})::Vector{Int}
    """Extract column indices from pivot list."""
    return [pc for (_, pc) in pivot_cols]
end

function free_var_cols(m::Mat, pivot_cols::Vector{Tuple{Int, Int}})::Vector{Int}
    """Find columns that are free variables."""
    pcols = Set(pivot_col_set(pivot_cols))
    return [c for c in 0:num_vars(m)-1 if c ∉ pcols]
end

function pivot_row_effective_rhs(m::Mat, pivot_row::Int, free_vals::Vector{Tuple{Int, Int}})::Int
    """Compute effective RHS after plugging in free variables."""
    adjustment = sum(m[pivot_row, fc] * fv for (fc, fv) in free_vals; init=0)
    return m[pivot_row, rhs_col(m)] - adjustment
end

function solve_pivot_var(m::Mat, pivot_row::Int, pivot_col::Int, free_vals::Vector{Tuple{Int, Int}})::Int
    """Solve for a pivot variable given free variable values."""
    eff_rhs = pivot_row_effective_rhs(m, pivot_row, free_vals)
    pivot_val = m[pivot_row, pivot_col]
    return eff_rhs ÷ pivot_val
end

function solve_system(m::Mat, pivot_cols::Vector{Tuple{Int, Int}}, free_vals::Vector{Tuple{Int, Int}})::Vector{Tuple{Int, Int}}
    """Solve system given free variable values."""
    solution = copy(free_vals)
    for (pr, pc) in pivot_cols
        push!(solution, (pc, solve_pivot_var(m, pr, pc, free_vals)))
    end
    return solution
end

function all_combinations(ranges::Vector{Int})::Vector{Vector{Int}}
    """Generate all combinations of values in ranges."""
    if isempty(ranges)
        return [Int[]]
    end

    result = [Int[]]
    for r in ranges
        new_result = Vector{Int}[]
        for i in 0:r
            for combo in result
                push!(new_result, vcat(combo, [i]))
            end
        end
        result = new_result
    end

    return result
end

function get_button_max(button_idx::Int, toggles::Vector{Vector{Int}}, jolts::Vector{Int})::Int
    """Get maximum value for a button (free variable)."""
    affected_counters = toggles[button_idx + 1]  # +1 for Julia 1-indexing
    if !isempty(affected_counters)
        return minimum(jolts[c + 1] for c in affected_counters)  # +1 for Julia 1-indexing
    else
        return maximum(jolts)
    end
end

function get_maxes(free_cols::Vector{Int}, toggles::Vector{Vector{Int}}, jolts::Vector{Int})::Vector{Int}
    """Get maximum values for all free variables."""
    return [get_button_max(fc, toggles, jolts) for fc in free_cols]
end

function solution_sum(solution::Vector{Tuple{Int, Int}})::Int
    """Sum all variable values in solution."""
    return sum(v for (_, v) in solution)
end

function solution_non_negative_p(sol::Vector{Tuple{Int, Int}})::Bool
    """Check if all solution values are non-negative."""
    return all(v >= 0 for (_, v) in sol)
end

function integer_solution_p(m::Mat, pivot_cols::Vector{Tuple{Int, Int}}, free_vals::Vector{Tuple{Int, Int}})::Bool
    """Check if free_vals yields an integer solution."""
    for (pr, pc) in pivot_cols
        pivot_val = m[pr, pc]
        eff_rhs = pivot_row_effective_rhs(m, pr, free_vals)
        # Check divisibility using absolute value to handle negative pivots
        if eff_rhs % abs(pivot_val) != 0
            return false
        end
    end
    return true
end

function solve_line_p2(line::D10LineP2)::Int
    """Solve part 2 using linear algebra."""
    toggles = line.toggles
    jolts = line.joltage_goal
    m = make_toggles_mat(toggles, jolts)
    pivots = eliminate!(m)
    fvc = free_var_cols(m, pivots)
    fv_maxes = get_maxes(fvc, toggles, jolts)
    all_fv_values = all_combinations(fv_maxes)

    best = nothing
    for comb in all_fv_values
        fv_vals = collect(zip(fvc, comb))
        if integer_solution_p(m, pivots, fv_vals)
            sol = solve_system(m, pivots, fv_vals)
            if solution_non_negative_p(sol)
                total = solution_sum(sol)
                if best === nothing || total < best
                    best = total
                end
            end
        end
    end

    return best === nothing ? -1 : best
end

expect("day10_part2", "33")
function day10_part2(input_str::String)::String
    lines = [parse_line_p2(line) for line in split(strip(input_str), '\n') if !isempty(line)]
    total = sum(solve_line_p2(line) for line in lines)
    return string(total)
end

# Main execution
if abspath(PROGRAM_FILE) == @__FILE__
    # Test on sample input
    sample = read_sample(10)
    println("Testing Part 1 on sample:")
    result1 = day10_part1(sample)
    check_result("day10_part1", result1, "sample")

    println("\nTesting Part 2 on sample:")
    result2 = day10_part2(sample)
    check_result("day10_part2", result2, "sample")

    # Run on real input
    real = read_input(10)
    println("\nRunning Part 1 on real input:")
    println(day10_part1(real))

    println("\nRunning Part 2 on real input:")
    println(day10_part2(real))
end

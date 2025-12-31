# Common utilities for idiomatic Julia AOC solutions
# Uses native Julia patterns and standard library

# Path utilities (same as direct mirror)
const AOC_ROOT = dirname(dirname(@__FILE__))

function get_input_path(day::Int)
    joinpath(AOC_ROOT, "inputs", "day$(lpad(day, 2, '0')).txt")
end

function get_sample_path(day::Int)
    joinpath(AOC_ROOT, "inputs", "day$(lpad(day, 2, '0'))_sample.txt")
end

function read_input(day::Int)
    String(strip(read(get_input_path(day), String)))
end

function read_sample(day::Int)
    String(strip(read(get_sample_path(day), String)))
end

# Testing utilities
const EXPECTED_RESULTS = Dict{String, String}()

function expect(func_name::String, expected::String)
    """Register expected result for a function."""
    EXPECTED_RESULTS[func_name] = expected
end

function check_result(func_name::String, result::String, input_type::String="real")
    """Check if result matches expected value."""
    if input_type == "sample"
        if haskey(EXPECTED_RESULTS, func_name)
            expected = EXPECTED_RESULTS[func_name]
            if result == expected
                println("✓ $func_name [sample]: $result")
            else
                error("✗ $func_name [sample]: got $result, expected $expected")
            end
        else
            println("$func_name [sample]: $result (no expected)")
        end
    else
        println("$func_name: $result")
    end
    return result
end

# Data types: Use native Julia patterns

# Point as tuple for simplicity
const Point = Tuple{Int, Int}

point_incx(p::Point) = (p[1] + 1, p[2])
point_decx(p::Point) = (p[1] - 1, p[2])
point_incy(p::Point) = (p[1], p[2] + 1)
point_decy(p::Point) = (p[1], p[2] - 1)

# Use native Matrix for 2D arrays
# Julia matrices are column-major by default with [row, col] indexing

# Adjacency list representation for graphs (for day 11)
const AdjacencyList = Vector{Vector{Int}}

function create_adjacency_list(n::Int)
    """Create empty adjacency list with n nodes."""
    [Int[] for _ in 1:n]
end

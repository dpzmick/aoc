# Common data structures and utilities for AOC solutions
# Mirrors the elisp implementation patterns exactly

# Path utilities
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

# Data structure: Point (for day 9)
struct Point
    x::Int
    y::Int
end

point_incx(p::Point) = Point(p.x + 1, p.y)
point_decx(p::Point) = Point(p.x - 1, p.y)
point_incy(p::Point) = Point(p.x, p.y + 1)
point_decy(p::Point) = Point(p.x, p.y - 1)

# Data structure: Vec2D (for day 9)
# 2D structure using flat vector with ROW-MAJOR indexing
# Matches day09.el exactly:
#   - (x, y) where x=column, y=row, origin at top-left
#   - y increases downward, x increases rightward
#   - index = row * num_cols + col = y * cols + x + 1 (Julia 1-indexed)
mutable struct Vec2D
    data::Vector{Union{Nothing, Int}}
    rows::Int
    cols::Int
end

function Vec2D(rows::Int, cols::Int, initial_value=nothing)
    Vec2D(fill(initial_value, rows * cols), rows, cols)
end

function inbounds(v::Vec2D, p::Point)
    p.x >= 0 && p.y >= 0 && p.x < v.cols && p.y < v.rows
end

function index(v::Vec2D, p::Point)
    @assert inbounds(v, p) "Point out of bounds: $(p.x), $(p.y)"
    # Row-major: index = y * cols + x, then +1 for Julia's 1-indexing
    return p.y * v.cols + p.x + 1
end

Base.getindex(v::Vec2D, p::Point) = v.data[index(v, p)]
Base.setindex!(v::Vec2D, val, p::Point) = (v.data[index(v, p)] = val)

# Data structure: Mat (for day 10)
# Matrix using flat vector with COLUMN-MAJOR indexing
# Matches day10.el exactly:
#   - index = x + y * rows + 1 (Julia 1-indexed)
mutable struct Mat
    data::Vector{Int}
    rows::Int
    cols::Int
end

function Mat(rows::Int, cols::Int, init::Int=0)
    Mat(fill(init, rows * cols), rows, cols)
end

function inbounds(m::Mat, x::Int, y::Int)
    x >= 0 && y >= 0 && x < m.rows && y < m.cols
end

function index(m::Mat, x::Int, y::Int)
    @assert inbounds(m, x, y) "Index out of bounds: $x, $y"
    # Column-major: index = x + y * rows, then +1 for Julia
    return x + y * m.rows + 1
end

Base.getindex(m::Mat, x::Int, y::Int) = m.data[index(m, x, y)]
Base.setindex!(m::Mat, val, x::Int, y::Int) = (m.data[index(m, x, y)] = val)

# Data structure: RowMajorMat (for day 11)
# Matrix using flat vector with ROW-MAJOR indexing
# Matches day11.el exactly (optimization for row-wise access):
#   - index = x * cols + y + 1 (Julia 1-indexed)
mutable struct RowMajorMat
    data::Vector{Int}
    rows::Int
    cols::Int
end

function RowMajorMat(rows::Int, cols::Int, init::Int=0)
    RowMajorMat(fill(init, rows * cols), rows, cols)
end

function inbounds(m::RowMajorMat, x::Int, y::Int)
    x >= 0 && y >= 0 && x < m.rows && y < m.cols
end

function index(m::RowMajorMat, x::Int, y::Int)
    @assert inbounds(m, x, y) "Index out of bounds: $x, $y"
    # Row-major: index = x * cols + y, then +1 for Julia
    return x * m.cols + y + 1
end

Base.getindex(m::RowMajorMat, x::Int, y::Int) = m.data[index(m, x, y)]
Base.setindex!(m::RowMajorMat, val, x::Int, y::Int) = (m.data[index(m, x, y)] = val)

# Math utilities (for day 10)
function row_gcd(mat::Mat, row_idx::Int)
    """Get GCD of all non-zero values in a row."""
    vals = [abs(mat[row_idx, c]) for c in 0:mat.cols-1 if mat[row_idx, c] != 0]
    if isempty(vals)
        return 1
    end
    return reduce(gcd, vals)
end

function normalize_row!(mat::Mat, row_idx::Int)
    """Divide row by GCD of its coefficients to keep values bounded."""
    g = row_gcd(mat, row_idx)
    if g > 1
        for c in 0:mat.cols-1
            mat[row_idx, c] = mat[row_idx, c] ÷ g
        end
    end
end

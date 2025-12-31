# Day 9: 2D Geometry with Coordinate Compression
# Idiomatic Julia version using native arrays and standard library patterns

include("aoc_common_idiomatic.jl")

function process_input(input_str::String)::Vector{Point}
    """Parse input into list of (x, y) tuples."""
    points = Point[]
    for line in split(strip(input_str), '\n')
        if isempty(line)
            continue
        end
        parts = split(line, ',')
        push!(points, (parse(Int, parts[1]), parse(Int, parts[2])))
    end
    return points
end

function area_between(a::Point, b::Point)::Int
    """Calculate area of rectangle between two points."""
    return (1 + abs(a[1] - b[1])) * (1 + abs(a[2] - b[2]))
end

# Part 1: Brute force all pairs
expect("day9_part1", "50")
function day9_part1(input_str::String)::String
    pts = process_input(input_str)
    n = length(pts)
    best = 0

    for i in 1:n
        for j in i+1:n
            area = area_between(pts[i], pts[j])
            if area > best
                best = area
            end
        end
    end

    return string(best)
end

# Part 2: Coordinate compression and prefix sums
function compress_points(points::Vector{Point})::Tuple{Vector{Point}, Vector{Int}, Vector{Int}}
    """
    Compress coordinates to remove gaps.
    Returns: (compressed_points, x_values, y_values) for uncompression.
    """
    xs = sort(unique([x for (x, y) in points]))
    ys = sort(unique([y for (x, y) in points]))
    x_map = Dict(v => i-1 for (i, v) in enumerate(xs))
    y_map = Dict(v => i-1 for (i, v) in enumerate(ys))

    compressed = [(x_map[x], y_map[y]) for (x, y) in points]
    return compressed, xs, ys
end

function add_padding(cpoints::Vector{Point})::Vector{Point}
    """Add 1 to each coordinate for padding."""
    return [(x + 1, y + 1) for (x, y) in cpoints]
end

function uncompress_point(p::Point, xs::Vector{Int}, ys::Vector{Int})::Point
    """Uncompress a point back to original coordinates."""
    return (xs[p[1] + 1], ys[p[2] + 1])  # +1 for Julia 1-indexing
end

function make_init_grid(cpoints::Vector{Point})::Matrix{Int}
    """
    Create initial grid with compressed points marked.
    Grid uses standard Julia indexing: grid[y+1, x+1] (1-indexed)
    Returns grid with shape (rows, cols) filled with -1, with points marked as 1.
    """
    max_x = maximum(x for (x, y) in cpoints)
    max_y = maximum(y for (x, y) in cpoints)

    # Create grid: -1 = unvisited, 0 = exterior, 1 = interior/edge
    grid = fill(-1, max_y + 2, max_x + 2)

    for (x, y) in cpoints
        grid[y + 1, x + 1] = 1  # +1 for Julia 1-indexing
    end

    return grid
end

function draw_edge!(grid::Matrix{Int}, p1::Point, p2::Point)
    """Draw edge between two points."""
    x1, y1 = p1
    x2, y2 = p2

    if x1 == x2  # vertical line
        y_start, y_end = min(y1, y2), max(y1, y2)
        grid[y_start+1:y_end+1, x1+1] .= 1  # +1 for Julia 1-indexing
    elseif y1 == y2  # horizontal line
        x_start, x_end = min(x1, x2), max(x1, x2)
        grid[y1+1, x_start+1:x_end+1] .= 1  # +1 for Julia 1-indexing
    end
end

function draw_all_edges!(grid::Matrix{Int}, cpoints::Vector{Point})
    """Draw edges connecting all consecutive points in a loop."""
    n = length(cpoints)
    for i in 1:n
        draw_edge!(grid, cpoints[i], cpoints[mod1(i + 1, n)])
    end
end

function flood_fill_exterior!(grid::Matrix{Int})
    """Flood fill from (0,0) to mark all exterior cells as 0."""
    rows, cols = size(grid)
    queue = [(0, 0)]

    while !isempty(queue)
        (x, y) = popfirst!(queue)

        if 0 <= x < cols && 0 <= y < rows && grid[y + 1, x + 1] == -1
            grid[y + 1, x + 1] = 0  # mark as exterior
            push!(queue, (x + 1, y))
            push!(queue, (x - 1, y))
            push!(queue, (x, y + 1))
            push!(queue, (x, y - 1))
        end
    end
end

function fill_rest!(grid::Matrix{Int})
    """Fill remaining -1 cells with 1 (interior)."""
    grid[grid .== -1] .= 1
end

function prefix_sum(grid::Matrix{Int})::Matrix{Int}
    """
    Compute prefix sums along each row (x direction).
    Returns a new array with cumulative sums.
    """
    return cumsum(grid, dims=2)
end

function pair_forms_rect(a::Point, b::Point, prefix_grid::Matrix{Int})::Bool
    """Check if two points form a valid filled rectangle using prefix sums."""
    ax, ay = a
    bx, by = b
    min_x = min(ax, bx)
    max_x = max(ax, bx)
    dx = max_x - min_x
    min_y = min(ay, by)
    max_y = max(ay, by)

    # Check if all rows between min_y and max_y have the expected prefix sum
    for y in min_y:max_y
        expected = dx + 1  # dx+1 cells should all be 1
        if min_x > 0
            actual = prefix_grid[y + 1, max_x + 1] - prefix_grid[y + 1, min_x]  # +1 for Julia 1-indexing
        else
            actual = prefix_grid[y + 1, max_x + 1]  # +1 for Julia 1-indexing
        end

        if actual != expected
            return false
        end
    end

    return true
end

function all_valid_rects(cpoints::Vector{Point}, prefix_grid::Matrix{Int})::Vector{Tuple{Point, Point}}
    """Find all pairs of points that form valid rectangles."""
    rects = Tuple{Point, Point}[]
    n = length(cpoints)

    for i in 1:n
        for j in i+1:n
            if pair_forms_rect(cpoints[i], cpoints[j], prefix_grid)
                push!(rects, (cpoints[i], cpoints[j]))
            end
        end
    end

    return rects
end

function largest_rect(all_rects::Vector{Tuple{Point, Point}}, xs::Vector{Int}, ys::Vector{Int})::Tuple{Point, Point}
    """Find the rectangle with largest area in original coordinates."""
    rects_uncompressed = [
        (uncompress_point((p1[1] - 1, p1[2] - 1), xs, ys),
         uncompress_point((p2[1] - 1, p2[2] - 1), xs, ys))
        for (p1, p2) in all_rects
    ]

    return argmax(pair -> area_between(pair[1], pair[2]), rects_uncompressed)
end

expect("day9_part2", "24")
function day9_part2(input_str::String)::String
    points = process_input(input_str)
    cpoints, xs, ys = compress_points(points)
    cpoints = add_padding(cpoints)
    grid = make_init_grid(cpoints)

    draw_all_edges!(grid, cpoints)
    flood_fill_exterior!(grid)
    fill_rest!(grid)
    prefix_grid = prefix_sum(grid)

    all_rects = all_valid_rects(cpoints, prefix_grid)
    best = largest_rect(all_rects, xs, ys)

    return string(area_between(best[1], best[2]))
end

# Main execution
if abspath(PROGRAM_FILE) == @__FILE__
    # Test on sample input
    sample = read_sample(9)
    println("Testing Part 1 on sample:")
    result1 = day9_part1(sample)
    check_result("day9_part1", result1, "sample")

    println("\nTesting Part 2 on sample:")
    result2 = day9_part2(sample)
    check_result("day9_part2", result2, "sample")

    # Run on real input
    real = read_input(9)
    println("\nRunning Part 1 on real input:")
    println(day9_part1(real))

    println("\nRunning Part 2 on real input:")
    println(day9_part2(real))
end

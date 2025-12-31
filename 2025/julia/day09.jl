# Day 9: 2D Geometry with Coordinate Compression
# Direct port from day09.el maintaining same data structures and algorithms

include("aoc_common.jl")

function process_input(input_str::String)::Vector{Point}
    """Parse input into list of Points."""
    points = Point[]
    for line in split(strip(input_str), '\n')
        if isempty(line)
            continue
        end
        parts = split(line, ',')
        push!(points, Point(parse(Int, parts[1]), parse(Int, parts[2])))
    end
    return points
end

function area_between(a::Point, b::Point)::Int
    """Calculate area of rectangle between two points."""
    return (1 + abs(a.x - b.x)) * (1 + abs(a.y - b.y))
end

# Part 1: Brute force all pairs
expect("day9_part1", "50")
function day9_part1(input_str::String)::String
    pts = process_input(input_str)
    n = length(pts)
    best = 0

    for i in 1:n
        a = pts[i]
        for j in i+1:n
            b = pts[j]
            area = area_between(a, b)
            if area > best
                best = area
            end
        end
    end

    return string(best)
end

# Part 2: Coordinate compression and prefix sums
function compressed_axis(points::Vector{Point}, getter)::Vector{Int}
    """Get sorted unique values along an axis."""
    return sort(unique([getter(p) for p in points]))
end

function make_index_map(sorted_values::Vector{Int})::Dict{Int, Int}
    """Create mapping from value to compressed index."""
    return Dict(v => i-1 for (i, v) in enumerate(sorted_values))
end

function compress_points(points::Vector{Point})::Vector{Point}
    """Compress coordinates to remove gaps."""
    xs = compressed_axis(points, p -> p.x)
    ys = compressed_axis(points, p -> p.y)
    x_map = make_index_map(xs)
    y_map = make_index_map(ys)

    return [Point(x_map[p.x], y_map[p.y]) for p in points]
end

function add_padding(cpoints::Vector{Point})::Vector{Point}
    """Add 1 to each coordinate for padding."""
    return [Point(p.x + 1, p.y + 1) for p in cpoints]
end

function remove_padding(p::Point)::Point
    """Remove padding from a point."""
    return Point(p.x - 1, p.y - 1)
end

function uncompress_point(p::Point, points::Vector{Point})::Point
    """Uncompress a single point back to original coordinates."""
    xs = compressed_axis(points, p -> p.x)
    ys = compressed_axis(points, p -> p.y)
    return Point(xs[p.x + 1], ys[p.y + 1])  # +1 for Julia 1-indexing
end

function make_init_grid(cpoints::Vector{Point})::Vec2D
    """Create initial grid with compressed points marked."""
    max_x = maximum(p.x for p in cpoints)
    max_y = maximum(p.y for p in cpoints)
    v = Vec2D(max_y + 2, max_x + 2, nothing)

    for cp in cpoints
        v[cp] = 1
    end

    return v
end

function draw_edge!(v::Vec2D, p1::Point, p2::Point)
    """Draw edge between two points."""
    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y

    if x1 == x2  # vertical line
        for y in min(y1, y2):max(y1, y2)
            v[Point(x1, y)] = 1
        end
    elseif y1 == y2  # horizontal line
        for x in min(x1, x2):max(x1, x2)
            v[Point(x, y1)] = 1
        end
    end
end

function draw_all_edges!(v::Vec2D, cpoints::Vector{Point})
    """Draw edges connecting all consecutive points in a loop."""
    n = length(cpoints)
    for i in 1:n
        draw_edge!(v, cpoints[i], cpoints[mod1(i + 1, n)])
    end
end

function flood_fill_exterior!(v::Vec2D)
    """Flood fill from (0,0) to mark all exterior cells as 0."""
    queue = [Point(0, 0)]

    while !isempty(queue)
        p = popfirst!(queue)

        if inbounds(v, p) && v[p] === nothing
            v[p] = 0  # mark as exterior
            push!(queue, point_incx(p))
            push!(queue, point_decx(p))
            push!(queue, point_incy(p))
            push!(queue, point_decy(p))
        end
    end
end

function fill_rest!(v::Vec2D)
    """Fill remaining nothing cells with 1 (interior)."""
    for x in 0:v.cols-1
        for y in 0:v.rows-1
            p = Point(x, y)
            if v[p] === nothing
                v[p] = 1
            end
        end
    end
end

function prefix_sum!(v::Vec2D)
    """Compute prefix sums along each row (x direction)."""
    for y in 0:v.rows-1
        s = 0
        for x in 0:v.cols-1
            p = Point(x, y)
            s += v[p]
            v[p] = s
        end
    end
end

function pair_forms_rect(a::Point, b::Point, v::Vec2D)::Bool
    """Check if two points form a valid filled rectangle."""
    ax, ay = a.x, a.y
    bx, by = b.x, b.y
    min_x = min(ax, bx)
    max_x = max(ax, bx)
    dx = max_x - min_x
    min_y = min(ay, by)
    max_y = max(ay, by)

    # Check if all rows between min_y and max_y have the expected prefix sum
    for y in min_y:max_y
        p_max = Point(max_x, y)
        p_before_min = min_x > 0 ? Point(min_x - 1, y) : nothing

        expected = dx + 1  # dx+1 cells should all be 1
        if p_before_min === nothing || !inbounds(v, p_before_min)
            actual = v[p_max]
        else
            actual = v[p_max] - v[p_before_min]
        end

        if actual != expected
            return false
        end
    end

    return true
end

function all_valid_rects(cpoints::Vector{Point}, v::Vec2D)::Vector{Tuple{Point, Point}}
    """Find all pairs of points that form valid rectangles."""
    rects = Tuple{Point, Point}[]
    n = length(cpoints)

    for i in 1:n
        for j in i+1:n
            if pair_forms_rect(cpoints[i], cpoints[j], v)
                push!(rects, (cpoints[i], cpoints[j]))
            end
        end
    end

    return rects
end

function largest_rect(all_rects::Vector{Tuple{Point, Point}}, points::Vector{Point})::Tuple{Point, Point}
    """Find the rectangle with largest area in original coordinates."""
    rects_uncompressed = [
        (uncompress_point(remove_padding(pair[1]), points),
         uncompress_point(remove_padding(pair[2]), points))
        for pair in all_rects
    ]

    return argmax(pair -> area_between(pair[1], pair[2]), rects_uncompressed)
end

expect("day9_part2", "24")
function day9_part2(input_str::String)::String
    points = process_input(input_str)
    cpoints = add_padding(compress_points(points))
    v = make_init_grid(cpoints)

    draw_all_edges!(v, cpoints)
    flood_fill_exterior!(v)
    fill_rest!(v)
    prefix_sum!(v)

    all_rects = all_valid_rects(cpoints, v)
    best = largest_rect(all_rects, points)

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

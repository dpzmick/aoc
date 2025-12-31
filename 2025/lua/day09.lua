-- Day 9: 2D Geometry with Coordinate Compression
-- Port from day09.py maintaining same algorithms

local aoc = require("aoc_common")
local Point = aoc.Point
local Vec2D = aoc.Vec2D

local M = {}

-- Parse input into list of Points (using 0-based coordinates like Python)
local function process_input(input_str)
    local points = {}
    for line in input_str:gmatch("[^\r\n]+") do
        if line ~= "" then
            local x, y = line:match("^(-?%d+),(-?%d+)$")
            if x and y then
                table.insert(points, Point.new(tonumber(x), tonumber(y)))
            end
        end
    end
    return points
end

-- Calculate area of rectangle between two points
local function area_between(a, b)
    return (1 + math.abs(a.x - b.x)) * (1 + math.abs(a.y - b.y))
end

-- Part 1: Brute force all pairs
function M.day9_part1(input_str)
    local pts = process_input(input_str)
    local n = #pts
    local best = 0

    for i = 1, n do
        local a = pts[i]
        for j = i + 1, n do
            local b = pts[j]
            local area = area_between(a, b)
            if area > best then
                best = area
            end
        end
    end

    return tostring(best)
end

-- Part 2: Coordinate compression and prefix sums

-- Get sorted unique values along an axis
local function compressed_axis(points, getter)
    local values = {}
    local seen = {}
    for _, p in ipairs(points) do
        local v = getter(p)
        if not seen[v] then
            seen[v] = true
            table.insert(values, v)
        end
    end
    table.sort(values)
    return values
end

-- Create mapping from value to compressed index (0-based)
local function make_index_map(sorted_values)
    local map = {}
    for i, v in ipairs(sorted_values) do
        map[v] = i - 1  -- 0-based indexing for Vec2D
    end
    return map
end

-- Compress coordinates to remove gaps
local function compress_points(points)
    local xs = compressed_axis(points, function(p) return p.x end)
    local ys = compressed_axis(points, function(p) return p.y end)
    local x_map = make_index_map(xs)
    local y_map = make_index_map(ys)

    local compressed = {}
    for _, p in ipairs(points) do
        table.insert(compressed, Point.new(x_map[p.x], y_map[p.y]))
    end
    return compressed
end

-- Add 1 to each coordinate for padding
local function add_padding(cpoints)
    local padded = {}
    for _, p in ipairs(cpoints) do
        table.insert(padded, Point.new(p.x + 1, p.y + 1))
    end
    return padded
end

-- Remove padding from a point
local function remove_padding(p)
    return Point.new(p.x - 1, p.y - 1)
end

-- Uncompress a single point back to original coordinates
local function uncompress_point(p, points)
    local xs = compressed_axis(points, function(p) return p.x end)
    local ys = compressed_axis(points, function(p) return p.y end)
    return Point.new(xs[p.x + 1], ys[p.y + 1])  -- Convert back to 1-based for lookup
end

-- Create initial grid with compressed points marked
-- Use -1 as sentinel for unvisited cells
local function make_init_grid(cpoints)
    local max_x = -math.huge
    local max_y = -math.huge
    for _, p in ipairs(cpoints) do
        if p.x > max_x then max_x = p.x end
        if p.y > max_y then max_y = p.y end
    end

    local v = Vec2D.new(max_y + 2, max_x + 2, -1)

    for _, cp in ipairs(cpoints) do
        v:set(cp, 1)
    end

    return v
end

-- Draw edge between two points
local function draw_edge(v, p1, p2)
    local x1, y1 = p1.x, p1.y
    local x2, y2 = p2.x, p2.y

    if x1 == x2 then  -- vertical line
        local min_y = math.min(y1, y2)
        local max_y = math.max(y1, y2)
        for y = min_y, max_y do
            v:set(Point.new(x1, y), 1)
        end
    elseif y1 == y2 then  -- horizontal line
        local min_x = math.min(x1, x2)
        local max_x = math.max(x1, x2)
        for x = min_x, max_x do
            v:set(Point.new(x, y1), 1)
        end
    end
end

-- Draw edges connecting all consecutive points in a loop
local function draw_all_edges(v, cpoints)
    local n = #cpoints
    for i = 1, n do
        local next_i = (i % n) + 1
        draw_edge(v, cpoints[i], cpoints[next_i])
    end
end

-- Flood fill from (0,0) to mark all exterior cells as 0
local function flood_fill_exterior(v)
    local queue = {Point.new(0, 0)}
    local head = 1

    while head <= #queue do
        local p = queue[head]
        head = head + 1

        if v:inbounds(p) and v:ref(p) == -1 then
            v:set(p, 0)  -- mark as exterior
            table.insert(queue, aoc.point_incx(p))
            table.insert(queue, aoc.point_decx(p))
            table.insert(queue, aoc.point_incy(p))
            table.insert(queue, aoc.point_decy(p))
        end
    end
end

-- Fill remaining -1 cells with 1 (interior)
local function fill_rest(v)
    for x = 0, v.cols - 1 do
        for y = 0, v.rows - 1 do
            local p = Point.new(x, y)
            if v:ref(p) == -1 then
                v:set(p, 1)
            end
        end
    end
end

-- Compute prefix sums along each row (x direction)
local function prefix_sum(v)
    for y = 0, v.rows - 1 do
        local s = 0
        for x = 0, v.cols - 1 do
            local p = Point.new(x, y)
            s = s + v:ref(p)
            v:set(p, s)
        end
    end
end

-- Check if two points form a valid filled rectangle
local function pair_forms_rect(a, b, v)
    local ax, ay = a.x, a.y
    local bx, by = b.x, b.y
    local min_x = math.min(ax, bx)
    local max_x = math.max(ax, bx)
    local dx = max_x - min_x
    local min_y = math.min(ay, by)
    local max_y = math.max(ay, by)

    -- Check if all rows between min_y and max_y have the expected prefix sum
    for y = min_y, max_y do
        local p_max = Point.new(max_x, y)
        local p_before_min = min_x > 0 and Point.new(min_x - 1, y) or nil

        local expected = dx + 1  -- dx+1 cells should all be 1
        local actual
        if p_before_min == nil or not v:inbounds(p_before_min) then
            actual = v:ref(p_max)
        else
            actual = v:ref(p_max) - v:ref(p_before_min)
        end

        if actual ~= expected then
            return false
        end
    end

    return true
end

-- Find all pairs of points that form valid rectangles
local function all_valid_rects(cpoints, v)
    local rects = {}
    local n = #cpoints

    for i = 1, n do
        for j = i + 1, n do
            if pair_forms_rect(cpoints[i], cpoints[j], v) then
                table.insert(rects, {cpoints[i], cpoints[j]})
            end
        end
    end

    return rects
end

-- Find the rectangle with largest area in original coordinates
local function largest_rect(all_rects, points)
    local rects_uncompressed = {}
    for _, pair in ipairs(all_rects) do
        local p1 = uncompress_point(remove_padding(pair[1]), points)
        local p2 = uncompress_point(remove_padding(pair[2]), points)
        table.insert(rects_uncompressed, {p1, p2})
    end

    local best = rects_uncompressed[1]
    local best_area = area_between(best[1], best[2])

    for _, pair in ipairs(rects_uncompressed) do
        local area = area_between(pair[1], pair[2])
        if area > best_area then
            best = pair
            best_area = area
        end
    end

    return best
end

function M.day9_part2(input_str)
    local points = process_input(input_str)
    local cpoints = add_padding(compress_points(points))
    local v = make_init_grid(cpoints)

    draw_all_edges(v, cpoints)
    flood_fill_exterior(v)
    fill_rest(v)
    prefix_sum(v)

    local all_rects = all_valid_rects(cpoints, v)
    local best = largest_rect(all_rects, points)

    return tostring(area_between(best[1], best[2]))
end

return M

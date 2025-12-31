-- Common utilities and data structures for AOC solutions
-- All indexing is 1-based (Lua native)

local M = {}

-- Get project root directory
local function get_project_root()
    -- Get the directory containing this script
    local script_path = debug.getinfo(1, "S").source:sub(2)
    local lua_dir = script_path:match("(.*/)")
    if not lua_dir then
        lua_dir = "./"
    end
    -- Project root is parent of lua/ directory
    return lua_dir .. "../"
end

M.project_root = get_project_root()

-- Read input file for a given day
function M.read_input(day)
    local path = string.format("%sinputs/day%02d.txt", M.project_root, day)
    local f = assert(io.open(path, "r"), "Failed to open " .. path)
    local content = f:read("*all")
    f:close()
    -- Trim whitespace
    return content:match("^%s*(.-)%s*$")
end

-- GCD function (for day10)
function M.gcd(a, b)
    a, b = math.abs(a), math.abs(b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return a
end

-- Get GCD of all non-zero values in a matrix row (for day10)
function M.row_gcd(mat, row_idx)
    local vals = {}
    for c = 1, mat.cols do
        local v = mat:ref(row_idx, c)
        if v ~= 0 then
            table.insert(vals, math.abs(v))
        end
    end
    if #vals == 0 then
        return 1
    end

    local result = vals[1]
    for i = 2, #vals do
        result = M.gcd(result, vals[i])
    end
    return result
end

-- Divide row by GCD to keep values bounded (for day10)
function M.normalize_row(mat, row_idx)
    local g = M.row_gcd(mat, row_idx)
    if g > 1 then
        for c = 1, mat.cols do
            mat:set(row_idx, c, math.floor(mat:ref(row_idx, c) / g))
        end
    end
end

-- Point class (for day09)
-- Note: Using 1-based indexing throughout
M.Point = {}
M.Point.__index = M.Point

function M.Point.new(x, y)
    return setmetatable({x = x, y = y}, M.Point)
end

function M.Point:__tostring()
    return string.format("(%d, %d)", self.x, self.y)
end

function M.Point:__eq(other)
    return self.x == other.x and self.y == other.y
end

-- Helper functions for point movement (1-based coordinates)
function M.point_incx(p)
    return M.Point.new(p.x + 1, p.y)
end

function M.point_decx(p)
    return M.Point.new(p.x - 1, p.y)
end

function M.point_incy(p)
    return M.Point.new(p.x, p.y + 1)
end

function M.point_decy(p)
    return M.Point.new(p.x, p.y - 1)
end

-- Vec2D class (row-major flat array for day09)
-- Uses 0-based coordinates (like Python) but stores in 1-based Lua array
M.Vec2D = {}
M.Vec2D.__index = M.Vec2D

function M.Vec2D.new(rows, cols, initial)
    local obj = {
        rows = rows,
        cols = cols,
        data = {}
    }
    initial = initial or 0
    for i = 1, rows * cols do
        obj.data[i] = initial
    end
    return setmetatable(obj, M.Vec2D)
end

function M.Vec2D:inbounds(p)
    -- 0-based coordinates
    return p.x >= 0 and p.y >= 0 and p.x < self.cols and p.y < self.rows
end

function M.Vec2D:index(p)
    -- Convert 0-based (y, x) to 1-based flat array index
    -- Row-major: y * cols + x + 1
    return p.y * self.cols + p.x + 1
end

function M.Vec2D:ref(p)
    if not self:inbounds(p) then
        error(string.format("Point (%d,%d) out of bounds (rows=%d, cols=%d)", p.x, p.y, self.rows, self.cols))
    end
    return self.data[self:index(p)]
end

function M.Vec2D:set(p, value)
    if not self:inbounds(p) then
        error(string.format("Point (%d,%d) out of bounds (rows=%d, cols=%d)", p.x, p.y, self.rows, self.cols))
    end
    self.data[self:index(p)] = value
end

-- Mat class (column-major flat array for day10/day11)
-- Indexing: index = x + (y - 1) * rows  (1-based coordinates)
M.Mat = {}
M.Mat.__index = M.Mat

function M.Mat.new(rows, cols, initial)
    local obj = {
        rows = rows,
        cols = cols,
        data = {}
    }
    initial = initial or 0
    for i = 1, rows * cols do
        obj.data[i] = initial
    end
    return setmetatable(obj, M.Mat)
end

function M.Mat:index(x, y)
    -- Column-major: x + (y - 1) * rows
    -- x is row index (1..rows), y is column index (1..cols)
    return x + (y - 1) * self.rows
end

function M.Mat:ref(x, y)
    if x < 1 or x > self.rows or y < 1 or y > self.cols then
        error(string.format("Index (%d, %d) out of bounds (rows=%d, cols=%d)", x, y, self.rows, self.cols))
    end
    return self.data[self:index(x, y)]
end

function M.Mat:set(x, y, value)
    if x < 1 or x > self.rows or y < 1 or y > self.cols then
        error(string.format("Index (%d, %d) out of bounds (rows=%d, cols=%d)", x, y, self.rows, self.cols))
    end
    self.data[self:index(x, y)] = value
end

return M

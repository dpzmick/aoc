-- Day 10: Linear Algebra with Integer-only Gaussian Elimination
-- Port from day10.py maintaining same algorithms

local aoc = require("aoc_common")
local Mat = aoc.Mat
local bit = require("bit")

local M = {}

-- Part 1: BFS through state space with bit operations

local D10Line = {}
D10Line.__index = D10Line

function D10Line.new(length, goal, toggles, joltage_goal)
    return setmetatable({
        length = length,
        goal = goal,
        toggles = toggles,
        joltage_goal = joltage_goal
    }, D10Line)
end

-- Parse goal string into bitmask
local function parse_goal(goal_str)
    local result = 0
    for i = 1, #goal_str do
        local c = goal_str:sub(i, i)
        if c == '#' then
            result = bit.bor(result, bit.lshift(1, i - 1))
        end
    end
    return result
end

-- Parse toggle positions into bitmask
local function parse_toggle(positions)
    local result = 0
    for _, pos in ipairs(positions) do
        result = bit.bor(result, bit.lshift(1, pos))
    end
    return result
end

-- Parse all toggles from string
local function parse_toggles(toggles_str)
    local toggles = {}
    for match in toggles_str:gmatch("%(([0-9,]+)%)") do
        local positions = {}
        for num in match:gmatch("(%d+)") do
            table.insert(positions, tonumber(num))
        end
        table.insert(toggles, parse_toggle(positions))
    end
    return toggles
end

-- Extract goal, toggles, and joltage from line
local function extract_line_parts(line)
    local goal_str, toggles_str, jolts_str = line:match("^%[([.#]+)%]%s*(.-)%s*{([0-9,]+)}$")
    if not goal_str then
        error("Failed to parse line: " .. line)
    end
    return goal_str, toggles_str, jolts_str
end

-- Parse a line into D10Line structure
local function parse_line(line)
    local goal_str, toggles_str, jolts_str = extract_line_parts(line)
    local joltage_goal = {}
    for num in jolts_str:gmatch("(%d+)") do
        table.insert(joltage_goal, tonumber(num))
    end

    return D10Line.new(
        #goal_str,
        parse_goal(goal_str),
        parse_toggles(toggles_str),
        joltage_goal
    )
end

-- Parse all lines
local function process_input(input_str)
    local lines = {}
    for line in input_str:gmatch("[^\r\n]+") do
        if line ~= "" then
            table.insert(lines, parse_line(line))
        end
    end
    return lines
end

-- Apply toggle using XOR
local function apply_toggle(state, toggle)
    return bit.bxor(state, toggle)
end

-- Solve using BFS through state space
local function solve_line_bfs(line)
    local queue = {{node = 0, dist = 0}}
    local visited = {}
    local goal = line.goal
    local toggles = line.toggles

    while #queue > 0 do
        local current = table.remove(queue, 1)
        local node, dist = current.node, current.dist

        if node == goal then
            return dist
        end

        if not visited[node] then
            visited[node] = true
            for _, toggle in ipairs(toggles) do
                local next_node = apply_toggle(node, toggle)
                table.insert(queue, {node = next_node, dist = dist + 1})
            end
        end
    end

    return -1  -- No solution found
end

function M.day10_part1(input_str)
    local lines = process_input(input_str)
    local total = 0
    for _, line in ipairs(lines) do
        total = total + solve_line_bfs(line)
    end
    return tostring(total)
end

-- Part 2: Linear algebra with integer-only Gaussian elimination

local D10LineP2 = {}
D10LineP2.__index = D10LineP2

function D10LineP2.new(toggles, joltage_goal)
    return setmetatable({
        toggles = toggles,
        joltage_goal = joltage_goal
    }, D10LineP2)
end

-- Parse toggles as lists of positions (not bitmasks)
local function parse_toggles_p2(toggles_str)
    local toggles = {}
    for match in toggles_str:gmatch("%(([0-9,]+)%)") do
        local positions = {}
        for num in match:gmatch("(%d+)") do
            table.insert(positions, tonumber(num) + 1)  -- Convert to 1-based
        end
        table.insert(toggles, positions)
    end
    return toggles
end

-- Parse line for part 2
local function parse_line_p2(line)
    local _, toggles_str, jolts_str = extract_line_parts(line)
    local joltage_goal = {}
    for num in jolts_str:gmatch("(%d+)") do
        table.insert(joltage_goal, tonumber(num))
    end

    return D10LineP2.new(
        parse_toggles_p2(toggles_str),
        joltage_goal
    )
end

-- Create augmented matrix A|b for system Ax=b
local function make_toggles_mat(toggles, jolts)
    local num_counters = #jolts
    local num_buttons = #toggles
    local rows = num_counters
    local cols = num_buttons + 1  -- +1 for RHS

    local m = Mat.new(rows, cols, 0)

    -- For each button, mark which counters it affects
    for button_idx = 1, num_buttons do
        for _, counter_idx in ipairs(toggles[button_idx]) do
            m:set(counter_idx, button_idx, 1)
        end
    end

    -- RHS column (target joltages)
    for counter_idx = 1, num_counters do
        m:set(counter_idx, num_buttons + 1, jolts[counter_idx])
    end

    return m
end

-- Matrix operations for Gaussian elimination

local function swap_rows(m, r1, r2)
    if r1 == r2 then return end
    for c = 1, m.cols do
        local tmp = m:ref(r1, c)
        m:set(r1, c, m:ref(r2, c))
        m:set(r2, c, tmp)
    end
end

local function add_rows(m, r1, r2, factor1, factor2)
    for c = 1, m.cols do
        local r1_val = m:ref(r1, c)
        local r2_val = m:ref(r2, c)
        m:set(r2, c, factor1 * r1_val + factor2 * r2_val)
    end
end

local function nonzero_row_idx_for_col(m, c, st)
    st = st or 1
    for r = st, m.rows do
        if m:ref(r, c) ~= 0 then
            return r
        end
    end
    return nil
end

local function eliminate_down(m, pivot_r, c)
    for r = pivot_r + 1, m.rows do
        local factor1 = m:ref(r, c)
        local factor2 = -m:ref(pivot_r, c)
        add_rows(m, pivot_r, r, factor1, factor2)
        aoc.normalize_row(m, r)
    end
end

local function eliminate_up(m, pivot_r, c)
    for r = 1, pivot_r - 1 do
        local target_val = m:ref(r, c)
        local pivot_val = m:ref(pivot_r, c)
        if target_val ~= 0 then
            add_rows(m, pivot_r, r, -target_val, pivot_val)
            aoc.normalize_row(m, r)
        end
    end
end

local function eliminate(m)
    local pivot_row = 1
    local pivot_cols = {}

    -- Forward elimination (create upper triangular)
    for c = 1, m.cols do
        local tgt_r = nonzero_row_idx_for_col(m, c, pivot_row)
        if tgt_r then
            swap_rows(m, pivot_row, tgt_r)
            eliminate_down(m, pivot_row, c)
            table.insert(pivot_cols, {pivot_row, c})
            pivot_row = pivot_row + 1
        end
    end

    -- Back substitution (reduced row echelon form)
    for _, pair in ipairs(pivot_cols) do
        local pr, pc = pair[1], pair[2]
        eliminate_up(m, pr, pc)
    end

    return pivot_cols
end

-- Helper functions for solving the system

local function rhs_col(m)
    return m.cols
end

local function num_vars(m)
    return m.cols - 1
end

local function pivot_col_set(pivot_cols)
    local set = {}
    for _, pair in ipairs(pivot_cols) do
        set[pair[2]] = true
    end
    return set
end

local function free_var_cols(m, pivot_cols)
    local pcols = pivot_col_set(pivot_cols)
    local free_cols = {}
    for c = 1, num_vars(m) do
        if not pcols[c] then
            table.insert(free_cols, c)
        end
    end
    return free_cols
end

local function pivot_row_effective_rhs(m, pivot_row, free_vals)
    local adjustment = 0
    for _, pair in ipairs(free_vals) do
        local fc, fv = pair[1], pair[2]
        adjustment = adjustment + m:ref(pivot_row, fc) * fv
    end
    return m:ref(pivot_row, rhs_col(m)) - adjustment
end

local function solve_pivot_var(m, pivot_row, pivot_col, free_vals)
    local eff_rhs = pivot_row_effective_rhs(m, pivot_row, free_vals)
    local pivot_val = m:ref(pivot_row, pivot_col)
    return math.floor(eff_rhs / pivot_val)
end

local function solve_system(m, pivot_cols, free_vals)
    local solution = {}
    for _, pair in ipairs(free_vals) do
        table.insert(solution, pair)
    end
    for _, pair in ipairs(pivot_cols) do
        local pr, pc = pair[1], pair[2]
        table.insert(solution, {pc, solve_pivot_var(m, pr, pc, free_vals)})
    end
    return solution
end

local function all_combinations(ranges)
    if #ranges == 0 then
        return {{}}
    end

    local result = {{}}
    for _, r in ipairs(ranges) do
        local new_result = {}
        for i = 0, r do
            for _, combo in ipairs(result) do
                local new_combo = {}
                for _, v in ipairs(combo) do
                    table.insert(new_combo, v)
                end
                table.insert(new_combo, i)
                table.insert(new_result, new_combo)
            end
        end
        result = new_result
    end

    return result
end

local function get_button_max(button_idx, toggles, jolts)
    local affected_counters = toggles[button_idx]
    if #affected_counters > 0 then
        local min_jolt = math.huge
        for _, c in ipairs(affected_counters) do
            if jolts[c] < min_jolt then
                min_jolt = jolts[c]
            end
        end
        return min_jolt
    else
        local max_jolt = 0
        for _, j in ipairs(jolts) do
            if j > max_jolt then
                max_jolt = j
            end
        end
        return max_jolt
    end
end

local function get_maxes(free_cols, toggles, jolts)
    local maxes = {}
    for _, fc in ipairs(free_cols) do
        table.insert(maxes, get_button_max(fc, toggles, jolts))
    end
    return maxes
end

local function solution_sum(solution)
    local total = 0
    for _, pair in ipairs(solution) do
        total = total + pair[2]
    end
    return total
end

local function solution_non_negative_p(sol)
    for _, pair in ipairs(sol) do
        if pair[2] < 0 then
            return false
        end
    end
    return true
end

local function integer_solution_p(m, pivot_cols, free_vals)
    for _, pair in ipairs(pivot_cols) do
        local pr, pc = pair[1], pair[2]
        local pivot_val = m:ref(pr, pc)
        local eff_rhs = pivot_row_effective_rhs(m, pr, free_vals)
        if eff_rhs % math.abs(pivot_val) ~= 0 then
            return false
        end
    end
    return true
end

local function solve_line_p2(line)
    local toggles = line.toggles
    local jolts = line.joltage_goal
    local m = make_toggles_mat(toggles, jolts)
    local pivots = eliminate(m)
    local fvc = free_var_cols(m, pivots)
    local fv_maxes = get_maxes(fvc, toggles, jolts)
    local all_fv_values = all_combinations(fv_maxes)

    local best = nil
    for _, comb in ipairs(all_fv_values) do
        local fv_vals = {}
        for i, fc in ipairs(fvc) do
            table.insert(fv_vals, {fc, comb[i]})
        end

        if integer_solution_p(m, pivots, fv_vals) then
            local sol = solve_system(m, pivots, fv_vals)
            if solution_non_negative_p(sol) then
                local total = solution_sum(sol)
                if best == nil or total < best then
                    best = total
                end
            end
        end
    end

    return best or -1
end

function M.day10_part2(input_str)
    local lines = {}
    for line in input_str:gmatch("[^\r\n]+") do
        if line ~= "" then
            table.insert(lines, parse_line_p2(line))
        end
    end

    local total = 0
    for _, line in ipairs(lines) do
        total = total + solve_line_p2(line)
    end
    return tostring(total)
end

return M

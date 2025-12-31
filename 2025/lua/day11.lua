-- Day 11: Graph Path Counting with Memoized DFS
-- Port from day11.py maintaining same algorithms

local aoc = require("aoc_common")
local Mat = aoc.Mat

local M = {}

-- Parse line into source and destinations
local function parse_line(line)
    local src, dsts_str = line:match("^([^:]+):%s*(.+)$")
    if not src then return nil end

    local dsts = {}
    for dst in dsts_str:gmatch("%S+") do
        table.insert(dsts, dst)
    end

    return src, dsts
end

-- Create mapping from node names to IDs (1-based)
local function make_node_table(input_str)
    local node_map = {}
    local names = {}  -- Keep track of order for counting

    for line in input_str:gmatch("[^\r\n]+") do
        if line ~= "" then
            local src, dsts = parse_line(line)
            if src then
                if not node_map[src] then
                    table.insert(names, src)
                    node_map[src] = #names
                end
                for _, dst in ipairs(dsts) do
                    if not node_map[dst] then
                        table.insert(names, dst)
                        node_map[dst] = #names
                    end
                end
            end
        end
    end

    return node_map, #names
end

-- Fill adjacency matrix for one line
local function fill_mat_line(line, adj, node_map)
    local src, dsts = parse_line(line)
    if not src then return end

    local row = node_map[src]
    for _, dst in ipairs(dsts) do
        local col = node_map[dst]
        adj:set(row, col, 1)
    end
end

-- Parse input into node table and adjacency matrix
local function process_input(input_str)
    local node_map, n = make_node_table(input_str)
    local adj = Mat.new(n, n, 0)

    for line in input_str:gmatch("[^\r\n]+") do
        if line ~= "" then
            fill_mat_line(line, adj, node_map)
        end
    end

    return node_map, adj
end

-- Part 1: Simple DFS to count all paths
local function dfs(from_node, to_node, adj)
    if from_node == to_node then
        return 1
    end

    -- Sum paths from all children
    local total = 0
    for i = 1, adj.cols do
        if adj:ref(from_node, i) == 1 then
            total = total + dfs(i, to_node, adj)
        end
    end

    return total
end

function M.day11_part1(input_str)
    local node_map, adj = process_input(input_str)
    local you = node_map['you']
    local out = node_map['out']
    return tostring(dfs(you, out, adj))
end

-- Part 2: DFS with memoization tracking specific nodes visited
local function dfs_p2(from_node, to_node, dac_id, fft_id, adj)
    local memo = {}

    local function dfs_inner(from_node, to_node, seen_dac, seen_fft)
        -- Update seen flags
        seen_dac = seen_dac or (from_node == dac_id)
        seen_fft = seen_fft or (from_node == fft_id)

        -- Create memo key (encode as single number)
        local key = from_node * 4 + (seen_dac and 2 or 0) + (seen_fft and 1 or 0)
        if memo[key] then
            return memo[key]
        end

        -- Base case
        local result
        if from_node == to_node then
            result = (seen_dac and seen_fft) and 1 or 0
        else
            -- Recursive case: sum paths from children
            result = 0
            for i = 1, adj.cols do
                if adj:ref(from_node, i) == 1 then
                    result = result + dfs_inner(i, to_node, seen_dac, seen_fft)
                end
            end
        end

        memo[key] = result
        return result
    end

    return dfs_inner(from_node, to_node, false, false)
end

function M.day11_part2(input_str)
    local node_map, adj = process_input(input_str)
    local svr = node_map['svr']
    local out = node_map['out']
    local dac = node_map['dac']
    local fft = node_map['fft']
    return tostring(dfs_p2(svr, out, dac, fft, adj))
end

return M

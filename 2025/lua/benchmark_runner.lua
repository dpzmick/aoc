-- Benchmark runner for LuaJIT AOC solutions
-- Runs solutions multiple times and outputs statistics to JSON

local socket = require("socket")
local json = require("cjson")
local aoc = require("aoc_common")

-- Benchmark configuration
local WARMUP = 5
local ITERATIONS = 20
local SLOW_THRESHOLD_MS = 1000.0
local MIN_ITERATIONS = 3

-- High-resolution timer
local function time_ms()
    return socket.gettime() * 1000
end

-- Statistics functions
local function mean(times)
    local sum = 0
    for _, t in ipairs(times) do
        sum = sum + t
    end
    return sum / #times
end

local function median(times)
    local sorted = {}
    for _, t in ipairs(times) do
        table.insert(sorted, t)
    end
    table.sort(sorted)
    return sorted[math.ceil(#sorted / 2)]
end

local function std_dev(times)
    if #times <= 1 then return 0 end

    local m = mean(times)
    local sum_sq = 0
    for _, t in ipairs(times) do
        sum_sq = sum_sq + (t - m) ^ 2
    end
    return math.sqrt(sum_sq / #times)
end

local function min_val(times)
    local m = times[1]
    for _, t in ipairs(times) do
        if t < m then m = t end
    end
    return m
end

local function max_val(times)
    local m = times[1]
    for _, t in ipairs(times) do
        if t > m then m = t end
    end
    return m
end

-- Benchmark a single function with warmup and timed runs
local function benchmark_function(func, input_data, warmup, iterations)
    local times = {}

    -- Warmup with adaptive iteration reduction
    for i = 1, warmup do
        local start = time_ms()
        func(input_data)
        local elapsed = time_ms() - start

        if i == 1 and elapsed > SLOW_THRESHOLD_MS then
            iterations = MIN_ITERATIONS
            io.stderr:write(string.format(
                "  First warmup took %.2fms, reducing iterations to %d\n",
                elapsed, iterations))
            break
        end
    end

    -- Timed runs
    local result
    for i = 1, iterations do
        local start = time_ms()
        result = func(input_data)
        local elapsed = time_ms() - start
        table.insert(times, elapsed)

        if i == 1 and elapsed > SLOW_THRESHOLD_MS and iterations > MIN_ITERATIONS then
            iterations = MIN_ITERATIONS
            io.stderr:write(string.format(
                "  First iteration took %.2fms, reducing remaining to %d\n",
                elapsed, iterations - 1))
        end
    end

    return times, result
end

-- Benchmark both parts of a day
local function benchmark_day(day, warmup, iterations)
    local module = require(string.format("day%02d", day))
    local input = aoc.read_input(day)
    local results = {}

    for _, part in ipairs({1, 2}) do
        local func_name = string.format("day%d_part%d", day, part)
        local func = module[func_name]

        if func then
            local times, result = benchmark_function(func, input, warmup, iterations)

            table.insert(results, {
                day = day,
                part = part,
                mean_ms = mean(times),
                median_ms = median(times),
                std_dev_ms = std_dev(times),
                min_ms = min_val(times),
                max_ms = max_val(times),
                iterations = #times,
                result = result
            })
        end
    end

    return results
end

-- Run benchmarks for specified days
local function run_all_benchmarks(days, warmup, iterations)
    local all_results = {}

    for _, day in ipairs(days) do
        io.stderr:write(string.format("Benchmarking day %02d...\n", day))
        local success, results = pcall(benchmark_day, day, warmup, iterations)

        if success then
            for _, r in ipairs(results) do
                table.insert(all_results, r)
                io.stderr:write(string.format(
                    "  Part %d: %.2fms (±%.2fms)\n",
                    r.part, r.mean_ms, r.std_dev_ms))
            end
        else
            io.stderr:write(string.format("  Error: %s\n", results))
        end
    end

    return all_results
end

-- Save benchmark results to JSON
local function save_results(results, filename)
    local f = assert(io.open(filename, "w"))
    f:write(json.encode(results))
    f:close()
end

-- Main execution
local function main()
    local days = {9, 10, 11}
    local warmup = WARMUP
    local iterations = ITERATIONS

    io.stderr:write(string.format(
        "Running LuaJIT benchmarks (%d warmup, %d timed iterations)...\n",
        warmup, iterations))

    local results = run_all_benchmarks(days, warmup, iterations)

    local output_file = "lua_results.json"
    save_results(results, output_file)

    io.stderr:write(string.format("\nResults saved to %s\n", output_file))
    io.stderr:write(string.format("Benchmarked %d solutions\n", #results))
end

-- Run if executed as script
if not pcall(debug.getlocal, 4, 1) then
    main()
end

return {
    benchmark_day = benchmark_day,
    run_all_benchmarks = run_all_benchmarks
}

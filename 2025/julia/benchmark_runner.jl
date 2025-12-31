# Benchmark runner for Julia AOC solutions
# Outputs JSON results matching the format expected by comparison scripts

using JSON
using Statistics

# Configuration
const WARMUP_ITERATIONS = 5
const DEFAULT_ITERATIONS = 20
const SLOW_THRESHOLD_MS = 1000
const SLOW_ITERATIONS = 3

# Get input file path
function get_input_path(day::Int)
    return joinpath(dirname(@__DIR__), "inputs", "day$(lpad(day, 2, '0')).txt")
end

function read_input(day::Int)
    return strip(read(get_input_path(day), String))
end

# Benchmark a single function
function benchmark_function(func, input_data; warmup=WARMUP_ITERATIONS, iterations=DEFAULT_ITERATIONS)
    """Run benchmark with warmup and collect timing statistics."""

    # Warmup phase
    first_time = 0.0
    for i in 1:warmup
        t = @elapsed func(input_data)
        if i == 1
            first_time = t * 1000  # Convert to ms
        end
    end

    # Adapt iterations if first run was slow
    if first_time > SLOW_THRESHOLD_MS
        iterations = SLOW_ITERATIONS
        println("  First iteration took $(round(first_time, digits=2))ms, reducing to $iterations iterations")
    end

    # Timed phase
    times_ms = Float64[]
    result = nothing
    for i in 1:iterations
        elapsed = @elapsed result = func(input_data)
        push!(times_ms, elapsed * 1000)  # Convert to ms
    end

    # Calculate statistics
    stats = Dict(
        "mean_ms" => mean(times_ms),
        "median_ms" => median(times_ms),
        "std_dev_ms" => std(times_ms),
        "min_ms" => minimum(times_ms),
        "max_ms" => maximum(times_ms),
        "iterations" => iterations,
        "result" => string(result)
    )

    return stats
end

# Benchmark a single day implementation
function benchmark_day_implementation(day::Int, impl_file::String, impl_name::String)
    """Benchmark both parts of a day's implementation."""
    println("Benchmarking Day $day ($impl_name)...")

    # Include the implementation file
    try
        include(impl_file)
    catch e
        println("  Error loading $impl_file: $e")
        return []
    end

    # Get input
    input_data = read_input(day)

    results = []

    # Benchmark part 1
    part1_func = Symbol("day$(day)_part1")
    if isdefined(Main, part1_func)
        println("  Part 1...")
        try
            func = getfield(Main, part1_func)
            stats = benchmark_function((data) -> Base.invokelatest(func, data), input_data)
            push!(results, merge(stats, Dict(
                "day" => day,
                "part" => 1,
                "implementation" => impl_name
            )))
            println("    Result: $(stats["result"]) (mean: $(round(stats["mean_ms"], digits=2))ms)")
        catch e
            println("    Error: $e")
        end
    end

    # Benchmark part 2
    part2_func = Symbol("day$(day)_part2")
    if isdefined(Main, part2_func)
        println("  Part 2...")
        try
            func = getfield(Main, part2_func)
            stats = benchmark_function((data) -> Base.invokelatest(func, data), input_data)
            push!(results, merge(stats, Dict(
                "day" => day,
                "part" => 2,
                "implementation" => impl_name
            )))
            println("    Result: $(stats["result"]) (mean: $(round(stats["mean_ms"], digits=2))ms)")
        catch e
            println("    Error: $e")
        end
    end

    return results
end

# Benchmark all days
function benchmark_all()
    """Benchmark all implemented days with both versions."""
    days = [9, 10, 11]
    all_results = []

    for day in days
        # Try direct port (elisp mirror)
        direct_file = "day$(lpad(day, 2, '0')).jl"
        if isfile(direct_file)
            results = benchmark_day_implementation(day, direct_file, "julia-elisp")
            append!(all_results, results)
        end

        # Try idiomatic version
        idiomatic_file = "day$(lpad(day, 2, '0'))_idiomatic.jl"
        if isfile(idiomatic_file)
            results = benchmark_day_implementation(day, idiomatic_file, "julia-idiomatic")
            append!(all_results, results)
        end
    end

    return all_results
end

# Main execution
function main()
    println("Julia AOC Benchmark Runner")
    println("=" ^ 50)
    println("Warmup: $WARMUP_ITERATIONS iterations")
    println("Timed: $DEFAULT_ITERATIONS iterations (or $SLOW_ITERATIONS if >$(SLOW_THRESHOLD_MS)ms)")
    println("=" ^ 50)
    println()

    # Run benchmarks
    results = benchmark_all()

    # Write JSON output
    output_file = "julia_results.json"
    open(output_file, "w") do f
        JSON.print(f, results, 2)
    end

    println()
    println("=" ^ 50)
    println("Results written to $output_file")
    println("Total benchmarks: $(length(results))")
end

# Run if executed directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end

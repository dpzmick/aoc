# Simple benchmark runner that avoids world age issues
using JSON
using Statistics

days = [9, 10, 11]
implementations = [
    ("day{day}.jl", "julia-elisp"),
    ("day{day}_idiomatic.jl", "julia-idiomatic")
]
results = []

for day in days
    for (file_pattern, impl_name) in implementations
        file = replace(file_pattern, "{day}" => lpad(day, 2, '0'))
        file_path = joinpath(@__DIR__, file)

        if !isfile(file_path)
            println("Skipping $file (not found)")
            continue
        end

        println("Benchmarking day $day ($impl_name)...")

        # Run benchmarks in separate process to avoid world age issues
        cmd = """
        using Statistics
        include("$file")
        input = read_input($day)

        # Warmup
        for i in 1:5
            day$(day)_part1(input)
            day$(day)_part2(input)
        end

        # Benchmark part 1
        times_p1 = Float64[]
        for i in 1:20
            t = @elapsed result_p1 = day$(day)_part1(input)
            push!(times_p1, t * 1000)
        end
        result_p1 = day$(day)_part1(input)

        # Benchmark part 2
        times_p2 = Float64[]
        for i in 1:20
            t = @elapsed result_p2 = day$(day)_part2(input)
            push!(times_p2, t * 1000)
        end
        result_p2 = day$(day)_part2(input)

        using Statistics
        println("P1:\$(mean(times_p1)),\$(median(times_p1)),\$(std(times_p1)),\$(minimum(times_p1)),\$(maximum(times_p1)),\$(result_p1)")
        println("P2:\$(mean(times_p2)),\$(median(times_p2)),\$(std(times_p2)),\$(minimum(times_p2)),\$(maximum(times_p2)),\$(result_p2)")
        """

        output = read(`julia -e $cmd`, String)
        lines = split(strip(output), '\n')

        # Parse part 1
        p1_line = filter(l -> startswith(l, "P1:"), lines)[1]
        p1_data = split(replace(p1_line, "P1:" => ""), ',')
        push!(results, Dict(
            "day" => day,
            "part" => 1,
            "implementation" => impl_name,
            "mean_ms" => parse(Float64, p1_data[1]),
            "median_ms" => parse(Float64, p1_data[2]),
            "std_dev_ms" => parse(Float64, p1_data[3]),
            "min_ms" => parse(Float64, p1_data[4]),
            "max_ms" => parse(Float64, p1_data[5]),
            "result" => p1_data[6],
            "iterations" => 20
        ))

        # Parse part 2
        p2_line = filter(l -> startswith(l, "P2:"), lines)[1]
        p2_data = split(replace(p2_line, "P2:" => ""), ',')
        push!(results, Dict(
            "day" => day,
            "part" => 2,
            "implementation" => impl_name,
            "mean_ms" => parse(Float64, p2_data[1]),
            "median_ms" => parse(Float64, p2_data[2]),
            "std_dev_ms" => parse(Float64, p2_data[3]),
            "min_ms" => parse(Float64, p2_data[4]),
            "max_ms" => parse(Float64, p2_data[5]),
            "result" => p2_data[6],
            "iterations" => 20
        ))
    end
end

# Write JSON
open("julia_results.json", "w") do f
    JSON.print(f, results, 2)
end

println("\nResults written to julia_results.json")

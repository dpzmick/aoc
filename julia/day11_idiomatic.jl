# Day 11: Graph Path Counting with Memoized DFS
# Idiomatic Julia version using adjacency lists for better performance

include("aoc_common_idiomatic.jl")

function parse_line(line::AbstractString)::Tuple{String, Vector{String}}
    """Parse line into source and destinations."""
    parts = split(line, ':')
    return (String(parts[1]), String.(split(strip(parts[2]))))
end

function make_node_table(input_str::String)::Dict{String, Int}
    """Create mapping from node names to IDs (0-indexed for consistency)."""
    table = Dict{String, Int}()
    next_id = 0
    for line in split(strip(input_str), '\n')
        isempty(line) && continue
        src, dsts = parse_line(line)
        if !haskey(table, src)
            table[src] = next_id
            next_id += 1
        end
        for dst in dsts
            if !haskey(table, dst)
                table[dst] = next_id
                next_id += 1
            end
        end
    end
    return table
end

function build_adjacency_list(input_str::String, table::Dict{String, Int})::Vector{Vector{Int}}
    """Build adjacency list representation (much more efficient than matrix for sparse graphs)."""
    n = length(table)
    adj = [Int[] for _ in 1:n]

    for line in split(strip(input_str), '\n')
        isempty(line) && continue
        src, dsts = parse_line(line)
        src_id = table[src]
        for dst in dsts
            dst_id = table[dst]
            push!(adj[src_id + 1], dst_id)  # +1 for Julia 1-indexing
        end
    end

    return adj
end

function process_input(input_str::String)::Tuple{Dict{String, Int}, Vector{Vector{Int}}}
    """Parse input into node table and adjacency list."""
    table = make_node_table(input_str)
    adj = build_adjacency_list(input_str, table)
    return table, adj
end

# Part 1: Simple DFS to count all paths (much faster with adjacency lists!)
function dfs(from_node::Int, to_node::Int, adj::Vector{Vector{Int}})::Int
    """Count all paths from from_node to to_node."""
    from_node == to_node && return 1

    # Sum paths from all children (only iterate over actual neighbors!)
    return sum(dfs(child, to_node, adj) for child in adj[from_node + 1]; init=0)
end

expect("day11_part1", "5")
function day11_part1(input_str::String)::String
    table, adj = process_input(input_str)
    you = table["you"]
    out = table["out"]
    return string(dfs(you, out, adj))
end

# Part 2: DFS with memoization
function dfs_p2(from_node::Int, to_node::Int, dac_id::Int, fft_id::Int, adj::Vector{Vector{Int}})::Int
    """Count paths that visit both dac and fft nodes."""
    memo = Dict{Tuple{Int, Bool, Bool}, Int}()

    function dfs_inner(node::Int, seen_dac::Bool, seen_fft::Bool)::Int
        # Update seen flags
        seen_dac |= (node == dac_id)
        seen_fft |= (node == fft_id)

        # Check memo
        key = (node, seen_dac, seen_fft)
        haskey(memo, key) && return memo[key]

        # Base case
        if node == to_node
            result = (seen_dac && seen_fft) ? 1 : 0
        else
            # Recursive case: sum paths from children
            result = sum(dfs_inner(child, seen_dac, seen_fft) for child in adj[node + 1]; init=0)
        end

        return memo[key] = result
    end

    return dfs_inner(from_node, false, false)
end

expect("day11_part2", "2")
function day11_part2(input_str::String)::String
    table, adj = process_input(input_str)
    svr, out = table["svr"], table["out"]
    dac, fft = table["dac"], table["fft"]
    return string(dfs_p2(svr, out, dac, fft, adj))
end

# Main execution
if abspath(PROGRAM_FILE) == @__FILE__
    # Run on real input (no sample file for day11)
    real = read_input(11)
    println("Running Part 1 on real input:")
    println(day11_part1(real))

    println("\nRunning Part 2 on real input:")
    println(day11_part2(real))
end

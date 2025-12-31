# Day 11: Graph Path Counting with Memoized DFS
# Direct port from day11.el maintaining same data structures (RowMajorMat) and algorithms

include("aoc_common.jl")

function parse_line(line::AbstractString)::Tuple{String, Vector{String}}
    """Parse line into source and destinations."""
    parts = split(line, ':')
    src = String(parts[1])
    dsts = String.(split(strip(parts[2])))
    return (src, dsts)
end

function intern_name!(name::AbstractString, table::Dict{String, Int})
    """Assign ID to name if not already present."""
    if !haskey(table, name)
        table[name] = length(table)
    end
end

function make_node_table(input_str::String)::Dict{String, Int}
    """Create mapping from node names to IDs."""
    table = Dict{String, Int}()
    for line in split(strip(input_str), '\n')
        isempty(line) && continue
        src, dsts = parse_line(line)
        intern_name!(src, table)
        for dst in dsts
            intern_name!(dst, table)
        end
    end
    return table
end

function fill_mat_line!(line::AbstractString, adj::RowMajorMat, table::Dict{String, Int})
    """Fill adjacency matrix for one line."""
    src, dsts = parse_line(line)
    row = table[src]
    for dst in dsts
        col = table[dst]
        adj[row, col] = 1
    end
end

function process_input(input_str::String)::Tuple{Dict{String, Int}, RowMajorMat}
    """Parse input into node table and adjacency matrix."""
    table = make_node_table(input_str)
    n = length(table)
    adj = RowMajorMat(n, n, 0)

    for line in split(strip(input_str), '\n')
        isempty(line) && continue
        fill_mat_line!(line, adj, table)
    end

    return table, adj
end

# Part 1: Simple DFS to count all paths
function dfs(from_node::Int, to_node::Int, adj::RowMajorMat)::Int
    """Count all paths from from_node to to_node."""
    if from_node == to_node
        return 1
    end

    # Sum paths from all children
    total = 0
    for i in 0:adj.cols-1
        if adj[from_node, i] == 1
            total += dfs(i, to_node, adj)
        end
    end

    return total
end

expect("day11_part1", "5")
function day11_part1(input_str::String)::String
    table, adj = process_input(input_str)
    you = table["you"]
    out = table["out"]
    return string(dfs(you, out, adj))
end

# Part 2: DFS with memoization tracking specific nodes visited
function dfs_p2(from_node::Int, to_node::Int, dac_id::Int, fft_id::Int, adj::RowMajorMat)::Int
    """Count paths that visit both dac and fft nodes."""
    memo = Dict{Tuple{Int, Bool, Bool}, Int}()

    function dfs_inner(from_node::Int, to_node::Int, seen_dac::Bool, seen_fft::Bool)::Int
        # Update seen flags
        seen_dac = seen_dac || (from_node == dac_id)
        seen_fft = seen_fft || (from_node == fft_id)

        # Check memo
        key = (from_node, seen_dac, seen_fft)
        if haskey(memo, key)
            return memo[key]
        end

        # Base case
        if from_node == to_node
            result = (seen_dac && seen_fft) ? 1 : 0
        else
            # Recursive case: sum paths from children
            result = 0
            for i in 0:adj.cols-1
                if adj[from_node, i] == 1
                    result += dfs_inner(i, to_node, seen_dac, seen_fft)
                end
            end
        end

        memo[key] = result
        return result
    end

    return dfs_inner(from_node, to_node, false, false)
end

expect("day11_part2", "2")
function day11_part2(input_str::String)::String
    table, adj = process_input(input_str)
    svr = table["svr"]
    out = table["out"]
    dac = table["dac"]
    fft = table["fft"]
    result = dfs_p2(svr, out, dac, fft, adj)
    return string(result)
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

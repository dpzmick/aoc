# Minimal test for day11 part2 issue
include("aoc_common.jl")

function parse_line(line::AbstractString)::Tuple{String, Vector{String}}
    parts = split(line, ':')
    src = String(parts[1])
    dsts = String.(split(strip(parts[2])))
    return (src, dsts)
end

function intern_name!(name::AbstractString, table::Dict{String, Int})
    if !haskey(table, name)
        table[name] = length(table)
    end
end

function make_node_table(input_str::String)::Dict{String, Int}
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
    src, dsts = parse_line(line)
    row = table[src]
    for dst in dsts
        col = table[dst]
        adj[row, col] = 1
    end
end

function process_input(input_str::String)::Tuple{Dict{String, Int}, RowMajorMat}
    table = make_node_table(input_str)
    n = length(table)
    adj = RowMajorMat(n, n, 0)

    for line in split(strip(input_str), '\n')
        isempty(line) && continue
        fill_mat_line!(line, adj, table)
    end

    return table, adj
end

# EXACT port of Python dfs_p2
function dfs_p2(from_node::Int, to_node::Int, dac_id::Int, fft_id::Int, adj::RowMajorMat)::Int
    memo = Dict{Tuple{Int, Bool, Bool}, Int}()

    function dfs_inner(from_node::Int, to_node::Int, seen_dac::Bool, seen_fft::Bool)::Int
        # Update seen flags
        seen_dac_new = seen_dac || (from_node == dac_id)
        seen_fft_new = seen_fft || (from_node == fft_id)

        # Check memo
        key = (from_node, seen_dac_new, seen_fft_new)
        if haskey(memo, key)
            return memo[key]
        end

        # Base case
        if from_node == to_node
            result = (seen_dac_new && seen_fft_new) ? 1 : 0
        else
            # Recursive case
            result = 0
            for i in 0:adj.cols-1
                if adj[from_node, i] == 1
                    result += dfs_inner(i, to_node, seen_dac_new, seen_fft_new)
                end
            end
        end

        memo[key] = result
        return result
    end

    return dfs_inner(from_node, to_node, false, false)
end

# Test
input_str = read_input(11)
table, adj = process_input(input_str)
you, out, dac, fft = table["you"], table["out"], table["dac"], table["fft"]
println("IDs: you=$you, out=$out, dac=$dac, fft=$fft")
println("adj size: $(adj.rows)x$(adj.cols)")

# Print some edges from you
println("Edges from you:")
for i in 0:adj.cols-1
    if adj[you, i] == 1
        # Find node name for this ID
        for (name, id) in table
            if id == i
                println("  you -> $name (id=$i)")
                break
            end
        end
    end
end

# Check basic connectivity first
function simple_dfs(from_node::Int, to_node::Int, adj::RowMajorMat)::Int
    if from_node == to_node
        return 1
    end
    total = 0
    for i in 0:adj.cols-1
        if adj[from_node, i] == 1
            total += simple_dfs(i, to_node, adj)
        end
    end
    return total
end

println("Testing basic connectivity:")
println("  you -> out: $(simple_dfs(you, out, adj)) paths")
println("  you -> dac: $(simple_dfs(you, dac, adj)) paths")
println("  you -> fft: $(simple_dfs(you, fft, adj)) paths")
println("  dac -> out: $(simple_dfs(dac, out, adj)) paths")
println("  fft -> out: $(simple_dfs(fft, out, adj)) paths")

result = dfs_p2(you, out, dac, fft, adj)
println("Result: $result")

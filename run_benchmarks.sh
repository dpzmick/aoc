#!/bin/bash
set -e

echo "======================================================================"
echo "Multi-Language AOC Benchmark Suite"
echo "======================================================================"
echo ""

echo "Running Elisp benchmarks (5 warmup, 20 timed iterations)..."
emacs --batch -l benchmark_elisp.el -f run-elisp-benchmarks
echo ""

echo "Running Python benchmarks (5 warmup, 20 timed iterations)..."
cd python && python3 benchmark_runner.py && cd ..
echo ""

echo "Running LuaJIT benchmarks (5 warmup, 20 timed iterations)..."
# Set Lua paths for LuaJIT (Lua 5.1 compatible packages)
export LUA_PATH='/opt/homebrew/Cellar/luarocks/3.12.2/share/lua/5.1/?.lua;/opt/homebrew/share/lua/5.1/?.lua;/opt/homebrew/share/lua/5.1/?/init.lua;/opt/homebrew/lib/lua/5.1/?.lua;/opt/homebrew/lib/lua/5.1/?/init.lua;./?.lua;./?/init.lua;/Users/dpzmick/.luarocks/share/lua/5.1/?.lua;/Users/dpzmick/.luarocks/share/lua/5.1/?/init.lua'
export LUA_CPATH='/opt/homebrew/lib/lua/5.1/?.so;/opt/homebrew/lib/lua/5.1/loadall.so;./?.so;/Users/dpzmick/.luarocks/lib/lua/5.1/?.so'
cd lua && luajit benchmark_runner.lua && cd ..
echo ""

echo "Running Julia benchmarks (5 warmup, 20 timed iterations)..."
cd julia && julia benchmark_runner.jl && cd ..
echo ""

echo "======================================================================"
echo "BENCHMARK COMPARISON: Multi-Language Results"
echo "======================================================================"
echo ""

python3 compare_multi.py

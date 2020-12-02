#!/bin/bash

array=("ABP")
mkdir -p results
for i in "${array[@]}"; do
    echo "Start of benchmark for $i"
    escript run_bench.sh "benchmarks/${i}Slice.csp" | tee "results/${i}Slice-$(date +%Y%m%d-%H%M%S).txt"
done

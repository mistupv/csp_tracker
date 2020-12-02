#!/bin/bash

array=("Buses" "Loop" "Disk" "Oven" "ProdCons" "ReadWrite" "Traffic" "ABP" "ATM" "CPU")
mkdir -p results
for i in "${array[@]}"; do
    echo "Start of benchmark for $i"
    escript run_bench.sh "benchmarks/${i}Slice.csp" | tee "results/${i}Slice-$(date +%Y%m%d-%H%M%S).txt"
done

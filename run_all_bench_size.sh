#!/bin/bash

array=("Loop" "ProdCons" "ReadWrite")
mkdir -p results
#array=("ReadWrite" "ProdCons")
for i in "${array[@]}"; do
    echo "Start of benchmark for $i"
    escript run_bench_size.sh "benchmarks/$i.csp" | tee "results/$i-Size-$(date +%Y%m%d-%H%M%S).txt"
done

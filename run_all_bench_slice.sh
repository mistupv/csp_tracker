#!/bin/bash

array=("Buses" "Loop" "Disk" "Oven" "ProdCons" "ReadWrite" "Traffic" "ABP" "ATM" "CPU")
for i in "${array[@]}"; do 
    escript run_bench.sh "benchmarks/${i}Slice.csp" > "results/$i.txt"
done


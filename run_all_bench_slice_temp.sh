#!/bin/sh

array=("ProdCons" "ReadWrite" "Traffic" "ABP" "ATM" "CPU" "Oven" )
for i in "${array[@]}"; do 
    escript run_bench.sh 'benchmarks/'$i'Slice.csp' > results/$i.txt
done


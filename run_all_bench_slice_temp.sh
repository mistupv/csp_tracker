#!/bin/sh

array=("ABP" "ReadWrite")
for i in "${array[@]}"; do 
    escript run_bench.sh 'benchmarks/'$i'Slice.csp' > results/$i.txt
done


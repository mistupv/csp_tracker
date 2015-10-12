#!/bin/sh

array=("Loop" "ProdCons" "ReadWrite")
for i in "${array[@]}"; do 
    escript run_bench.sh 'benchmarks/'$i'.csp' > results/$i.txt
done


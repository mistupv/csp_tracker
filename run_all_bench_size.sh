#!/bin/sh

#array=("Loop" "ProdCons" "ReadWrite")
array=("ReadWrite")
for i in "${array[@]}"; do 
    escript run_bench_size.sh 'benchmarks/'$i'.csp' > results/$i.txt
done


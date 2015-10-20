#!/bin/sh

array=("Loop" "ProdCons" "ReadWrite")
#array=("ReadWrite" "ProdCons")
for i in "${array[@]}"; do 
    escript run_bench_size.sh 'benchmarks/'$i'.csp' > results/$i.txt
done

#escript run_bench_size.sh 'benchmarks/ReadWrite.csp' > results/ReadWrite.txt
# escript run_bench_size.sh 'benchmarks/Loop.csp' > results/Loop.txt


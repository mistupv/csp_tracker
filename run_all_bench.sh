#!/bin/bash


run() {
  export CSP_TRACKER_MODE=$1
  echo "Execution mode: $CSP_TRACKER_MODE"
  for i in {1..1000}; do
    echo -n "Iteration $i "
    escript run_bench.sh "benchmarks/$2.csp" 1 1000
  done
}

# Store a reference to the code used to run this benchmark
git_hash=$(git rev-parse HEAD)

# ABP and ATM have been removed due to missing channel compatibility
array=("Buses" "Loop" "Disk" "Oven" "ProdCons" "ReadWrite" "Traffic" "CPU")
#array=("Buses" "Loop" "Disk" "Oven" "ProdCons" "ReadWrite" "Traffic" "ABP" "ATM" "CPU")
mkdir -p results
for i in "${array[@]}"; do
    echo "Start of benchmark for $i"
    date=$(date +%Y%m%d-%H%M%S)
    log="results/$i-$date.txt"
    echo $git_hash > $log
    run run   $i | tee -a $log
    run track $i | tee -a $log
done

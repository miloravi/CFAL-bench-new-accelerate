# Check if already sourced
[[ -n "${CFAL_COMMON_SOURCED:-}" ]] && return 0
CFAL_COMMON_SOURCED=1

# Names of folders containing accelerate-llvm and accelerate-llvm-native
PACKAGES=(accelerate-llvm-old accelerate-llvm)

# Name of the accelerate-llvm variant that will be displayed in results
declare -A PKG_NAMES=(
  [accelerate-llvm-old]="old"
  [accelerate-llvm]="new"
)

# Thread counts to benchmark
THREAD_COUNTS=(1 4 8 12 16 20 24 28 32)
# THREAD_COUNTS=(1 3 6)

parse_flags() {
    TIMER_FALLBACK=""
    DEBUG=""
    
    for arg in "$@"; do
        if [[ "$arg" == "--timer-fallback" ]]; then
            TIMER_FALLBACK="ghc-options:
  accelerate: -DTRACY_TIMER_FALLBACK"
        fi
        if [[ "$arg" == "--debug" && -z "$DEBUG" ]]; then
            DEBUG="flags:
  accelerate:
    debug: true"
        fi
        if [[ "$arg" == "--tracy" ]]; then
            DEBUG="flags:
  accelerate:
    debug: true
    tracy: true"
        fi
    done
}

create_temp_stack_yaml() {
    local pkg="$1"
    local path="$2"
    local extra_packages="$3"
    local extra_deps="$4"
    local extra_flags="$5"
    
    parse_flags "$@"

    cat > temp-stack.yaml <<EOF
snapshot:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/21/25.yaml

$extra_flags

packages:
- .
- ../${path}accelerate
- ../$path$pkg/accelerate-llvm
- ../$path$pkg/accelerate-llvm-native
$extra_packages

extra-deps:
- monadLib-3.10.3@sha256:026ba169762e63f0fe5f5c78829808f522a28730043bc3ad9b7c146baedf709f,637
- github: tomsmeding/llvm-pretty
  commit: a253a7fc1c62f4825ffce6b2507eebc5dadff32c
- MIP-0.2.0.0
- OptDir-0.0.4
- bytestring-encoding-0.1.2.0
- acme-missiles-0.3
- git: https://github.com/commercialhaskell/stack.git
  commit: e7b331f14bcffb8367cd58fbfc8b40ec7642100a
$extra_deps

$timer_fallback

$debug
EOF
}

bench() {
    local path="$1"
    local bench_name="$2"
    local extra_packages="$3"
    local extra_deps="$4"
    local extra_flags="$5"

    mkdir -p results

    # Remove old results files
    rm -f results/benchmark_*.csv

    for pkg in "${PACKAGES[@]}"; do
      name="${PKG_NAMES[$pkg]}"
      
      rm -f results/results-$name-*.csv

      echo "Benching $name"

      # Create temp stack.yaml
      create_temp_stack_yaml "$pkg" "$path" "$extra_packages" "$extra_deps" "$extra_flags" > temp-stack.yaml

      for threads in "${THREAD_COUNTS[@]}"; do
        echo "Benching with $threads threads"
        
        # Set thread count and run benchmark
        export ACCELERATE_LLVM_NATIVE_THREADS=$threads
        STACK_YAML=temp-stack.yaml stack run $bench_name -- --csv results/results-$name-$threads.csv --time-limit 300
        
        # Add thread count column to CSV
        if [ -f "results/results-$name-$threads.csv" ]; then

          while IFS= read -r line; do
            # Skip header line
            if [[ $line == "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB"* ]]; then
                continue
            fi
            
            # Skip empty lines
            [[ -z "$line" ]] && continue
            
            # Extract benchmark name (first field)
            if [[ $line =~ ^\"([^\"]*)\", ]]; then
                # In case the name is in quotes
                benchmark_name="${BASH_REMATCH[1]}"
            elif [[ $line =~ ^([^,]*), ]]; then
                # In case the name is without quotes, stop at ','
                benchmark_name="${BASH_REMATCH[1]}"
            else
                echo "Error: Unable to parse benchmark name from line: $line" >&2
                exit 1
            fi
            
            file_name=$(echo "$benchmark_name" | sed 's/\//_/g' | sed 's/ /_/g')
            output_file="results/benchmark_${file_name}.csv"
            
            # Create header if this is the first time writing to this file
            if [ ! -f "$output_file" ]; then
                echo "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB,scheduler,threads" > "$output_file"
            fi
            
            # Add the data line with package name and thread count
            echo "${line},${name},${threads}" >> "$output_file"
            
        done < "results/results-$name-$threads.csv"

      fi
        
      done
      
      # Clean up individual thread files
      rm -f results/results-$name-*.csv

      rm temp-stack.yaml
      
    done

    echo "Benchmarks results saved in results folder"

    unset ACCELERATE_LLVM_NATIVE_THREADS

}
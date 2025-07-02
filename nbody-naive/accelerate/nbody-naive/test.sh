#!/bin/bash

# Source shared configuration
source "../../../accelerate_config.sh" || { echo "Failed to source accelerate_config.sh"; exit 1; }

plot "results/benchmark_CPU_(Scalar_Z_[100000],10).csv"

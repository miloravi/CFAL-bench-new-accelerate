# Source shared configuration
source "../../accelerate_config.sh" || { echo "Failed to source accelerate_config.sh"; exit 1; }

bench "../../" "mg"

# Source shared configuration
source "../../../accelerate_config.sh" || { echo "Failed to source accelerate_config.sh"; exit 1; }

parse_flags "$@"

for pkg in "${PACKAGES[@]}"; do
  name="${PKG_NAMES[$pkg]}"
  # Remove old results file to avoid appending
  rm -f "results-$name.csv"
  # Create a temporary stack.yaml for old and new package
  create_temp_stack_yaml "$pkg" "../../../" "$TIMER_FALLBACK" "$DEBUG" "" "" > temp-stack.yaml

  echo "Running $name"
  STACK_YAML=temp-stack.yaml stack run nbody-naive -- --csv results-$name.csv

  rm temp-stack.yaml
done

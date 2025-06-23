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

declare -A PKG_NAMES=(
  [accelerate-llvm-old]="old"
  [accelerate-llvm]="new"
)

for pkg in accelerate-llvm-old accelerate-llvm; do
  name="${PKG_NAMES[$pkg]}"
  # Remove old results file to avoid appending
  rm -f "results-$name.csv"
  # Create a temporary stack.yaml for old and new package
  cat > temp-stack.yaml <<EOF
snapshot:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/21/25.yaml

packages:
- .
- ../../../accelerate
- ../../../$pkg/accelerate-llvm
- ../../../$pkg/accelerate-llvm-native

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

$TIMER_FALLBACK

$DEBUG
EOF

  echo "Running $name"
  STACK_YAML=temp-stack.yaml stack run mg -- --csv results-$name.csv

  rm temp-stack.yaml
done
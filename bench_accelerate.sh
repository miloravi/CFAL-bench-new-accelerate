#!/bin/bash

(cd "MG/accelerate" && bash bench.sh) || { echo "Failed to bench MG"; exit 1; }
(cd "nbody-naive/accelerate/nbody-naive" && bash bench.sh) || { echo "Failed to bench nbody-naive"; exit 1; }
(cd "quickhull/accelerate" && bash bench.sh) || { echo "Failed to bench quickhull"; exit 1; }

(cd "MG/accelerate" && bash run.sh) || { echo "Failed to run MG"; exit 1; }
(cd "nbody-naive/accelerate/nbody-naive" && bash run.sh) || { echo "Failed to run nbody-naive"; exit 1; }
(cd "quickhull/accelerate" && bash run.sh) || { echo "Failed to run quickhull"; exit 1; }
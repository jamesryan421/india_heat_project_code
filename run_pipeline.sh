#!/bin/bash
set -e

# Load R packages from hone library
export R_LIBS_USER="$HOME/R/library"

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

# Install and verify R packages if needed
echo "----------\n"
echo "Checking R package dependencies..."
Rscript requirements.R

echo "Finished checking R package dependencies"
echo "----------\n"

# NEW: Define where data lives on THIS specific machine
export PROJECT_DATA_DIR="/rdf/xt9/india_estimation"

# Tell targets where to put the _targets store
if [ ! -f _targets.yaml ]; then
    echo "Configuring main project data path to: $PROJECT_DATA_DIR"
    Rscript -e "targets::tar_config_set(store = '${PROJECT_DATA_DIR}/_targets')"
fi

echo "Launching targets pipeline execution at Timestamp: $(date +'%Y-%m-%d %H:%M:%S')"
Rscript -e "targets::tar_make()"

echo "Successfully finished pipeline execution at Timestamp: $(date +'%Y-%m-%d %H:%M:%S')"
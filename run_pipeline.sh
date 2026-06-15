#!/bin/bash
set -e

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

# NEW: Define where data lives on THIS specific machine
export PROJECT_DATA_DIR="/rdf/xt9/india_estimation"

# Tell targets where to put the _targets store
if [ ! -f _targets.yaml ]; then
    Rscript -e "targets::tar_config_set(store = '${MY_PROJECT_DATA_DIR}/_targets')"
fi

Rscript -e "targets::tar_make()"
#!/bin/bash
#SBATCH --job-name=heat_main
#SBATCH --partition=commons
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=48G
#SBATCH --time=01:00:00
#SBATCH --output=/scratch/td52/heat_light_2026-10-09/results/%x-%j.out
source /scratch/td52/heat_light_2026-10-09/env.sh
export PROJECT_DATA_DIR=/scratch/td52/heat_light_2026-10-09/data_main
cd /scratch/td52/heat_light_2026-10-09/run_main
Rscript -e "targets::tar_config_set(store = '/scratch/td52/heat_light_2026-10-09/run_main/_targets')"
Rscript -e 'targets::tar_make(reporter = "timestamp")'
Rscript -e 'm <- targets::tar_meta(); e <- m$name[!is.na(m$error)]; cat("errored targets:", length(e), "\n"); print(e)'

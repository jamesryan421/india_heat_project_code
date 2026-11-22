#!/bin/bash
#SBATCH --job-name=heat_theta3
#SBATCH --partition=commons
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=01:00:00
#SBATCH --output=/scratch/td52/heat_light_2026-10-09/theta/out/%x-%j.out
source /scratch/td52/heat_light_2026-10-09/env.sh
T=/scratch/td52/heat_light_2026-10-09/theta
MIG=/scratch/td52/heat_light_2026-10-09/migchoice/in
POPF=/scratch/td52/heat_light_2026-10-09/pipeline_data/data/population_projections/india_pop_worldpop.csv
cd $T
echo "=== Survey micro ==="; Rscript code/theta_micro.R in $MIG out || exit 1
echo "=== Estimation ==="; Rscript code/theta_est.R out || exit 1
echo "=== Implied heat value ==="; Rscript code/theta_implied.R $MIG $POPF out $T/out

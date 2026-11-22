#!/bin/bash
#SBATCH --job-name=heat_figdata
#SBATCH --partition=commons
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=00:40:00
#SBATCH --output=/scratch/td52/heat_light_2026-10-09/theta/out/%x-%j.out
source /scratch/td52/heat_light_2026-10-09/env.sh
T=/scratch/td52/heat_light_2026-10-09/theta
MIG=/scratch/td52/heat_light_2026-10-09/migchoice/in
POPF=/scratch/td52/heat_light_2026-10-09/pipeline_data/data/population_projections/india_pop_worldpop.csv
cd $T
Rscript code/figdata.R $MIG $POPF out $T/out

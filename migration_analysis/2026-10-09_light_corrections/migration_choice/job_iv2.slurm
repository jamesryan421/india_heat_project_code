#!/bin/bash
#SBATCH --job-name=heat_migiv2
#SBATCH --partition=commons
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=40G
#SBATCH --time=02:00:00
#SBATCH --output=/scratch/td52/heat_light_2026-10-09/migchoice/out/%x-%j.out
source /scratch/td52/heat_light_2026-10-09/env.sh
cd /scratch/td52/heat_light_2026-10-09/migchoice
Rscript in/mig_choice_iv_altwage.R in /scratch/td52/heat_light_2026-10-09/pipeline_data/data/population_projections/india_pop_worldpop.csv out /scratch/td52/heat_light_2026-10-09/pipeline_data/data/bartik/bartik_instruments_ec05_ec13.csv ${B:-300}

#!/bin/bash
#SBATCH --job-name=heat_migchoice
#SBATCH --partition=commons
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=24G
#SBATCH --time=00:40:00
#SBATCH --output=/scratch/td52/heat_light_2026-10-09/migchoice/out/%x-%j.out
source /scratch/td52/heat_light_2026-10-09/env.sh
cd /scratch/td52/heat_light_2026-10-09/migchoice
Rscript in/mig_choice.R in /scratch/td52/heat_light_2026-10-09/pipeline_data/data/population_projections/india_pop_worldpop.csv out

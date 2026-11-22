module purge >/dev/null 2>&1
module load GCC/13.3.0 OpenMPI/5.0.3 R/4.4.2 R-bundle-CRAN/2024.11
export R_LIBS_USER=/scratch/td52/heat_light_2026-10-09/Rlib
export R_LIBS=$R_LIBS_USER
export OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1

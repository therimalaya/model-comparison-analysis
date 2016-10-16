#!/bin/sh
#SBATCH --ntasks=32
#SBATCH --nodes=1
#SBATCH --job-name=BayesFit
#SBATCH --mem=16G

module load R/3.3.1
Rscript -e "source('03-simulation.r')"

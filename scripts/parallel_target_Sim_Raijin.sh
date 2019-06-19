#!/bin/bash
#PBS -l wd
#PBS -o targetSim.out
#PBS -e targetSim.err
#PBS -q normal
#PBS -l mem=100GB
#PBS -l ncpus=32
#PBS -P yr31
#PBS -l walltime=00:30:00

module load R/3.5.1

R -f /short/yr31/aa7970/azData/DD/Scripts/parallel_target_Sim_Raijin.R \
--dataDir /short/yr31/aa7970/azData/DD/Data/ \
--dataFile protein.fasta \
--startRow 1 \
--offset 10 \
--nClusters 122 \
--outFile protSim  >  output

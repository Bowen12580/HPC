#!/bin/bash
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
echo "R is about to run"
R --vanilla < $HOME/run_files/bd623_HPC_2023_neutral_cluster.R
mv output_* $HOME/output_files
echo "R has finished running"

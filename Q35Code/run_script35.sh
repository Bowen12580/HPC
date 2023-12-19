#!/bin/bash
#PBS -l walltime=00:10:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
echo "R is about to run"
R --vanilla < $HOME/run_files35/bd623_HPC_2023_demographic_cluster.R
mv simulation_result_* $HOME/output_files35
echo "R has finished running"

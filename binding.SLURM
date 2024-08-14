#!/bin/bash

####################################

#SBATCH --job-name=kalpana_ehr
#SBATCH --time=40:00
#SBATCH --mail-type=END
#SBATCH --mem=300m
#SBATCH --cpus-per-task=1
#SBATCH --array=1-2000
#SBATCH --output=s_out/%x_%A-%a.out

R CMD BATCH --no-save --no-restore code16.R r_out/"$SLURM_JOB_NAME"_"$SLURM_ARRAY_JOB_ID"-"$SLURM_ARRAY_TASK_ID".Rout
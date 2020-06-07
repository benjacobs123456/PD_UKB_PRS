#!/bin/bash
#$ -pe smp 40
#$ -l h_vmem=5G
#$ -l h_rt=240:0:0
#$ -l highmem
#$ -j y
#$ -N bootstrap_prs_ukb_case_control
#$ -o /data/scratch/hmy117
cd /data/Wolfson-UKBB-Dobson/ukb_pheno_911

module load R
Rscript /data/Wolfson-UKBB-Dobson/other_scripts/bootstrap_PD_PRS_excluded_cases.r 1000 40





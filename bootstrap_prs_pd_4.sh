#!/bin/bash
#$ -pe smp 30
#$ -l h_vmem=10G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -o /data/scratch/hmy117
#$ -l highmem
cd /data/Wolfson-UKBB-Dobson/ukb_pheno_911

module load R
Rscript /data/Wolfson-UKBB-Dobson/PD_pheno/bootstrap_prs_pd_4.r 5000 30


#!/bin/bash
#$ -pe smp 20
#$ -l h_vmem=5G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -o /data/scratch/hmy117
cd /data/Wolfson-UKBB-Dobson/ukb_pheno_911

module load R
Rscript /data/Wolfson-UKBB-Dobson/PD_pheno/bootstrap_prs_pd_1.r 5000 20


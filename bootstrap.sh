#!/bin/bash
#$ -pe smp 20
#$ -l h_vmem=10G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -o /data/scratch/hmy117
cd /data/Wolfson-UKBB-Dobson/ukb_pheno_911

module load R
Rscript /data/Wolfson-UKBB-Dobson/PD_pheno/example_bootstrap.r 5000 20


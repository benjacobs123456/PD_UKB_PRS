#!/bin/bash
#$ -pe smp 25
#$ -l h_vmem=6G
#$ -l h_rt=1:0:0
#$ -j y
#$ -N convert_raw_ukb_phenotype_data
#$ -o /data/scratch/hmy117
#$ -t 1:10

cd /data/Wolfson-UKBB-Dobson/ukb_pheno_911

i=$((${SGE_TASK_ID}-1))
j=$(echo chunkx0$i)
k=$(echo outputx0$i)

echo "task id is" ${SGE_TASK_ID}
echo $j

# This will run the script below to convert the ukb
# phenotype data to a usable format
module load R/3.6.1


Rscript /data/Wolfson-UKBB-Dobson/other_scripts/ukb_convert_pheno_PD_stage3.r \
$j \
$k


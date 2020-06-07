#!/bin/bash
#$ -pe smp 4
#$ -l h_vmem=4G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -t 1:22

##############################
# Step 2 - clump PD GWAS
##############################

cd /data/scratch/hmy117


module load plink

#clump
for i in 0.00005
	do
		plink --bfile /data/Wolfson-UKBB-Dobson/1kg_reference/filtered_chr${SGE_TASK_ID} \
--clump pd_gwas \
--clump-r2 0.4 \
--clump-kb 250 \
--clump-p1 $i \
--out pval$i\_chr${SGE_TASK_ID} \
--threads $NSLOTS

# print duplicate rsids for exclusion in the next stage 
		awk /rs/'{print $3}' /data/Wolfson-UKBB-Dobson/imputed_ukb_genotypes/plink2_files/chr_${SGE_TASK_ID}\.pvar | sort | uniq -d > non_unique_snps_chr${SGE_TASK_ID}
	done














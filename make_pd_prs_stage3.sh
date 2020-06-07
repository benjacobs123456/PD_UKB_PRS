#!/bin/bash
#$ -pe smp 15
#$ -l h_vmem=4G
#$ -l h_rt=1:0:0
#$ -j y
#$ -t 1:22
#$ -cwd

##############################
# Step 3 - individual risk scores
##############################

cd /data/scratch/hmy117

# SCORE for different pvals
# MUST be run after stage 2 (clumping)

module load plink/2.0-20170920

for i in 0.00005
	do
    # print rsids
		awk '{print $3}' pval$i\_chr${SGE_TASK_ID}\.clumped > pval$i\_chr${SGE_TASK_ID}\_snps
    # score
		plink2 --pfile /data/Wolfson-UKBB-Dobson/imputed_ukb_genotypes/plink2_files/chr_${SGE_TASK_ID} \
    --exclude non_unique_snps_chr${SGE_TASK_ID} \
    --extract pval$i\_chr${SGE_TASK_ID}\_snps \
    --score pd_gwas_for_score variance-standardize cols=+scoresums \
    --out pval$i\_chr${SGE_TASK_ID} \
    --threads $NSLOTS
    		file=$(echo pval$i\_chr${SGE_TASK_ID}\.sscore)
		if [ -f "$file" ]; then 
			awk 'NR>1{print $2,$6}' pval$i\_chr${SGE_TASK_ID}\.sscore | sort -k1 -n > sorted_chr${SGE_TASK_ID}\_scores_pval$i
		fi
	done













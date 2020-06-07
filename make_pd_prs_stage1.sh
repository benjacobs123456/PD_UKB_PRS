#!/bin/bash
#$ -pe smp 1
#$ -l h_vmem=16G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y

module load R
#########################################################
# Step 1 for making PRS - collates PD GWAS with UKB SNPS
#########################################################

cd /data/Wolfson-UKBB-Dobson/PD/ALASTAIR_SEPT2019

#########################################################
# MAF filter, palindromic variants filter, autosomes only
#########################################################

awk 'NR>1{if($4>0.01 && $4 < 0.99) print $1,$2,$3,$4,$8,$9,$10,$2$3}' META_NO_UKBB1.tbl | grep -E "ac|ag|ca|ga|ct|gt|tg|tc" | grep -v -E "X|Y" | cat > /data/scratch/hmy117/pd_gwas_sorted

cd /data/Wolfson-UKBB-Dobson/imputed_ukb_genotypes/plink2_files
rm ukb_snps
for i in {1..22}
  do
    cat chr_$i\.pvar >> ukb_snps
  done
awk 'NR>1{print "chr"$1":"$2,$3,$4,$5}' ukb_snps > /data/scratch/hmy117/ukb_snps_sorted

cd /data/scratch/hmy117/
Rscript /data/Wolfson-UKBB-Dobson/other_scripts/join_pd_ukb.r

awk 'BEGIN{print "SNP","A1","A2","EAF","Beta","SE","P"};NR>1{print $9,$2,$3,$4,$5,$6,$7}' pd_snps_rsids.tsv > pd_gwas

awk 'NR>1{print $1,toupper($2),$5}' pd_gwas > pd_gwas_for_score
rm pd_gwas_sorted




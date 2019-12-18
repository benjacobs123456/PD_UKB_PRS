#! /bin/bash
#$ -pe smp 1
#$ -l h_vmem=16G
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -o /data/scratch/hmy117/

###########################################
# Step 4 - combined scores across chromosomes and clean up
###########################################


cd /data/scratch/hmy117/

for j in 1 0.8 0.6 0.4 0.2 0.1 0.05 0.005 0.0005 0.00005 0.00000005
  do

#combbine scores

    for i in {2..22}
      do
        join sorted_chr1_scores_pval$j sorted_chr$i\_scores_pval$j > combined_score
        mv combined_score sorted_chr1_scores_pval$j
      done
    awk 'BEGIN{print "EID","PRS"}; NR>=1{for (i=2;i<=NF;i++) x+=$i; print $1,x; x=0}' sorted_chr1_scores_pval$j > summarised_PRS_results_pval$j
  done

mv summarised_PRS_results* /data/Wolfson-UKBB-Dobson/pd_prs
rm overall_prs*
rm sorted_chr*
rm  prs_clumped_pval*
rm *.sscore
rm *scores*


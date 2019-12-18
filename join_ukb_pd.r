library(dplyr)
library(readr)

############################
# This is a small script to join the SNP lists in
# the PD GWAS (CHR:POS) with rsids from UKB
############################

setwd("/data/scratch/hmy117")


ukb_snps = read_table2("ukb_snps_sorted",col_names=FALSE)
pd = read_table2("pd_gwas_sorted",col_names=FALSE)

colnames(ukb_snps)[1]="position"
colnames(pd)[1]="position"

combo = left_join(pd,ukb_snps,by="position")
combo = na.omit(combo)

write_tsv(combo,"pd_snps_rsids.tsv")


setwd("/data/Wolfson-UKBB-Dobson/PD_pheno/")
library(dplyr)
library(readr)
library(ggplot2)

aps_1 = read_csv("prs_APs_1.csv")
aps_2 = read_csv("prs_APs_2.csv")
aps_3 = read_csv("prs_APs_3.csv")
aps_4 = read_csv("prs_APs_4.csv")

aps = bind_rows(aps_1,aps_2,aps_3,aps_4)
colnames(aps)=c("AP","Lower_ci","Upper_ci")
aps$risk_factor = c("Alcohol","Smoking","Depression","Sleepiness","Epilepsy","Diabetes","Gastric ulcer","Age at menarche")
write_csv(aps,"AP_summary.csv")

p=ggplot(aps,aes(AP,risk_factor))+
  labs(x="AP due to interaction",y="Risk factor")+
  geom_vline(xintercept=0,alpha=0.1)+
  geom_point()+geom_errorbarh(mapping=aes(xmin=Lower_ci,xmax=Upper_ci,y=risk_factor),height=0.3)+
  theme_classic()+
  theme(text=element_text(size=16))
png("interaction_ap.png",height=8,width=8,res=300,units="in")
p
dev.off()
  


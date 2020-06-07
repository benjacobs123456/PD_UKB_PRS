#############################################
#               Load packages 
#############################################

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(RNOmni)
library(rcompanion)
library(ROCR)

#############################################
#               read in data 
#############################################
setwd("/data/Wolfson-UKBB-Dobson/PD_pheno")
selected_vars = read_tsv("testing_set.tsv")
args=commandArgs(trailingOnly=TRUE)
print(paste0("Number of bootstrap iterations:",args[1]))
trials = as.numeric(as.character(args[1]))
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.00005")
selected_vars = selected_vars %>% select(-contains("prs"))
selected_vars$EID = as.numeric(as.character(selected_vars$EID))
selected_vars = selected_vars %>% left_join(prs,by="EID")
selected_vars = selected_vars %>% filter(!(is.na(PRS)))
selected_vars$PRS=rankNorm(selected_vars$PRS)


# interaction with PRS as continuous var
selected_vars$smoking_status=recode(selected_vars$smoking_status,"Previous"="Ever","Current"="Ever","Never"="Never")

# first recode diseases using incident disease
# filter out incident cases
dm = selected_vars %>% filter(DM_status==1 & age_at_dm_diagnosis<=`Age at recruitment.0.0`) %>% mutate("DM_Dx_pre_rec"=1)
no_dm = selected_vars %>% filter(!EID %in% dm$EID) %>% mutate("DM_Dx_pre_rec"=0)
selected_vars = bind_rows(dm,no_dm)
# filter out incident cases
epilepsy = selected_vars %>% filter(Epilepsy_status==1 & age_at_epilepsy_diagnosis<=`Age at recruitment.0.0`) %>% mutate("epilepsy_Dx_pre_rec"=1)
no_epilepsy = selected_vars %>% filter(!EID %in% epilepsy$EID) %>% mutate("epilepsy_Dx_pre_rec"=0)
selected_vars = bind_rows(epilepsy,no_epilepsy)
# filter out incident cases
depression = selected_vars %>% filter(Depression==1 & age_at_depression_diagnosis<=`Age at recruitment.0.0`) %>% mutate("depression_Dx_pre_rec"=1)
no_depression = selected_vars %>% filter(!EID %in% depression$EID) %>% mutate("depression_Dx_pre_rec"=0)
selected_vars = bind_rows(depression,no_depression)
# filter out incident cases
anxiety = selected_vars %>% filter(Anxiety==1 & age_at_anxiety_diagnosis<=`Age at recruitment.0.0`) %>% mutate("Anxiety_Dx_pre_rec"=1)
no_anxiety = selected_vars %>% filter(!EID %in% anxiety$EID) %>% mutate("Anxiety_Dx_pre_rec"=0)
selected_vars = bind_rows(anxiety,no_anxiety)
# filter out incident cases
migraine = selected_vars %>% filter(Migraine==1 & age_at_migraine_diagnosis<=`Age at recruitment.0.0`) %>% mutate("migraine_Dx_pre_rec"=1)
no_migraine = selected_vars %>% filter(!EID %in% migraine$EID) %>% mutate("migraine_Dx_pre_rec"=0)
selected_vars = bind_rows(migraine,no_migraine)
# filter out incident cases
constipation = selected_vars %>% filter(Constipation_status==1 & age_at_constipation_diagnosis<=`Age at recruitment.0.0`) %>% mutate("constipation_Dx_pre_rec"=1)
no_constipation = selected_vars %>% filter(!EID %in% constipation$EID) %>% mutate("constipation_Dx_pre_rec"=0)
selected_vars = bind_rows(constipation,no_constipation)
# filter out incident cases
head_injury = selected_vars %>% filter(Head_injury_status==1 & age_at_head_injury_diagnosis<=`Age at recruitment.0.0`) %>% mutate("head_injury_Dx_pre_rec"=1)
no_head_injury = selected_vars %>% filter(!EID %in% head_injury$EID) %>% mutate("head_injury_Dx_pre_rec"=0)
selected_vars = bind_rows(head_injury,no_head_injury)
# filter out incident cases
Hypertension = selected_vars %>% filter(Hypertension_status==1 & age_at_Hypertension_diagnosis<=`Age at recruitment.0.0`) %>% mutate("Hypertension_Dx_pre_rec"=1)
no_Hypertension = selected_vars %>% filter(!EID %in% Hypertension$EID) %>% mutate("Hypertension_Dx_pre_rec"=0)
selected_vars = bind_rows(Hypertension,no_Hypertension)
Gastric_ulcer = selected_vars %>% filter(Gastric_ulcer_status==1 & age_at_Gastric_ulcer_diagnosis<=`Age at recruitment.0.0`) %>% mutate("Gastric_ulcer_Dx_pre_rec"=1)
no_Gastric_ulcer = selected_vars %>% filter(!EID %in% Gastric_ulcer$EID) %>% mutate("Gastric_ulcer_Dx_pre_rec"=0)
selected_vars = bind_rows(Gastric_ulcer,no_Gastric_ulcer)


library(doParallel)
registerDoParallel(cores=args[2])
df=selected_vars
rm(selected_vars)

df = df %>% select(PD_Dx_after_rec,
                   `Age at recruitment.0.0`,
                   `Sex.0.0`, 
                   `Genetic principal components.0.1`, 
                   `Genetic principal components.0.2`, 
                   `Genetic principal components.0.3`, 
                   `Genetic principal components.0.4`,
                   `Townsend deprivation index at recruitment.0.0`,
                   PRS,alcohol,
                   smoking_status,
                   depression_Dx_pre_rec,
                   sleepiness,
                   epilepsy_Dx_pre_rec,
                   DM_Dx_pre_rec,
                   Gastric_ulcer_Dx_pre_rec,
                   `Age when periods started (menarche).0.0`)

# alcohol   
r = foreach(icount(trials), .combine=cbind) %dopar% {
  df = sample_n(df, size=nrow(df), replace=TRUE)
  model = glm(data=df,
              PD_Dx_after_rec~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                alcohol*PRS,
              family=binomial(link="logit"))
  coeffs=as.numeric(coef(model))
  RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
  AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))}

model = glm(data=df,
            PD_Dx_after_rec~`Age at recruitment.0.0`+
              `Sex.0.0`+
              `Townsend deprivation index at recruitment.0.0`+
              `Genetic principal components.0.1`+
              `Genetic principal components.0.2`+
              `Genetic principal components.0.3`+
              `Genetic principal components.0.4`+
              alcohol*PRS,
            family=binomial(link="logit"))
coeffs=as.numeric(coef(model))
RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))

alcohol_prs=c(AP,quantile(r,c(0.025,0.975)))
print("finished alcohol")
# smoking
r = foreach(icount(trials), .combine=cbind) %dopar% {
  df = sample_n(df, size=nrow(df), replace=TRUE)
  model = glm(data=df,
              PD_Dx_after_rec~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                smoking_status*PRS,
              family=binomial(link="logit"))
  coeffs=as.numeric(coef(model))
  RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
  AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))}

model = glm(data=df,
            PD_Dx_after_rec~`Age at recruitment.0.0`+
              `Sex.0.0`+
              `Townsend deprivation index at recruitment.0.0`+
              `Genetic principal components.0.1`+
              `Genetic principal components.0.2`+
              `Genetic principal components.0.3`+
              `Genetic principal components.0.4`+
              smoking_status*PRS,
            family=binomial(link="logit"))
coeffs=as.numeric(coef(model))
RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))

smoking_prs=c(AP,quantile(r,c(0.025,0.975)))
print("finished smoking")

prs_summary = data.frame(rbind(alcohol_prs,smoking_prs))
write_csv(prs_summary,"prs_APs_1.csv")
print(paste0("Finished at ",Sys.time()))

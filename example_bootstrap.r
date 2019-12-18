#############################################
#               Load packages 
#############################################

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(RNOmni)

#############################################
#               read in data 
#############################################

args=commandArgs(trailingOnly=TRUE)
print(paste0("Number of bootstrap iterations:",args[1]))
trials = as.numeric(as.character(args[1]))

setwd("/data/Wolfson-UKBB-Dobson/PD_pheno")
#setwd("H:/UKB_PD")

selected_vars = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/ukb_pheno_final_PD_1012")
#selected_vars = read_tsv("H:/UKB_PD/ukb_pheno_final_612")
biochem = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/ukb_pheno_final_with_biochem")

#biochem = read_tsv("H:/UKB_PD//ukb_pheno_final_with_biochem")


#redefine PD status based on first report field
table(biochem$PD_status)
biochem=biochem %>% select(EID,`Date of parkinson's disease report.0.0`,
                           Urate.0.0,
                           Testosterone.0.0,
                           SHBG.0.0,
                           `Vitamin D.0.0`,
                           Triglycerides.0.0,
                           `Lipoprotein A.0.0`,
                           `LDL direct.0.0`,
                           `HDL cholesterol.0.0`,
                           `Glycated haemoglobin (HbA1c).0.0`,
                           Cholesterol.0.0,
                           `Frequency of hard/lumpy stools in the last 3 months.0.0`,
                           `Handedness (chirality/laterality).0.0`,
                           `Coffee intake.0.0`,
                           `Source of parkinson's disease report.0.0`)
selected_vars = selected_vars %>% left_join(biochem,by="EID")
rm(biochem)
#############################################
#               Get vars in right format 
#############################################

selected_vars$PD_status=factor(selected_vars$PD_status)
selected_vars$Sex.0.0 = factor(selected_vars$Sex.0.0)
selected_vars$`Year of birth.0.0` = as.numeric(selected_vars$`Year of birth.0.0`)
selected_vars$`Month of birth.0.0` = factor(selected_vars$`Month of birth.0.0`)
selected_vars$`Place of birth in UK - north co-ordinate.0.0` = as.numeric(selected_vars$`Place of birth in UK - north co-ordinate.0.0`)
df1= selected_vars %>% 
  filter(`Place of birth in UK - north co-ordinate.0.0`<0) %>%
  mutate (`Place of birth in UK - north co-ordinate.0.0` = NA)
selected_vars = df1 %>% bind_rows(selected_vars %>% filter(!(EID %in% df1$EID)))

df1= selected_vars %>% 
  filter(`Age completed full time education.0.0`<0) %>%
  mutate (`Age completed full time education.0.0` = NA)
selected_vars = df1 %>% bind_rows(selected_vars %>% filter(!(EID %in% df1$EID)))
selected_vars$smoking_status = factor(selected_vars$`Smoking status.0.0`,levels=c("Never","Current","Previous"))

selected_vars$`Country of birth (UK/elsewhere).0.0`=factor(recode((factor(selected_vars$`Country of birth (UK/elsewhere).0.0`)),
                                                                  "England"="UK",
                                                                  "Republic of Ireland"="UK",
                                                                  "Northern Ireland"="UK",
                                                                  "Scotland"="UK",
                                                                  "Wales"="UK",
                                                                  "Elsewhere"="Non-UK",
                                                                  "Do not know"="NA",
                                                                  "Prefer not to answer"="NA"),levels=c("UK","Non-UK"))


selected_vars$`Breastfed as a baby.0.0` = factor(selected_vars$ `Breastfed as a baby.0.0`,levels=c("No","Yes"))
selected_vars$`Comparative body size at age 10.0.0` = factor(selected_vars$ `Comparative body size at age 10.0.0`, levels=c("Thinner","About average","Plumper"))
selected_vars$`Maternal smoking around birth.0.0` = factor(selected_vars$`Maternal smoking around birth.0.0`,levels=c("No","Yes"))
selected_vars$`Ethnic background.0.0` = factor(selected_vars$`Ethnic background.0.0`)
selected_vars$raw_ethnicity=selected_vars$`Ethnic background.0.0`
selected_vars$`Ethnic background.0.0`=factor(recode(selected_vars$`Ethnic background.0.0`,African="Non-white",`Any other Asian background`="Non-white",`Other ethnic group`="Non-white",`White and Black Caribbean`="Non-white",`White and Asian`="Non-white",`Any other Black background`="Non-white",`White and Black African`="Non-white",`Any other mixed background`="Non-white",`Bangladeshi`="Non-white",`British Caribbean`="Non-white",`Pakistani`="Non-white",`Indian`="Non-white",`Caribbean`="Non-white",`Chinese`="Non-white",`British`="White",`Irish`="White",`Any other white background`="White",`Do not know`="NA",`Prefer not to answer`="NA"),levels=c("White","Non-white"))
selected_vars$`Daytime dozing / sleeping (narcolepsy).0.0`=factor(selected_vars$`Daytime dozing / sleeping (narcolepsy).0.0`,levels=c("All of the time","Sometimes","Often","Never/rarely"))
selected_vars$`Alcohol intake frequency..0.0`=factor(selected_vars$`Alcohol intake frequency..0.0`,levels=c("Daily or almost daily","Never","Once or twice a week","One to three times a month","Special occasions only","Three or four times a week"))
selected_vars$alcohol=recode(selected_vars$`Alcohol intake frequency..0.0`,
                             "Daily or almost daily"="Once a week or more",
                             "Never"="Less than once a week",
                             "Once or twice a week"="Once a week or more",
                             "One to three times a month"="Less than once a week",
                             "Special occasions only"="Less than once a week",
                             "Three or four times a week"="Once a week or more")
selected_vars$sleepiness=recode(selected_vars$`Daytime dozing / sleeping (narcolepsy).0.0`,
                                'All of the time'="Yes",
                                'Sometimes'="Yes",
                                'Often'="Yes",
                                'Never/rarely'="No")
selected_vars$sleepiness=relevel(selected_vars$sleepiness,ref="No")

selected_vars$`Comparative body size at age 10.0.0`=recode(selected_vars$`Comparative body size at age 10.0.0`,"Thinner"="Not overweight","About average"="Not overweight","Plumper"="Overweight")


selected_vars$`Age when periods started (menarche).0.0` = as.numeric(selected_vars$`Age when periods started (menarche).0.0`)
df1= selected_vars %>% 
  filter(`Age when periods started (menarche).0.0`<0) %>%
  mutate (`Age when periods started (menarche).0.0` = NA)
selected_vars = df1 %>% bind_rows(selected_vars %>% filter(!(EID %in% df1$EID)))


selected_vars$`Relative age voice broke.0.0` = factor(selected_vars$`Relative age voice broke.0.0`,
                                                      levels=c("About average age", "Younger than average","Older than average"))

selected_vars$handedness=factor(selected_vars$`Handedness (chirality/laterality).0.0`,levels=c("Left-handed","Right-handed"))
df1= selected_vars %>% 
  filter(`Coffee intake.0.0`<0) %>%
  mutate (`Coffee intake.0.0` = NA)
selected_vars = df1 %>% bind_rows(selected_vars %>% filter(!(EID %in% df1$EID)))

selected_vars$PD_status = factor(selected_vars$PD_status)


selected_vars = selected_vars %>% 
  mutate("dob"=as.numeric(as.Date(paste0(`Year of birth.0.0`,"-01-01")))) %>% 
  mutate("dodx"=as.numeric(`Date of parkinson's disease report.0.0`)) %>%
  mutate("age_at_pd_dx"=(dodx-dob)/365)


# incident cases
incident_cases = selected_vars %>% filter(PD_status==1 & age_at_pd_dx>=`Age at recruitment.0.0`) %>% mutate("PD_Dx_after_rec"=1)
non_incident_cases = selected_vars %>% filter(!PD_status==1) %>% filter(!EID %in% incident_cases$EID) %>% mutate("PD_Dx_after_rec"=0)
cases_after_rec = selected_vars %>% filter(PD_status==1) %>% filter(!EID %in% incident_cases$EID) %>% mutate("PD_Dx_after_rec"=NA)
selected_vars = bind_rows(incident_cases,non_incident_cases,cases_after_rec)

# filter out incident cases
depression = selected_vars %>% filter(Depression==1 & age_at_depression_diagnosis<=`Age at recruitment.0.0`) %>% mutate("depression_Dx_pre_rec"=1)
no_depression = selected_vars %>% filter(!EID %in% depression$EID) %>% mutate("depression_Dx_pre_rec"=0)
selected_vars = bind_rows(depression,no_depression)

epilepsy = selected_vars %>% filter(Epilepsy_status==1 & age_at_epilepsy_diagnosis<=`Age at recruitment.0.0`) %>% mutate("epilepsy_Dx_pre_rec"=1)
no_epilepsy = selected_vars %>% filter(!EID %in% epilepsy$EID) %>% mutate("epilepsy_Dx_pre_rec"=0)
selected_vars = bind_rows(epilepsy,no_epilepsy)

dm = selected_vars %>% filter(DM_status==1 & age_at_dm_diagnosis<=`Age at recruitment.0.0`) %>% mutate("DM_Dx_pre_rec"=1)
no_dm = selected_vars %>% filter(!EID %in% dm$EID) %>% mutate("DM_Dx_pre_rec"=0)
selected_vars = bind_rows(dm,no_dm)

Gastric_ulcer = selected_vars %>% filter(Gastric_ulcer_status==1 & age_at_Gastric_ulcer_diagnosis<=`Age at recruitment.0.0`) %>% mutate("Gastric_ulcer_Dx_pre_rec"=1)
no_Gastric_ulcer = selected_vars %>% filter(!EID %in% Gastric_ulcer$EID) %>% mutate("Gastric_ulcer_Dx_pre_rec"=0)
selected_vars = bind_rows(Gastric_ulcer,no_Gastric_ulcer)

# get drug definitions (NSAIDs, CCBs, Beta-blockers)
#drugs = read_tsv("/data/Wolfson-UKBB-Dobson/PD_pheno/ukb_pheno.tsv")
#colnames(drugs)[1]="EID"
#selected_vars = left_join(selected_vars,drugs,by="EID")

#prs
library(rcompanion)
library(ROCR)
library(RNOmni)

# constipation
# hi
# anxiety
# smoking

selected_vars = selected_vars %>% filter(`Genetic ethnic grouping.0.0`=="Caucasian")
selected_vars = selected_vars %>% filter(`Ethnic background.0.0`=="White")
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.005")

#prs = read_table2("H:/UKB_PD/r2_08/summarised_PRS_results_pval0.005")
selected_vars = selected_vars %>% select(-contains("prs"))
selected_vars$EID = as.numeric(as.character(selected_vars$EID))
selected_vars = selected_vars %>% left_join(prs,by="EID")
selected_vars = selected_vars %>% filter(!(is.na(PRS)))
selected_vars$PRS=rankNorm(selected_vars$PRS)

# interaction with PRS as continuous var
selected_vars$smoking_status=recode(selected_vars$smoking_status,"Previous"="Ever","Current"="Ever","Never"="Never")



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

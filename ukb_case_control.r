#############################################
#               Load packages 
#############################################
library(Hmisc)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(RNOmni)
library(rcompanion)
library(ROCR)
library(RNOmni)
library(caret)
#############################################
#               read in data 
#############################################
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

summary(selected_vars$`Age completed full time education.0.0`)

df1= bind_rows(selected_vars %>% 
  filter(`Age completed full time education.0.0`<5) %>%
  mutate (`Age completed full time education.0.0` = NA),
  selected_vars %>% 
    filter(`Age completed full time education.0.0`>`Age at recruitment.0.0`) %>%
    mutate (`Age completed full time education.0.0` = NA))
selected_vars = df1 %>% bind_rows(selected_vars %>% filter(!(EID %in% df1$EID)))

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
print("Sum stats for age at dx")
summary(selected_vars$age_at_pd_dx)


# incident cases
incident_cases = selected_vars %>% filter(PD_status==1 & age_at_pd_dx>=`Age at recruitment.0.0`) %>% mutate("PD_Dx_after_rec"=1)
non_incident_cases = selected_vars %>% filter(!PD_status==1) %>% filter(!EID %in% incident_cases$EID) %>% mutate("PD_Dx_after_rec"=0)
cases_after_rec = selected_vars %>% filter(PD_status==1) %>% filter(!EID %in% incident_cases$EID) %>% mutate("PD_Dx_after_rec"=NA)
selected_vars = bind_rows(incident_cases,non_incident_cases,cases_after_rec)

print("Printing number of incident cases")
table(selected_vars$PD_Dx_after_rec)
print("Printing number of prevalent cases")
table(selected_vars$PD_status)

# get drug definitions (NSAIDs, CCBs, Beta-blockers)
#drugs = read_tsv("/data/Wolfson-UKBB-Dobson/PD_pheno/ukb_pheno.tsv")
#colnames(drugs)[1]="EID"
#selected_vars = left_join(selected_vars,drugs,by="EID")
#############################################
#     Work out basic demographics 
#############################################
summ1 = cbind(rbind(
  table(selected_vars$Sex.0.0,selected_vars$PD_status),
   table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_status),
  table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_status)),
  c(table(selected_vars$Sex.0.0,selected_vars$PD_status)[,1]/sum(table(selected_vars$Sex.0.0,selected_vars$PD_status)[,1]),
    table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_status)[,1]/sum(table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_status)[,1]),
    table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_status)[,1]/sum(table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_status)[,1])),
  c(table(selected_vars$Sex.0.0,selected_vars$PD_status)[,2]/sum(table(selected_vars$Sex.0.0,selected_vars$PD_status)[,2]),
    table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_status)[,2]/sum(table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_status)[,2]),
    table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_status)[,2]/sum(table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_status)[,2])))
summ1=data.frame(summ1)
summ1$Trait=rownames(summ1)    
summ1$Control=paste(summ1$X0," (",round(summ1$V3*100,2),"%)")
summ1$Cases=paste(summ1$X1," (",round(summ1$V4*100,2),"%)")
summ1=summ1[,c(5:7)]
rownames(summ1)=NULL
summ2 = selected_vars %>% group_by(PD_status) %>%
  summarise('Age'=mean(`Age at recruitment.0.0`,na.rm=TRUE),
            'Age SD'=sd(`Age at recruitment.0.0`,na.rm=TRUE),
            'Age completed full-time education'=mean(`Age completed full time education.0.0`,na.rm=TRUE),
            'Age completed full-time education SD'=sd(`Age completed full time education.0.0`,na.rm=TRUE),
            'Townsend deprivation index'=mean(`Townsend deprivation index at recruitment.0.0`,na.rm=TRUE),
            'Townsend deprivation index SD'=sd(`Townsend deprivation index at recruitment.0.0`,na.rm=TRUE))
summ2 = data.frame(t(data.matrix(summ2)))
summ2=data.frame("Trait"=c("Age","Age completed full-time education","Townsend deprivation index"),
                 "Control"=c(paste0(round(summ2$X1[2],2)," (",round(summ2$X1[3],2),")"),
                             paste0(round(summ2$X1[4],2)," (",round(summ2$X1[5],2),")"),
                             paste0(round(summ2$X1[6],2)," (",round(summ2$X1[7],2),")")),
                 "Cases"=c(paste0(round(summ2$X2[2],2)," (",round(summ2$X2[3],2),")"),
                           paste0(round(summ2$X2[4],2)," (",round(summ2$X2[5],2),")"),
                           paste0(round(summ2$X2[6],2)," (",round(summ2$X2[7],2),")")))

summ3 = rbind(summ1,summ2)
write.csv(summ3,"supplementary_table_2.csv")
table(selected_vars$PD_status)
table(selected_vars$PD_Dx_after_rec)
# univariate comparisons

pd=selected_vars %>% filter(PD_status==1)
no_pd=selected_vars %>% filter(PD_status==0)
pvals = data.frame("Sex"=chisq.test(table(selected_vars$Sex.0.0,selected_vars$PD_status))$p.value,
"Ethnicity"=chisq.test(table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_status))$p.value,
"Age"=t.test(pd$`Age at recruitment.0.0`,no_pd$`Age at recruitment.0.0`)$p.value,
"Deprivation"=t.test(pd$`Townsend deprivation index at recruitment.0.0`,no_pd$`Townsend deprivation index at recruitment.0.0`)$p.value,
"Education years"=t.test(pd$`Age completed full time education.0.0`,no_pd$`Age completed full time education.0.0`)$p.value,
"Country of birth"=chisq.test(table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_status))$p.value)
pvals
write_csv(pvals,"prevalent_pvals.csv")

# repeat for incident cases
#############################################
#     Work out basic demographics 
#############################################
summ1 = cbind(rbind(
  table(selected_vars$Sex.0.0,selected_vars$PD_Dx_after_rec),
   table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_Dx_after_rec),
  table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_Dx_after_rec)),
  c(table(selected_vars$Sex.0.0,selected_vars$PD_Dx_after_rec)[,1]/sum(table(selected_vars$Sex.0.0,selected_vars$PD_Dx_after_rec)[,1]),
    table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_Dx_after_rec)[,1]/sum(table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_Dx_after_rec)[,1]),
    table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_Dx_after_rec)[,1]/sum(table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_Dx_after_rec)[,1])),
  c(table(selected_vars$Sex.0.0,selected_vars$PD_Dx_after_rec)[,2]/sum(table(selected_vars$Sex.0.0,selected_vars$PD_Dx_after_rec)[,2]),
    table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_Dx_after_rec)[,2]/sum(table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_Dx_after_rec)[,2]),
    table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_Dx_after_rec)[,2]/sum(table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_Dx_after_rec)[,2])))
summ1=data.frame(summ1)
summ1$Trait=rownames(summ1)    
summ1$Control=paste(summ1$X0," (",round(summ1$V3*100,2),"%)")
summ1$Cases=paste(summ1$X1," (",round(summ1$V4*100,2),"%)")
summ1=summ1[,c(5:7)]
rownames(summ1)=NULL
summ2 = selected_vars %>% group_by(PD_Dx_after_rec) %>%
  summarise('Age'=mean(`Age at recruitment.0.0`,na.rm=TRUE),
            'Age SD'=sd(`Age at recruitment.0.0`,na.rm=TRUE),
            'Age completed full-time education'=mean(`Age completed full time education.0.0`,na.rm=TRUE),
            'Age completed full-time education SD'=sd(`Age completed full time education.0.0`,na.rm=TRUE),
            'Townsend deprivation index'=mean(`Townsend deprivation index at recruitment.0.0`,na.rm=TRUE),
            'Townsend deprivation index SD'=sd(`Townsend deprivation index at recruitment.0.0`,na.rm=TRUE))
summ2 = data.frame(t(data.matrix(summ2)))
summ2=data.frame("Trait"=c("Age","Age completed full-time education","Townsend deprivation index"),
                 "Control"=c(paste0(round(summ2$X1[2],2)," (",round(summ2$X1[3],2),")"),
                             paste0(round(summ2$X1[4],2)," (",round(summ2$X1[5],2),")"),
                             paste0(round(summ2$X1[6],2)," (",round(summ2$X1[7],2),")")),
                 "Cases"=c(paste0(round(summ2$X2[2],2)," (",round(summ2$X2[3],2),")"),
                           paste0(round(summ2$X2[4],2)," (",round(summ2$X2[5],2),")"),
                           paste0(round(summ2$X2[6],2)," (",round(summ2$X2[7],2),")")))

summ3 = rbind(summ1,summ2)
write.csv(summ3,"supplementary_table_2b_incident.csv")
table(selected_vars$PD_Dx_after_rec)
table(selected_vars$PD_Dx_after_rec)

# univariate comparisons

pd=selected_vars %>% filter(PD_Dx_after_rec==1)
no_pd=selected_vars %>% filter(PD_Dx_after_rec==0)
pvals = data.frame("Sex"=chisq.test(table(selected_vars$Sex.0.0,selected_vars$PD_Dx_after_rec))$p.value,
"Ethnicity"=chisq.test(table(selected_vars$`Ethnic background.0.0`,selected_vars$PD_Dx_after_rec))$p.value,
"Age"=t.test(pd$`Age at recruitment.0.0`,no_pd$`Age at recruitment.0.0`)$p.value,
"Deprivation"=t.test(pd$`Townsend deprivation index at recruitment.0.0`,no_pd$`Townsend deprivation index at recruitment.0.0`)$p.value,
"Education years"=t.test(pd$`Age completed full time education.0.0`,no_pd$`Age completed full time education.0.0`)$p.value,
"Country of birth"=chisq.test(table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$PD_Dx_after_rec))$p.value)
pvals
write_csv(pvals,"incident_pvals.csv")



#######################################
# incident cases
#######################################


#############################################
#     Build multivariable models 
#############################################
smok=glm(data=selected_vars,
         PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
           selected_vars$smoking_status,
         family=binomial(link="logit"))
lr_df = data.frame('Exposure'="Smoking","P-value"=anova(smok,test="Chisq")$`Pr(>Chi)`[6])

pest=glm(data=selected_vars,
         PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
           factor(selected_vars$Pesticides),
         family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Pesticides","P-value"=anova(pest,test="Chisq")$`Pr(>Chi)`[6]))


alc=glm(data=selected_vars,
        PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(selected_vars$alcohol),
        family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Alcohol","P-value"=anova(alc,test="Chisq")$`Pr(>Chi)`[6]))

# filter out incident cases
dm = selected_vars %>% filter(DM_status==1 & age_at_dm_diagnosis<=`Age at recruitment.0.0`) %>% mutate("DM_Dx_pre_rec"=1)
no_dm = selected_vars %>% filter(!EID %in% dm$EID) %>% mutate("DM_Dx_pre_rec"=0)
selected_vars = bind_rows(dm,no_dm)

dm=glm(data=selected_vars,
       PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
         factor(selected_vars$DM_Dx_pre_rec),
       family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="DM","P-value"=anova(dm,test="Chisq")$`Pr(>Chi)`[6]))

# filter out incident cases
epilepsy = selected_vars %>% filter(Epilepsy_status==1 & age_at_epilepsy_diagnosis<=`Age at recruitment.0.0`) %>% mutate("epilepsy_Dx_pre_rec"=1)
no_epilepsy = selected_vars %>% filter(!EID %in% epilepsy$EID) %>% mutate("epilepsy_Dx_pre_rec"=0)
selected_vars = bind_rows(epilepsy,no_epilepsy)
epi=glm(data=selected_vars,
        PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(selected_vars$epilepsy_Dx_pre_rec),
        family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Epilepsy","P-value"=anova(epi,test="Chisq")$`Pr(>Chi)`[6]))


sleepiness=glm(data=selected_vars,
               PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 factor(selected_vars$sleepiness),
               family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Excessive daytime sleepiness","P-value"=anova(sleepiness,test="Chisq")$`Pr(>Chi)`[6]))



bmi=glm(data=selected_vars,
        PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          selected_vars$`Body mass index (BMI).0.0`,
        family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="BMI","P-value"=anova(bmi,test="Chisq")$`Pr(>Chi)`[6]))



cbmi=glm(data=selected_vars,
         PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
           factor(selected_vars$`Comparative body size at age 10.0.0`),
         family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Childhood BMI","P-value"=anova(cbmi,test="Chisq")$`Pr(>Chi)`[6]))

# filter out incident cases
depression = selected_vars %>% filter(Depression==1 & age_at_depression_diagnosis<=`Age at recruitment.0.0`) %>% mutate("depression_Dx_pre_rec"=1)
no_depression = selected_vars %>% filter(!EID %in% depression$EID) %>% mutate("depression_Dx_pre_rec"=0)
selected_vars = bind_rows(depression,no_depression)
dep=glm(data=selected_vars,
        PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(selected_vars$depression_Dx_pre_rec),
        family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Depression","P-value"=anova(dep,test="Chisq")$`Pr(>Chi)`[6]))

# filter out incident cases
anxiety = selected_vars %>% filter(Anxiety==1 & age_at_anxiety_diagnosis<=`Age at recruitment.0.0`) %>% mutate("Anxiety_Dx_pre_rec"=1)
no_anxiety = selected_vars %>% filter(!EID %in% anxiety$EID) %>% mutate("Anxiety_Dx_pre_rec"=0)
selected_vars = bind_rows(anxiety,no_anxiety)

anx=glm(data=selected_vars,
        PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(selected_vars$Anxiety_Dx_pre_rec),
        family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Anxiety","P-value"=anova(anx,test="Chisq")$`Pr(>Chi)`[6]))


# filter out incident cases
migraine = selected_vars %>% filter(Migraine==1 & age_at_migraine_diagnosis<=`Age at recruitment.0.0`) %>% mutate("migraine_Dx_pre_rec"=1)
no_migraine = selected_vars %>% filter(!EID %in% migraine$EID) %>% mutate("migraine_Dx_pre_rec"=0)
selected_vars = bind_rows(migraine,no_migraine)

mig=glm(data=selected_vars,
        PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(selected_vars$migraine_Dx_pre_rec),
        family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Migraine","P-value"=anova(mig,test="Chisq")$`Pr(>Chi)`[6]))


# filter out incident cases
constipation = selected_vars %>% filter(Constipation_status==1 & age_at_constipation_diagnosis<=`Age at recruitment.0.0`) %>% mutate("constipation_Dx_pre_rec"=1)
no_constipation = selected_vars %>% filter(!EID %in% constipation$EID) %>% mutate("constipation_Dx_pre_rec"=0)
selected_vars = bind_rows(constipation,no_constipation)

constipation=glm(data=selected_vars,
                 PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(selected_vars$constipation_Dx_pre_rec),
                 family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Constipation","P-value"=anova(constipation,test="Chisq")$`Pr(>Chi)`[6]))


# filter out incident cases
head_injury = selected_vars %>% filter(Head_injury_status==1 & age_at_head_injury_diagnosis<=`Age at recruitment.0.0`) %>% mutate("head_injury_Dx_pre_rec"=1)
no_head_injury = selected_vars %>% filter(!EID %in% head_injury$EID) %>% mutate("head_injury_Dx_pre_rec"=0)
selected_vars = bind_rows(head_injury,no_head_injury)
head_injury=glm(data=selected_vars,
                PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                  factor(selected_vars$head_injury_Dx_pre_rec),
                family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Head injury","P-value"=anova(head_injury,test="Chisq")$`Pr(>Chi)`[6]))

# filter out incident cases
Hypertension = selected_vars %>% filter(Hypertension_status==1 & age_at_Hypertension_diagnosis<=`Age at recruitment.0.0`) %>% mutate("Hypertension_Dx_pre_rec"=1)
no_Hypertension = selected_vars %>% filter(!EID %in% Hypertension$EID) %>% mutate("Hypertension_Dx_pre_rec"=0)
selected_vars = bind_rows(Hypertension,no_Hypertension)
Hypertension=glm(data=selected_vars,
                 PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(selected_vars$Hypertension_Dx_pre_rec),
                 family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Hypertension","P-value"=anova(Hypertension,test="Chisq")$`Pr(>Chi)`[6]))


Gastric_ulcer = selected_vars %>% filter(Gastric_ulcer_status==1 & age_at_Gastric_ulcer_diagnosis<=`Age at recruitment.0.0`) %>% mutate("Gastric_ulcer_Dx_pre_rec"=1)
no_Gastric_ulcer = selected_vars %>% filter(!EID %in% Gastric_ulcer$EID) %>% mutate("Gastric_ulcer_Dx_pre_rec"=0)
selected_vars = bind_rows(Gastric_ulcer,no_Gastric_ulcer)
Gastric_ulcer=glm(data=selected_vars,
                  PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                    factor(selected_vars$Gastric_ulcer_Dx_pre_rec),
                  family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="PUD","P-value"=anova(Gastric_ulcer,test="Chisq")$`Pr(>Chi)`[6]))


PD_FHx=glm(data=selected_vars,
           PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
             factor(selected_vars$PD_FHx),
           family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="PD FHx","P-value"=anova(PD_FHx,test="Chisq")$`Pr(>Chi)`[6]))


Dementia_FHx=glm(data=selected_vars,
                 PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(selected_vars$Dementia_FHx),
                 family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Dementia FHx","P-value"=anova(Dementia_FHx,test="Chisq")$`Pr(>Chi)`[6]))

Depression_FHx=glm(data=selected_vars,
                   PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                     factor(selected_vars$Depression_FHx),
                   family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Depression FHx","P-value"=anova(Depression_FHx,test="Chisq")$`Pr(>Chi)`[6]))


Diabetes_FHx=glm(data=selected_vars,
                 PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(selected_vars$Diabetes_FHx),
                 family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="DM FHx","P-value"=anova(Diabetes_FHx,test="Chisq")$`Pr(>Chi)`[6]))


Stroke_FHx=glm(data=selected_vars,
               PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 factor(selected_vars$Stroke_FHx),
               family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Stroke FHx","P-value"=anova(Stroke_FHx,test="Chisq")$`Pr(>Chi)`[6]))

breastfed=glm(data=selected_vars,
              PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                selected_vars$`Breastfed as a baby.0.0`,
              family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Breastfed","P-value"=anova(breastfed,test="Chisq")$`Pr(>Chi)`[6]))

mat_smok=glm(data=selected_vars,
             PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
               selected_vars$`Maternal smoking around birth.0.0`,
             family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Maternal smoking","P-value"=anova(mat_smok,test="Chisq")$`Pr(>Chi)`[6]))

handedness=glm(data=selected_vars,
               PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 selected_vars$handedness,
               family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Handedness","P-value"=anova(handedness,test="Chisq")$`Pr(>Chi)`[6]))


menarche=glm(data=selected_vars,
             PD_Dx_after_rec~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
               selected_vars$`Age when periods started (menarche).0.0`,
             family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Age at menarche","P-value"=anova(menarche,test="Chisq")$`Pr(>Chi)`[5]))


voicebreak=glm(data=selected_vars,
               PD_Dx_after_rec~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 selected_vars$`Relative age voice broke.0.0`,
               family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Age at voicebreaking","P-value"=anova(voicebreak,test="Chisq")$`Pr(>Chi)`[5]))

coffee=glm(data=selected_vars,
           PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
             selected_vars$`Coffee intake.0.0`,
           family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Coffee intake","P-value"=anova(coffee,test="Chisq")$`Pr(>Chi)`[6]))

edu=glm(data=selected_vars,
        PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars$`Age completed full time education.0.0`,
        family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Education","P-value"=anova(edu,test="Chisq")$`Pr(>Chi)`[6]))


df=data.frame(rbind(summary(Diabetes_FHx)$coefficients,
                    summary(Stroke_FHx)$coefficients,
                    summary(Dementia_FHx)$coefficients,
                    summary(Depression_FHx)$coefficients,
                    summary(PD_FHx)$coefficients,
                    summary(head_injury)$coefficients,
                    summary(Hypertension)$coefficients,
                    summary(mig)$coefficients,
                    summary(anx)$coefficients,
                    summary(dep)$coefficients,
                    summary(epi)$coefficients,
                    summary(dm)$coefficients,
                    summary(Gastric_ulcer)$coefficients,
                    summary(sleepiness)$coefficients,
                    summary(constipation)$coefficients,
                    summary(cbmi)$coefficients,
                    summary(bmi)$coefficients,
                    summary(alc)$coefficients,
                    summary(pest)$coefficients,
                    summary(smok)$coefficients,
                    summary(voicebreak)$coefficients,
                    summary(menarche)$coefficients,
                    summary(handedness)$coefficients,
                    summary(breastfed)$coefficients,
                    summary(mat_smok)$coefficients,
                    summary(coffee)$coefficients,
                    summary(edu)$coefficients))

df$name=rownames(df)
df=df[order(df$name),]
df=df[c(1:29),]
df$name=c("Childhood obesity",
          "Alcohol: <1 drink per week",
          "Anxiety",
          "Constipation",
          "Family History: dementia" ,
          "Depression",
          "Family history: depression",
          "Family history:diabetes",
          "Diabetes",
          "Epilepsy",
          "Gastric ulcer",
          "Head Injury",
          "Hypertension",
          "Migraine",
          "Family history: PD",
          "Pesticide exposure",
          "Excessive daytime sleepiness",
          "Family history: stroke",
          "Age completed full time education",
          "Age at menarche",
          "BMI",
          "Breastfed as a baby",
          "Cups of coffee per day",
          "Exposed to maternal smoking",
          "Age at voice breaking: older than average",
          "Age at voice breaking: younger than average",
          "Right-handed",
          "Smoking status: current",
          "Smoking status: previous")
rownames(df)=NULL
df$cat=c("Early life",
"Environmental",
"Comorbidity",
"Prodrome",
"Family history",
"Comorbidity",
"Family history",
"Family history",
"Comorbidity",
"Comorbidity",
"Comorbidity",
"Comorbidity",
"Comorbidity",
"Comorbidity",
"Family history",
"Environmental",
"Prodrome",
"Family history",
"Early life",
"Early life",
"Environmental",
"Early life",
"Environmental",
"Early life",
"Early life",
"Early life",
"Early life",
"Environmental",
"Environmental")
df = df[order(df$cat),]
df$num = factor(c(1:nrow(df)))
factor(df$name)
p=ggplot(df,aes(Estimate,num,fill=cat))+
  geom_errorbarh(mapping=aes(y=num,xmin=Estimate-1.96*Std..Error,xmax=Estimate+1.96*Std..Error),height=0.3)+
  geom_vline(xintercept=0,alpha=0.3)+
  geom_point(size=5,shape=22)+
  scale_fill_brewer(palette="Set2")+
  labs(x="Log(OR) for PD",y="Risk factor",fill="")+
  theme_classic()+
  scale_y_discrete(labels=df$name)


png(file="incident_case_control.png",height=12,width=12,res=300,units="in")
p
dev.off()
write_csv(df,"table_1.csv")
lr_df$fdr=p.adjust(lr_df$P.value,method="fdr")
write_csv(lr_df,"table_1b.csv")

multivar_model = glm(data=selected_vars,
                     PD_Dx_after_rec~`Sex.0.0`+`Ethnic background.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                       factor(selected_vars$PD_FHx)+alcohol+factor(selected_vars$depression_Dx_pre_rec)+factor(epilepsy_Dx_pre_rec)+sleepiness+smoking_status+factor(selected_vars$Dementia_FHx),
                     family=binomial(link="logit"))
multi = data.frame(summary(multivar_model)$coefficients)
multi$name = rownames(multi)
write_csv(multi,"multivariate.csv")

multivar_model = glm(data=selected_vars,
                     PD_Dx_after_rec~`Age when periods started (menarche).0.0`+`Ethnic background.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+factor(selected_vars$PD_FHx)+alcohol+factor(selected_vars$depression_Dx_pre_rec)+factor(epilepsy_Dx_pre_rec)+sleepiness+smoking_status+factor(selected_vars$Dementia_FHx),family=binomial(link="logit"))
multi = data.frame(summary(multivar_model)$coefficients)
multi$name = rownames(multi)
write_csv(multi,"multivariate_f.csv")

##########################################
# sensitivity analyses
########################################

#############################################
#     1) remove non-white individuals 
#############################################

white_only_selected_vars = selected_vars %>% filter(`Ethnic background.0.0`=="White")

smok=glm(data=white_only_selected_vars,
         PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
           white_only_selected_vars$smoking_status,
         family=binomial(link="logit"))

pest=glm(data=white_only_selected_vars,
         PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
           factor(white_only_selected_vars$Pesticides),
         family=binomial(link="logit"))

alc=glm(data=white_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(white_only_selected_vars$alcohol),
        family=binomial(link="logit"))

dm=glm(data=white_only_selected_vars,
       PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
         factor(white_only_selected_vars$DM_Dx_pre_rec),
       family=binomial(link="logit"))

epi=glm(data=white_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(white_only_selected_vars$epilepsy_Dx_pre_rec),
        family=binomial(link="logit"))
        
sleepiness=glm(data=white_only_selected_vars,
               PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 factor(white_only_selected_vars$sleepiness),
               family=binomial(link="logit"))

bmi=glm(data=white_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          white_only_selected_vars$`Body mass index (BMI).0.0`,
        family=binomial(link="logit"))

cbmi=glm(data=white_only_selected_vars,
         PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
           factor(white_only_selected_vars$`Comparative body size at age 10.0.0`),
         family=binomial(link="logit"))

dep=glm(data=white_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(white_only_selected_vars$depression_Dx_pre_rec),
        family=binomial(link="logit"))

anx=glm(data=white_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(white_only_selected_vars$Anxiety_Dx_pre_rec),
        family=binomial(link="logit"))

mig=glm(data=white_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(white_only_selected_vars$migraine_Dx_pre_rec),
        family=binomial(link="logit"))

constipation=glm(data=white_only_selected_vars,
                 PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(white_only_selected_vars$constipation_Dx_pre_rec),
                 family=binomial(link="logit"))

head_injury=glm(data=white_only_selected_vars,
                PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                  factor(white_only_selected_vars$head_injury_Dx_pre_rec),
                family=binomial(link="logit"))

Hypertension=glm(data=white_only_selected_vars,
                 PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(white_only_selected_vars$Hypertension_Dx_pre_rec),
                 family=binomial(link="logit"))

Gastric_ulcer=glm(data=white_only_selected_vars,
                  PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                    factor(white_only_selected_vars$Gastric_ulcer_Dx_pre_rec),
                  family=binomial(link="logit"))

PD_FHx=glm(data=white_only_selected_vars,
           PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
             factor(white_only_selected_vars$PD_FHx),
           family=binomial(link="logit"))

Dementia_FHx=glm(data=white_only_selected_vars,
                 PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(white_only_selected_vars$Dementia_FHx),
                 family=binomial(link="logit"))

Depression_FHx=glm(data=white_only_selected_vars,
                   PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                     factor(white_only_selected_vars$Depression_FHx),
                   family=binomial(link="logit"))

Diabetes_FHx=glm(data=white_only_selected_vars,
                 PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(white_only_selected_vars$Diabetes_FHx),
                 family=binomial(link="logit"))

Stroke_FHx=glm(data=white_only_selected_vars,
               PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 factor(white_only_selected_vars$Stroke_FHx),
               family=binomial(link="logit"))

breastfed=glm(data=white_only_selected_vars,
              PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                white_only_selected_vars$`Breastfed as a baby.0.0`,
              family=binomial(link="logit"))

mat_smok=glm(data=white_only_selected_vars,
             PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
               white_only_selected_vars$`Maternal smoking around birth.0.0`,
             family=binomial(link="logit"))

handedness=glm(data=white_only_selected_vars,
               PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 white_only_selected_vars$handedness,
               family=binomial(link="logit"))

menarche=glm(data=white_only_selected_vars,
             PD_Dx_after_rec~`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
               white_only_selected_vars$`Age when periods started (menarche).0.0`,
             family=binomial(link="logit"))

voicebreak=glm(data=white_only_selected_vars,
               PD_Dx_after_rec~`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 white_only_selected_vars$`Relative age voice broke.0.0`,
               family=binomial(link="logit"))

coffee=glm(data=white_only_selected_vars,
           PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
             white_only_selected_vars$`Coffee intake.0.0`,
           family=binomial(link="logit"))

edu=glm(data=white_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+white_only_selected_vars$`Age completed full time education.0.0`,
        family=binomial(link="logit"))

df=data.frame(rbind(summary(Diabetes_FHx)$coefficients,
                    summary(Stroke_FHx)$coefficients,
                    summary(Dementia_FHx)$coefficients,
                    summary(Depression_FHx)$coefficients,
                    summary(PD_FHx)$coefficients,
                    summary(head_injury)$coefficients,
                    summary(Hypertension)$coefficients,
                    summary(mig)$coefficients,
                    summary(anx)$coefficients,
                    summary(dep)$coefficients,
                    summary(epi)$coefficients,
                    summary(dm)$coefficients,
                    summary(Gastric_ulcer)$coefficients,
                    summary(sleepiness)$coefficients,
                    summary(constipation)$coefficients,
                    summary(cbmi)$coefficients,
                    summary(bmi)$coefficients,
                    summary(alc)$coefficients,
                    summary(pest)$coefficients,
                    summary(smok)$coefficients,
                    summary(voicebreak)$coefficients,
                    summary(menarche)$coefficients,
                    summary(handedness)$coefficients,
                    summary(breastfed)$coefficients,
                    summary(mat_smok)$coefficients,
                    summary(coffee)$coefficients,
                    summary(edu)$coefficients))

df$name=rownames(df)
df=df[order(df$name),]

write_csv(df,"white_people_only_incident_case_control.csv")

#############################################
#     2) remove non-white individuals 
#############################################

nonwhite_only_selected_vars = selected_vars %>% filter(`Ethnic background.0.0`=="Non-white")

smok=glm(data=nonwhite_only_selected_vars,
         PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
           nonwhite_only_selected_vars$smoking_status,
         family=binomial(link="logit"))

pest=glm(data=nonwhite_only_selected_vars,
         PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
           factor(nonwhite_only_selected_vars$Pesticides),
         family=binomial(link="logit"))

alc=glm(data=nonwhite_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(nonwhite_only_selected_vars$alcohol),
        family=binomial(link="logit"))

dm=glm(data=nonwhite_only_selected_vars,
       PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
         factor(nonwhite_only_selected_vars$DM_Dx_pre_rec),
       family=binomial(link="logit"))

epi=glm(data=nonwhite_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(nonwhite_only_selected_vars$epilepsy_Dx_pre_rec),
        family=binomial(link="logit"))
        
sleepiness=glm(data=nonwhite_only_selected_vars,
               PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 factor(nonwhite_only_selected_vars$sleepiness),
               family=binomial(link="logit"))

bmi=glm(data=nonwhite_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          nonwhite_only_selected_vars$`Body mass index (BMI).0.0`,
        family=binomial(link="logit"))

cbmi=glm(data=nonwhite_only_selected_vars,
         PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
           factor(nonwhite_only_selected_vars$`Comparative body size at age 10.0.0`),
         family=binomial(link="logit"))

dep=glm(data=nonwhite_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(nonwhite_only_selected_vars$depression_Dx_pre_rec),
        family=binomial(link="logit"))

anx=glm(data=nonwhite_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(nonwhite_only_selected_vars$Anxiety_Dx_pre_rec),
        family=binomial(link="logit"))

mig=glm(data=nonwhite_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
          factor(nonwhite_only_selected_vars$migraine_Dx_pre_rec),
        family=binomial(link="logit"))

constipation=glm(data=nonwhite_only_selected_vars,
                 PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(nonwhite_only_selected_vars$constipation_Dx_pre_rec),
                 family=binomial(link="logit"))

head_injury=glm(data=nonwhite_only_selected_vars,
                PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                  factor(nonwhite_only_selected_vars$head_injury_Dx_pre_rec),
                family=binomial(link="logit"))

Hypertension=glm(data=nonwhite_only_selected_vars,
                 PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(nonwhite_only_selected_vars$Hypertension_Dx_pre_rec),
                 family=binomial(link="logit"))

Gastric_ulcer=glm(data=nonwhite_only_selected_vars,
                  PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                    factor(nonwhite_only_selected_vars$Gastric_ulcer_Dx_pre_rec),
                  family=binomial(link="logit"))

PD_FHx=glm(data=nonwhite_only_selected_vars,
           PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
             factor(nonwhite_only_selected_vars$PD_FHx),
           family=binomial(link="logit"))

Dementia_FHx=glm(data=nonwhite_only_selected_vars,
                 PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(nonwhite_only_selected_vars$Dementia_FHx),
                 family=binomial(link="logit"))

Depression_FHx=glm(data=nonwhite_only_selected_vars,
                   PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                     factor(nonwhite_only_selected_vars$Depression_FHx),
                   family=binomial(link="logit"))

Diabetes_FHx=glm(data=nonwhite_only_selected_vars,
                 PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                   factor(nonwhite_only_selected_vars$Diabetes_FHx),
                 family=binomial(link="logit"))

Stroke_FHx=glm(data=nonwhite_only_selected_vars,
               PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 factor(nonwhite_only_selected_vars$Stroke_FHx),
               family=binomial(link="logit"))

breastfed=glm(data=nonwhite_only_selected_vars,
              PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                nonwhite_only_selected_vars$`Breastfed as a baby.0.0`,
              family=binomial(link="logit"))

mat_smok=glm(data=nonwhite_only_selected_vars,
             PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
               nonwhite_only_selected_vars$`Maternal smoking around birth.0.0`,
             family=binomial(link="logit"))

handedness=glm(data=nonwhite_only_selected_vars,
               PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 nonwhite_only_selected_vars$handedness,
               family=binomial(link="logit"))

menarche=glm(data=nonwhite_only_selected_vars,
             PD_Dx_after_rec~`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
               nonwhite_only_selected_vars$`Age when periods started (menarche).0.0`,
             family=binomial(link="logit"))

voicebreak=glm(data=nonwhite_only_selected_vars,
               PD_Dx_after_rec~`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                 nonwhite_only_selected_vars$`Relative age voice broke.0.0`,
               family=binomial(link="logit"))

coffee=glm(data=nonwhite_only_selected_vars,
           PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
             nonwhite_only_selected_vars$`Coffee intake.0.0`,
           family=binomial(link="logit"))

edu=glm(data=nonwhite_only_selected_vars,
        PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+nonwhite_only_selected_vars$`Age completed full time education.0.0`,
        family=binomial(link="logit"))

df=data.frame(rbind(summary(Diabetes_FHx)$coefficients,
                    summary(Stroke_FHx)$coefficients,
                    summary(Dementia_FHx)$coefficients,
                    summary(Depression_FHx)$coefficients,
                    summary(PD_FHx)$coefficients,
                    summary(head_injury)$coefficients,
                    summary(Hypertension)$coefficients,
                    summary(mig)$coefficients,
                    summary(anx)$coefficients,
                    summary(dep)$coefficients,
                    summary(epi)$coefficients,
                    summary(dm)$coefficients,
                    summary(Gastric_ulcer)$coefficients,
                    summary(sleepiness)$coefficients,
                    summary(constipation)$coefficients,
                    summary(cbmi)$coefficients,
                    summary(bmi)$coefficients,
                    summary(alc)$coefficients,
                    summary(pest)$coefficients,
                    summary(smok)$coefficients,
                    summary(voicebreak)$coefficients,
                    summary(menarche)$coefficients,
                    summary(handedness)$coefficients,
                    summary(breastfed)$coefficients,
                    summary(mat_smok)$coefficients,
                    summary(coffee)$coefficients,
                    summary(edu)$coefficients))
df$name=rownames(df)
df=df[order(df$name),]
write_csv(df,"nonwhite_people_only_incident_case_control.csv")



########################################
# apply predict algo
########################################


erectile_dysfunction = selected_vars %>% filter(Erectile_dysfunction_status==1 & age_at_Erectile_dysfunction_diagnosis <=`Age at recruitment.0.0`) %>% mutate("Erectile_dysfunction_status_pre_dx"=1)
no_erectile_dysfunction = selected_vars %>% filter(!EID %in% erectile_dysfunction$EID) %>% mutate("Erectile_dysfunction_status_pre_dx"=0)
selected_vars = bind_rows(no_erectile_dysfunction,erectile_dysfunction)





selected_vars$baseline_risk=1/(1+28.53049+73.67057*exp(-0.165308*(selected_vars$`Age at recruitment.0.0`-60)))
# adjust for sex
selected_vars=bind_rows(selected_vars %>%
  filter(Sex.0.0=="Female")%>%
  mutate("baseline_risk"=baseline_risk/1.5),
selected_vars %>%
  filter(Sex.0.0=="Male"))

selected_vars=bind_rows(selected_vars %>% 
  filter(`Smoking status.0.0`=="Current")%>%
  mutate("baseline_risk"=baseline_risk*0.44),
selected_vars=selected_vars %>% 
  filter(`Smoking status.0.0`=="Previous")%>%
  mutate("baseline_risk"=baseline_risk*0.78),
selected_vars=selected_vars %>% 
  filter(`Smoking status.0.0`=="Never"))

selected_vars=bind_rows(selected_vars %>%
  filter(PD_FHx=="1") %>%
  mutate("baseline_risk"=baseline_risk*4.45),
selected_vars %>% 
  filter(PD_FHx=="0"))
  
selected_vars=bind_rows(selected_vars %>% 
                          filter(`Coffee intake.0.0`>=1) %>%
                          mutate("baseline_risk"=baseline_risk*0.67),
                        selected_vars %>% 
                          filter(`Coffee intake.0.0`<1))
selected_vars=bind_rows(selected_vars %>% 
                          filter(alcohol=="Once a week or more") %>%
                          mutate("baseline_risk"=baseline_risk*0.90),
                        selected_vars %>% 
                          filter(alcohol=="Less than once a week"))

selected_vars=bind_rows(selected_vars %>% 
                          filter(constipation_Dx_pre_rec=="1") %>%
                          mutate("baseline_risk"=baseline_risk*2.34),
                        selected_vars %>% 
                          filter(constipation_Dx_pre_rec=="0"))
selected_vars=bind_rows(selected_vars %>% 
                          filter(depression_Dx_pre_rec=="1"|Anxiety_Dx_pre_rec=="1") %>%
                          mutate("baseline_risk"=baseline_risk*1.86),
                        selected_vars %>% 
                          filter(depression_Dx_pre_rec=="0"&Anxiety_Dx_pre_rec=="0"))

selected_vars=bind_rows(selected_vars %>% 
                          filter(Erectile_dysfunction_status_pre_dx=="1") %>%
                          mutate("baseline_risk"=baseline_risk*3.8),
                        selected_vars %>% 
                          filter(Erectile_dysfunction_status_pre_dx=="0"))


# that's the baseline odds of PD
# now we'll work out probability
selected_vars = selected_vars %>% mutate("predict_baseline_prob"=baseline_risk/(1+baseline_risk))
print("Printing baseline prob of PD among incident cases")
selected_vars %>% group_by(PD_Dx_after_rec) %>%
  summarise("Risk PD"=median(predict_baseline_prob,na.rm=TRUE),
            "Risk PD SD"=IQR(predict_baseline_prob,na.rm=TRUE))


p=ggplot(selected_vars[!is.na(selected_vars$PD_Dx_after_rec),],aes((predict_baseline_prob*100),fill=factor(PD_Dx_after_rec)))+
  geom_density(size=0.1,alpha=0.2)+
  theme_classic()+
  labs(x="Predicted probability of PD (%)", fill="Case/control status")+
  scale_x_log10()+
  theme(text=element_text(size=16))

png("predict_algo_incident.png",height=10,width=10,res=1000,units="in")
p  
dev.off()


predict_model = glm(data=selected_vars,PD_Dx_after_rec ~ log(baseline_risk),family=binomial(link="logit"))
# printing model fit incident PD ~ predict
summary(predict_model)
# nagel
nagelkerke(predict_model)
null_model = glm(data=selected_vars,PD_Dx_after_rec ~ `Age at recruitment.0.0`+Sex.0.0,family=binomial(link="logit"))
# nagel vs null of age + sex
nagelkerke(predict_model,null_model)

#prs
plot_table = selected_vars %>% filter(!is.na(PD_Dx_after_rec))

p = ggplot(plot_table,aes(`Genetic principal components.0.1`,`Genetic principal components.0.2`,fill=factor(PD_Dx_after_rec)))+
  geom_point(shape=22)+
  theme_bw()+
  labs(x="PC1",y="PC2",fill="PD status")

png("pc_plot_before_relatness_and_ancestry_exclusions.png",height=10,width=10,res=1000,units="in")
p
dev.off()

# exclude related people
kin = read_table2("/data/Wolfson-UKBB-Dobson/helper_progs_and_key/ukb43101_rel_s488282.dat")
exclusion = kin %>% filter(Kinship>0.0442) %>% select(ID1) %>% rename("EID"="ID1") 
selected_vars = selected_vars %>% filter(`Genetic ethnic grouping.0.0`=="Caucasian") %>% filter(!EID %in% exclusion$EID)

plot_table = selected_vars %>% filter(!is.na(PD_Dx_after_rec))
p = ggplot(plot_table,aes(`Genetic principal components.0.1`,`Genetic principal components.0.2`,fill=factor(PD_Dx_after_rec)))+
  geom_point(shape=22)+
  theme_bw()+
  labs(x="PC1",y="PC2",fill="PD status")

png("pc_plot_1.png",height=10,width=10,res=1000,units="in")
p
dev.off()

p = ggplot(plot_table,aes(`Genetic principal components.0.3`,`Genetic principal components.0.4`,fill=factor(PD_Dx_after_rec)))+
  geom_point(shape=22)+
  theme_bw()+
  labs(x="PC3",y="PC4",fill="PD status")

png("pc_plot_2.png",height=10,width=10,res=1000,units="in")
p
dev.off()

######################################
# prs
######################################
nagelkerke = c()
score_prs=function(){
  selected_vars = selected_vars %>% select(-contains("prs"))
  selected_vars = selected_vars %>% left_join(prs,by="EID")
  selected_vars = selected_vars %>% filter(!(is.na(PRS)))
  
  selected_vars = selected_vars %>% filter(!is.na(PRS))
  selected_vars$PRS=rankNorm(selected_vars$PRS)
  
  null_model = glm(data=selected_vars,
                   PD_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))
  prs_model = glm(data=selected_vars,
                  PD_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    PRS,
                  family=binomial(link="logit"))
  summary(prs_model)
  nag <<- nagelkerke(prs_model,null_model)
  nagelkerke <<- c(nagelkerke,nag$Pseudo.R.squared.for.model.vs.null[3])
}
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval0.8")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval0.6")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval0.4")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval0.2")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval0.1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval0.05")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval0.005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval0.0005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_01/summarised_PRS_results_pval0.00005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval0.8")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval0.6")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval0.4")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval0.2")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval0.1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval0.05")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval0.005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval0.0005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_02/summarised_PRS_results_pval0.00005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.8")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.6")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.4")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.2")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.05")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.0005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.00005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval0.8")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval0.6")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval0.4")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval0.2")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval0.1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval0.05")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval0.005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval0.0005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_06/summarised_PRS_results_pval0.00005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.8")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.6")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.4")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.2")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.05")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.0005")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.00005")
score_prs()








pvals = factor(rep(c(1,0.8,0.6,0.4,0.2,0.1,0.05,0.005, 0.0005, 0.00005),5))
r2 = factor(c(rep(0.1,10),rep(0.2,10),rep(0.4,10),rep(0.6,10),rep(0.8,10)))
nagel = data.frame(nagelkerke,pvals,r2)
nagel_plot = ggplot(nagel,aes(pvals,nagelkerke,fill=r2))+
  geom_col(position=position_dodge(),col="black")+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x="P value threshold",y="Nagelkerke PseudoR2",fill="Clumping R2 parameter")+
  theme(text=element_text(size=14))

png("prs.png",height=10,width=10,res=1000,units="in")
nagel_plot
dev.off()



#choose best prs based on r2 and read it in
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_08/summarised_PRS_results_pval0.0005")
#prs = read_table2("H:/UKB_PD/r2_08/summarised_PRS_results_pval0.005")

selected_vars = selected_vars %>% select(-contains("prs"))
selected_vars$EID = as.numeric(as.character(selected_vars$EID))
selected_vars = selected_vars %>% left_join(prs,by="EID")
selected_vars = selected_vars %>% filter(!(is.na(PRS)))
selected_vars$PRS=rankNorm(selected_vars$PRS)
  
# save progress
write_tsv(selected_vars,"processed_pheno_file.tsv")
#selected_vars = read_tsv("processed_pheno_file.tsv")


# prs decile plot
selected_vars$prs_decile = cut2(selected_vars$PRS,g=10)
prs_model = glm(data=selected_vars,
                PD_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  prs_decile,
                family=binomial(link="logit"))
tbl = data.frame(summary(prs_model)$coefficients[-c(1:7),])
tbl$decile=c(2:10)
tbl$or=exp(tbl$Estimate)
tbl$lower_ci=exp(tbl$Estimate-1.96*tbl$Std..Error)
tbl$upper_ci=exp(tbl$Estimate+1.96*tbl$Std..Error)

prs_decile_plot=ggplot(tbl,aes(decile,or,fill=or))+
  geom_errorbar(aes(x=decile,ymin=lower_ci,ymax=upper_ci,width=0.2))+
  geom_point(size=5,shape=22)+
  theme_classic()+
  theme(legend.position="none",text=element_text(size=16))+
  labs(x="PRS Decile",y="OR for PD (vs lowest decile)")

means = selected_vars %>% group_by(PD_status) %>%
  summarise("mean"=mean(PRS,na.rm=TRUE))

png("decile_plot.png",height=10,width=10,res=1000,units="in")
prs_decile_plot
dev.off()


count_tbl = table(selected_vars$prs_decile,selected_vars$PD_Dx_after_rec)
count_tbl = data.frame(cbind(count_tbl,count_tbl[,2]/(count_tbl[,1]+count_tbl[,2])))
write_csv(count_tbl,"counts_in_each_decile.csv")

# calibration

abs_prs_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  PRS,
                family=binomial(link="logit"))
null_prs_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`,
                family=binomial(link="logit"))

agesex_null_prs_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Age at recruitment.0.0`+
                  `Sex.0.0`,
                family=binomial(link="logit"))

prs_predict_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  PRS+
                  log(baseline_risk),
                family=binomial(link="logit"))

predict_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  log(baseline_risk),
                family=binomial(link="logit"))

predictions = predict(abs_prs_model,newdata=selected_vars,type="response")
null_predictions = predict(null_prs_model,newdata=selected_vars,type="response")
agesex_null_predictions = predict(agesex_null_prs_model,newdata=selected_vars,type="response")
predict_predictions = predict(predict_model,newdata=selected_vars,type="response")
prs_predict_predictions = predict(prs_predict_model,newdata=selected_vars,type="response")

preds = data.frame(predictions,null_predictions,predict_predictions,prs_predict_predictions,selected_vars$prs_decile,selected_vars$PD_Dx_after_rec)

preds = preds %>% group_by(selected_vars.prs_decile) %>% summarise(mean(predictions),mean(null_predictions),mean(predict_predictions),mean(prs_predict_predictions))
tbl = table(selected_vars$prs_decile,selected_vars$PD_Dx_after_rec)
obs_risk = tbl[,2]/rowSums(tbl)

pred_df = data.frame(preds,obs_risk)
pred_df$selected_vars.prs_decile = c(1:10)
library(reshape2)
pred_df = melt(pred_df,id="selected_vars.prs_decile")

pred_df$selected_vars.prs_decile = factor(pred_df$selected_vars.prs_decile)
calib_plot = ggplot(pred_df,aes(selected_vars.prs_decile,value,col=variable,group=variable))+geom_point()+geom_line()+labs(x="PRS decile",y="PD risk (probability scale)")+scale_color_brewer(palette="Set2")+theme_classic()

png("/data/Wolfson-UKBB-Dobson/PD_pheno/calibration_plot.png",height=10,width=10,res=1000,units="in")
calib_plot
dev.off()


preds = data.frame(predictions,null_predictions,agesex_null_predictions,predict_predictions,prs_predict_predictions,selected_vars$PD_Dx_after_rec)
preds =preds %>% filter(!is.na(selected_vars.PD_Dx_after_rec))
predictions = ROCR::prediction(list(preds$predictions,preds$null_predictions,preds$agesex_null_predictions,preds$predict_predictions,preds$prs_predict_predictions),list(factor(preds$selected_vars.PD_Dx_after_rec),factor(preds$selected_vars.PD_Dx_after_rec),factor(preds$selected_vars.PD_Dx_after_rec),factor(preds$selected_vars.PD_Dx_after_rec),factor(preds$selected_vars.PD_Dx_after_rec)))
roc.perf = performance(predictions, measure = "tpr", x.measure = "fpr")

# predictions
# null
# agesex_null
# predict_pred
# prs_pred
full_model = data.frame(x=roc.perf@x.values[[1]],y=roc.perf@y.values[[1]],model="full_model")
null_model = data.frame(x=roc.perf@x.values[[2]],y=roc.perf@y.values[[2]],model="null_model")
agesex_null_model = data.frame(x=roc.perf@x.values[[3]],y=roc.perf@y.values[[3]],model="agesex_null_model")
predict_model = data.frame(x=roc.perf@x.values[[4]],y=roc.perf@y.values[[4]],model="predict_model")
predict_prs_model = data.frame(x=roc.perf@x.values[[5]],y=roc.perf@y.values[[5]],model="predict_prs_model")


df = bind_rows(full_model,null_model,agesex_null_model,predict_model,predict_prs_model)


png("/data/Wolfson-UKBB-Dobson/PD_pheno/discrimination_plot.png",height=10,width=10,res=1000,units="in")
ggplot(df,aes(x,y,col=model))+geom_line()+scale_color_brewer(palette="Set2")+labs(x="False Positive Rate",y="True Positive Rate",col="Model")+theme_classic()+geom_abline()
dev.off()

auc.perf = performance(predictions, measure = "auc")
auc.perf@y.values



hist = ggplot(selected_vars,aes(PRS,fill=PD_status))+
  geom_density(alpha=0.5)+
  scale_fill_brewer(palette ="Set2",labels=c("Controls","PD"))+
  theme_classic()+
  theme(text=element_text(size=16))+
  labs(x="PRS",y="Density",fill="PD status")

png("prs_histo.png",height=10,width=10,res=1000,units="in")
hist
dev.off()



# does prs improve predict fit
prs_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  log(baseline_risk)+PRS,
                family=binomial(link="logit"))

library(caret)          
selected_vars$preds = predict(prs_model,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
baseline_prs_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)
pred = selected_vars %>% filter(preds %in% c(1,10)) %>% select(PD_Dx_after_rec,preds) 
pred$preds = factor(recode(pred$preds,"1"="0","10"="1"),levels=c("0","1"))
pred$PD_Dx_after_rec=factor(pred$PD_Dx_after_rec,levels=c("0","1"))
confusionMatrix(pred$preds,pred$PD_Dx_after_rec)


null_model = glm(data=selected_vars,
                 PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+log(baseline_risk),
                family=binomial(link="logit"))
selected_vars$preds = predict(null_model,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
baseline_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)

null_model2 = glm(data=selected_vars,
                 PD_Dx_after_rec~`Age at recruitment.0.0`+
                   `Sex.0.0`+
                   `Genetic principal components.0.1`+
                   `Genetic principal components.0.2`+
                   `Genetic principal components.0.3`+
                   `Genetic principal components.0.4`,
                 family=binomial(link="logit"))
selected_vars$preds = predict(null_model2,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
age_sex_pcs_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)

null_model3 = glm(data=selected_vars,
                 PD_Dx_after_rec~`Age at recruitment.0.0`+
                   `Sex.0.0`+
                   `Genetic principal components.0.1`+
                   `Genetic principal components.0.2`+
                   `Genetic principal components.0.3`+
                   `Genetic principal components.0.4`+PRS,
                 family=binomial(link="logit"))
selected_vars$preds = predict(null_model3,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
age_sex_pcs_prs_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)
pred = selected_vars %>% filter(preds %in% c(1,10)) %>% select(PD_Dx_after_rec,preds) 
pred$preds = factor(recode(pred$preds,"1"="0","10"="1"),levels=c("0","1"))
pred$PD_Dx_after_rec=factor(pred$PD_Dx_after_rec,levels=c("0","1"))
confusionMatrix(pred$preds,pred$PD_Dx_after_rec)


null_model4 = glm(data=selected_vars,
                 PD_Dx_after_rec~PRS,
                 family=binomial(link="logit"))
selected_vars$preds = predict(null_model4,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
baseline_only_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)
pred = selected_vars %>% filter(preds %in% c(1,10)) %>% select(PD_Dx_after_rec,preds) 
pred$preds = factor(recode(pred$preds,"1"="0","10"="1"),levels=c("0","1"))
pred$PD_Dx_after_rec=factor(pred$PD_Dx_after_rec,levels=c("0","1"))
confusionMatrix(pred$preds,pred$PD_Dx_after_rec)



summ = cbind(age_sex_pcs_prs_tbl,baseline_tbl,baseline_prs_tbl,baseline_only_tbl)
 
age_sex_pcs_prs = summ[,2]/colSums(summ)[2]*100
predict_pcs = summ[,4]/colSums(summ)[4]*100
predict_prs_pcs = summ[,6]/colSums(summ)[6]*100
predict_only = summ[,8]/colSums(summ)[8]*100

df = data.frame("decile"=rep(c(1:10),3),
"Risk algorithm"=c(rep("Age + Sex + PCs + PRS",10),
rep("Predict-PD + PRS + PCs",10),
rep("Predict-PD",10)),
"Proportion of incident cases in risk decile"=c(age_sex_pcs_prs,predict_prs_pcs,predict_only))
df$Risk.algorithm = factor(df$Risk.algorithm)
df$decile = factor(df$decile,order=TRUE)
p=ggplot(df,aes(decile,Proportion.of.incident.cases.in.risk.decile,fill=Risk.algorithm,group=Risk.algorithm))+
  geom_line(lwd=0.5,col="black")+
  geom_point(size=5,shape=22)+
  scale_fill_brewer(palette="Set2")+
  theme_classic()+
  theme(text=element_text(size=16))+
  labs(x="Risk Decile",y="Proportion of incident cases in risk decile",fill="Risk algorithm",col="Risk algorithm")
png("predictions.png",height=10,width=10,res=1000,units="in")
p
dev.off()
  

print("Nagelkerke for PRS + predict vs predict")
nagelkerke(prs_model,null_model)
summary(prs_model)
print("Nagelkerke for PRS + predict vs null (age,sex, four PCs)")
nagelkerke(prs_model,null_model2)




#age of onset

hist(selected_vars$age_at_pd_dx)
summary(selected_vars$age_at_pd_dx)
age_model=lm(data=selected_vars,
             age_at_pd_dx~`Age at recruitment.0.0`+
               `Sex.0.0`+
               `Genetic principal components.0.1`+
               `Genetic principal components.0.2`+
               `Genetic principal components.0.3`+
               `Genetic principal components.0.4`+
               PRS)
summary(age_model)
age_model2=lm(data=selected_vars,
             age_at_pd_dx~PRS)

summary(age_model2)
age_plot=ggplot(selected_vars,aes((PRS),age_at_pd_dx))+geom_point()+theme_classic()+theme(text=element_text(size=14))+
  labs(y="Age at diagnosis",x="PRS")+geom_smooth(method="lm",fill="red",alpha=0.3)

png("prs_age_of_dx.png",height=10,width=10,res=1000,units="in")
age_plot
dev.off()

#########################################
# use top prs excluding pd risk loci
#########################################
#choose best prs based on r2 and read it in
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/top_prs_excluding_sig_vars/summarised_PRS_results_pval0.0005")
#prs = read_table2("H:/UKB_PD/r2_08/summarised_PRS_results_pval0.005")
prs = prs %>% rename("prs_no_top_hits"=PRS)
selected_vars$EID = as.numeric(as.character(selected_vars$EID))
selected_vars = selected_vars %>% left_join(prs,by="EID")
selected_vars$prs_no_top_hits=rankNorm(selected_vars$prs_no_top_hits)

prs_no_top_hits_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  baseline_risk+prs_no_top_hits,
                family=binomial(link="logit"))

library(caret)          
selected_vars$preds = predict(prs_no_top_hits_model,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
baseline_prs_no_top_hits_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)
pred = selected_vars %>% filter(preds %in% c(1,10)) %>% select(PD_Dx_after_rec,preds) 
pred$preds = factor(recode(pred$preds,"1"="0","10"="1"),levels=c("0","1"))
pred$PD_Dx_after_rec=factor(pred$PD_Dx_after_rec,levels=c("0","1"))
confusionMatrix(pred$preds,pred$PD_Dx_after_rec)


null_model = glm(data=selected_vars,
                 PD_Dx_after_rec~`Genetic principal components.0.1`+
                   `Genetic principal components.0.2`+
                   `Genetic principal components.0.3`+
                   `Genetic principal components.0.4`+baseline_risk,
                 family=binomial(link="logit"))
selected_vars$preds = predict(null_model,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
baseline_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)

null_model2 = glm(data=selected_vars,
                  PD_Dx_after_rec~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`,
                  family=binomial(link="logit"))
selected_vars$preds = predict(null_model2,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
age_sex_pcs_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)

null_model3 = glm(data=selected_vars,
                  PD_Dx_after_rec~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+prs_no_top_hits,
                  family=binomial(link="logit"))
selected_vars$preds = predict(null_model3,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
age_sex_pcs_prs_no_top_hits_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)
pred = selected_vars %>% filter(preds %in% c(1,10)) %>% select(PD_Dx_after_rec,preds) 
pred$preds = factor(recode(pred$preds,"1"="0","10"="1"),levels=c("0","1"))
pred$PD_Dx_after_rec=factor(pred$PD_Dx_after_rec,levels=c("0","1"))
confusionMatrix(pred$preds,pred$PD_Dx_after_rec)


null_model4 = glm(data=selected_vars,
                  PD_Dx_after_rec~prs_no_top_hits,
                  family=binomial(link="logit"))
selected_vars$preds = predict(null_model4,newdata=selected_vars,type="response")
selected_vars$preds = cut2(selected_vars$preds,g=10)
levels(selected_vars$preds)=c(1:10)
baseline_only_tbl = table(selected_vars$preds,selected_vars$PD_Dx_after_rec)
pred = selected_vars %>% filter(preds %in% c(1,10)) %>% select(PD_Dx_after_rec,preds) 
pred$preds = factor(recode(pred$preds,"1"="0","10"="1"),levels=c("0","1"))
pred$PD_Dx_after_rec=factor(pred$PD_Dx_after_rec,levels=c("0","1"))
confusionMatrix(pred$preds,pred$PD_Dx_after_rec)



summ = cbind(age_sex_pcs_prs_no_top_hits_tbl,baseline_tbl,baseline_prs_no_top_hits_tbl,baseline_only_tbl)

age_sex_pcs_prs_no_top_hits = summ[,2]/colSums(summ)[2]*100
predict_pcs = summ[,4]/colSums(summ)[4]*100
predict_prs_no_top_hits_pcs = summ[,6]/colSums(summ)[6]*100
predict_only = summ[,8]/colSums(summ)[8]*100

df = data.frame("decile"=rep(c(1:10),3),
                "Risk algorithm"=c(rep("Age + Sex + PCs + prs_no_top_hits",10),
                                   rep("Predict-PD + prs_no_top_hits + PCs",10),
                                   rep("Predict-PD",10)),
                "Proportion of incident cases in risk decile"=c(age_sex_pcs_prs_no_top_hits,predict_prs_no_top_hits_pcs,predict_only))
df$Risk.algorithm = factor(df$Risk.algorithm)
df$decile = factor(df$decile,order=TRUE)
p=ggplot(df,aes(decile,Proportion.of.incident.cases.in.risk.decile,fill=Risk.algorithm,group=Risk.algorithm))+
  geom_line(lwd=0.5,col="black")+
  geom_point(size=5,shape=22)+
  scale_fill_brewer(palette="Set2")+
  theme_classic()+
  theme(text=element_text(size=16))+
  labs(x="Risk Decile",y="Proportion of incident cases in risk decile",fill="Risk algorithm",col="Risk algorithm")
png("predictions_excl_top_hits.png",height=10,width=10,res=1000,units="in")
p
dev.off()


print("Nagelkerke for prs_no_top_hits + predict vs predict")
nagelkerke(prs_no_top_hits_model,null_model)

print("printing incident cases")
table(selected_vars$PD_Dx_after_rec)
print("printing prevalent cases")
table(selected_vars$PD_status)



########################################
# multiplicative int
########################################

pdfhx_model = glm(data=selected_vars,
                     PD_Dx_after_rec~+`Age at recruitment.0.0`+
                       `Sex.0.0`+
                       `Townsend deprivation index at recruitment.0.0`+
                       `Genetic principal components.0.1`+
                       `Genetic principal components.0.2`+
                       `Genetic principal components.0.3`+
                       `Genetic principal components.0.4`+
                       factor(selected_vars$PD_FHx)*PRS,
                     family=binomial(link="logit"))

lr_df = data.frame('Exposure'="PD FHx","P-value"=anova(pdfhx_model,test="Chisq")$`Pr(>Chi)`[11],"Beta"=as.numeric(coef(pdfhx_model))[11])

alc_model = glm(data=selected_vars,
                  PD_Dx_after_rec~+`Age at recruitment.0.0`+
                    `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    alcohol*PRS,
                  family=binomial(link="logit"))

lr_df = rbind(lr_df,
data.frame('Exposure'="Alcohol",
"P-value"=anova(alc_model,test="Chisq")$`Pr(>Chi)`[11],
"Beta"=as.numeric(coef(alc_model))[11]))

dep_model = glm(data=selected_vars,
                PD_Dx_after_rec~+`Age at recruitment.0.0`+
                  `Sex.0.0`+
                   `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  factor(selected_vars$depression_Dx_pre_rec)*PRS,
                family=binomial(link="logit"))

lr_df = rbind(lr_df,data.frame('Exposure'="Depression","P-value"=anova(dep_model,test="Chisq")$`Pr(>Chi)`[11],"Beta"=as.numeric(coef(dep_model))[11]))

sleep_model = glm(data=selected_vars,
                PD_Dx_after_rec~+`Age at recruitment.0.0`+
                  `Sex.0.0`+
                   `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  sleepiness*PRS,
                family=binomial(link="logit"))

lr_df = rbind(lr_df,data.frame('Exposure'="Sleepiness","P-value"=anova(sleep_model,test="Chisq")$`Pr(>Chi)`[11],"Beta"=as.numeric(coef(sleep_model))[11]))

epi_model = glm(data=selected_vars,
                  PD_Dx_after_rec~+`Age at recruitment.0.0`+
                    `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                  factor(epilepsy_Dx_pre_rec)*PRS,
                  family=binomial(link="logit"))

lr_df = rbind(lr_df,data.frame('Exposure'="Epilepsy","P-value"=anova(epi_model,test="Chisq")$`Pr(>Chi)`[11],"Beta"=as.numeric(coef(epi_model))[11]))

selected_vars$smoking_status=recode(selected_vars$smoking_status,"Previous"="Ever","Current"="Ever","Never"="Never")

smok_model = glm(data=selected_vars,
                PD_Dx_after_rec~+`Age at recruitment.0.0`+
                  `Sex.0.0`+
                   `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  smoking_status*PRS,
                family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Smoking status","P-value"=anova(smok_model,test="Chisq")$`Pr(>Chi)`[11],"Beta"=as.numeric(coef(smok_model))[11]))

dementia_model = glm(data=selected_vars,
                 PD_Dx_after_rec~+`Age at recruitment.0.0`+
                   `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                   `Genetic principal components.0.1`+
                   `Genetic principal components.0.2`+
                   `Genetic principal components.0.3`+
                   `Genetic principal components.0.4`+
                   factor(selected_vars$Dementia_FHx)*PRS,
                 family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Dementia FHx status","P-value"=anova(dementia_model,test="Chisq")$`Pr(>Chi)`[11],"Beta"=as.numeric(coef(dementia_model))[11]))

dm_model = glm(data=selected_vars,
                 PD_Dx_after_rec~+`Age at recruitment.0.0`+
                   `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                   `Genetic principal components.0.1`+
                   `Genetic principal components.0.2`+
                   `Genetic principal components.0.3`+
                   `Genetic principal components.0.4`+
                   factor(selected_vars$DM_Dx_pre_rec)*PRS,
                 family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Diabetes","P-value"=anova(dm_model,test="Chisq")$`Pr(>Chi)`[11],"Beta"=as.numeric(coef(dm_model))[11]))

pud_model = glm(data=selected_vars,
                 PD_Dx_after_rec~+`Age at recruitment.0.0`+
                   `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                   `Genetic principal components.0.1`+
                   `Genetic principal components.0.2`+
                   `Genetic principal components.0.3`+
                   `Genetic principal components.0.4`+
                   factor(selected_vars$Gastric_ulcer_Dx_pre_rec)*PRS,
                 family=binomial(link="logit"))
lr_df = rbind(lr_df,data.frame('Exposure'="Gastric Ulcer","P-value"=anova(pud_model,test="Chisq")$`Pr(>Chi)`[11],"Beta"=as.numeric(coef(pud_model))[11]))

menarche_model = glm(data=selected_vars,
                  PD_Dx_after_rec~+`Age at recruitment.0.0`+
                   `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    `Age when periods started (menarche).0.0`*PRS,
                  family=binomial(link="logit"))

lr_df = rbind(lr_df,data.frame('Exposure'="Menarche","P-value"=anova(menarche_model,test="Chisq")$`Pr(>Chi)`[10],"Beta"=as.numeric(coef(menarche_model))[10]))
lr_df$FDR = p.adjust(lr_df$P.value,method="fdr")
lr_df=lr_df %>% select(Exposure,Beta,P.value,FDR)%>%arrange(FDR)

write_csv(lr_df,"multiplicative_int_lik_rats.csv")


# stratified plot
levels(selected_vars$prs_decile)=c(1:10)

bottom_decile = selected_vars %>% filter(prs_decile==1)
top_decile = selected_vars %>% filter(prs_decile==10)

bottoms = c()
bottom_model = function(x){
model = glm(data=bottom_decile,
                  PD_Dx_after_rec~+`Age at recruitment.0.0`+
                  Sex.0.0+
                   `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    bottom_decile[[x]],
                  family=binomial(link="logit"))
                  
bottoms <<- rbind(bottoms,c(x,summary(model)$coefficients[9,c(1,2)]))
}

bottom_model("DM_Dx_pre_rec")
bottom_model("alcohol")
bottom_model("Gastric_ulcer_Dx_pre_rec")
bottom_model("depression_Dx_pre_rec")
bottom_model("sleepiness")
bottom_model("epilepsy_Dx_pre_rec")
bottom_model("smoking_status")



tops = c()
top_model = function(x){
model = glm(data=top_decile,
                  PD_Dx_after_rec~+`Age at recruitment.0.0`+
                  Sex.0.0+
                   `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    top_decile[[x]],
                  family=binomial(link="logit"))
                  
tops <<- rbind(tops,c(x,summary(model)$coefficients[9,c(1,2)]))
}

top_model("DM_Dx_pre_rec")
top_model("alcohol")
top_model("Gastric_ulcer_Dx_pre_rec")
top_model("depression_Dx_pre_rec")
top_model("sleepiness")
top_model("epilepsy_Dx_pre_rec")
top_model("smoking_status")

menarche_model = glm(data=top_decile,
                  PD_Dx_after_rec~+`Age at recruitment.0.0`+
                   `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    `Age when periods started (menarche).0.0`,
                  family=binomial(link="logit"))
                  
tops <<- rbind(tops,c("Menarche",summary(menarche_model)$coefficients[8,c(1,2)]))

menarche_model = glm(data=bottom_decile,
                  PD_Dx_after_rec~+`Age at recruitment.0.0`+
                   `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    `Age when periods started (menarche).0.0`,
                  family=binomial(link="logit"))
                  
bottoms <<- rbind(bottoms,c("Menarche",summary(menarche_model)$coefficients[8,c(1,2)]))
df = bind_rows(data.frame(tops,decile="Top"),data.frame(bottoms,decile="Bottom"))

df$Estimate = as.numeric(df$Estimate)
df$Std..Error = as.numeric(df$Std..Error)


png(file="/data/Wolfson-UKBB-Dobson/PD_pheno/stratified_plot.png",height=8,width=8,res=300,units="in")
ggplot(df,aes(Estimate,V1,col=decile))+geom_point()+geom_errorbarh(aes(xmin=Estimate-1.96*Std..Error,xmax=Estimate+1.96*Std..Error),height=0.1)+theme_classic()+geom_vline(xintercept=0)+labs(x="Log(OR) for PD",y="Risk factor",col="PRS Decile")+theme(text=element_text(size=16))
dev.off()

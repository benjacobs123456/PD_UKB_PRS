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
biochem = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_911/ukb_pheno_final_with_biochem")

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
selected_vars$smoking_status = relevel(selected_vars$smoking_status,ref="Never")

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




#############################################
#     Build multivariable models 
#############################################

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

#save progress
#write_tsv(selected_vars,"processed_pheno.tsv")
#selected_vars = read_tsv("processed_pheno.tsv")

# model function
make_model = function(x){
model = glm(data=selected_vars,PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars[[x]],family=binomial(link="logit"))
}

make_model_nosex = function(x){
model = glm(data=selected_vars,PD_Dx_after_rec~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars[[x]],family=binomial(link="logit"))
}

# lr functions
get_lr = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[6]
}
get_lr_nosex = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[5]
}


# make models
smok=make_model("smoking_status")
pest=make_model("Pesticides")
alc=make_model("alcohol")
dm=make_model("DM_Dx_pre_rec")
epi=make_model("epilepsy_Dx_pre_rec")
bmi=make_model("Body mass index (BMI).0.0")
cbmi=make_model("Comparative body size at age 10.0.0")
sleepiness=make_model("sleepiness")
dep=make_model("depression_Dx_pre_rec")
anx=make_model("Anxiety_Dx_pre_rec")
mig=make_model("migraine_Dx_pre_rec")
constipation=make_model("constipation_Dx_pre_rec")
head_injury=make_model("head_injury_Dx_pre_rec")
Hypertension=make_model("Hypertension_Dx_pre_rec")
PD_FHx=make_model("PD_FHx")
Dementia_FHx=make_model("Dementia_FHx")
Depression_FHx=make_model("Depression_FHx")
Diabetes_FHx=make_model("Diabetes_FHx")
Stroke_FHx=make_model("Stroke_FHx")
breastfed=make_model("Breastfed as a baby.0.0")
mat_smok=make_model("Maternal smoking around birth.0.0")
handedness=make_model("handedness")
coffee=make_model("Coffee intake.0.0")
edu=make_model("Age completed full time education.0.0")
menarche=make_model_nosex("Age when periods started (menarche).0.0")
voicebreak=make_model_nosex("Relative age voice broke.0.0")
Gastric_ulcer=make_model("Gastric_ulcer_Dx_pre_rec")


models=list(smok,pest,alc,dm,epi,bmi,cbmi,sleepiness,dep,anx,mig,constipation,head_injury,Hypertension,PD_FHx,Dementia_FHx,Depression_FHx,Diabetes_FHx,Stroke_FHx,breastfed,mat_smok,handedness,coffee,edu,Gastric_ulcer)
coef_df = data.frame(models=c("smok","pest","alc","dm","epi","bmi","cbmi","sleepiness","dep","anx","mig","constipation","head_injury","Hypertension","PD_FHx","Dementia_FHx","Depression_FHx","Diabetes_FHx","Stroke_FHx","breastfed","mat_smok","handedness","coffee","edu","gastric ulcer"))
coef_df$LR_P = sapply(models,get_lr)
models2=list(menarche,voicebreak)
coef_df2 = data.frame(models=c("menarche","voicebreak"))
coef_df2$LR_P = sapply(models2,get_lr_nosex)
coef_df = bind_rows(coef_df,coef_df2)


df=rbind(summary(smok)$coefficients,summary(pest)$coefficients,summary(alc)$coefficients,summary(dm)$coefficients,summary(epi)$coefficients,summary(bmi)$coefficients,summary(cbmi)$coefficients,summary(sleepiness)$coefficients,summary(dep)$coefficients,summary(anx)$coefficients,summary(mig)$coefficients,summary(constipation)$coefficients,summary(head_injury)$coefficients,summary(Hypertension)$coefficients,summary(PD_FHx)$coefficients,summary(Dementia_FHx)$coefficients,summary(Depression_FHx)$coefficients,summary(Diabetes_FHx)$coefficients,summary(Stroke_FHx)$coefficients,summary(breastfed)$coefficients,summary(mat_smok)$coefficients,summary(handedness)$coefficients,summary(coffee)$coefficients,summary(edu)$coefficients,summary(Gastric_ulcer)$coefficients,summary(menarche)$coefficients,summary(voicebreak)$coefficients)
df = cbind(df,rownames(df))
df = data.frame(df)
colnames(df) = c("Beta","SE","Z","P","name")
rownames(df)=NULL

df=df %>% filter(!name %in% levels(df$name)[c(1,2,3,4,14)])

# caution: manual renaming
df$name=c("Smoking status: current",
          "Smoking status: previous",
          "Pesticide exposure",
          "Alcohol: >1 drink per week",
          "Diabetes",
          "Epilepsy",      
          "BMI",
          "Childhood obesity",
          "Excessive daytime sleepiness",
          "Depression",       
          "Anxiety",
          "Migraine",
          "Constipation",
          "Head Injury",
          "Hypertension",
          "Family history: PD",
          "Family History: dementia" ,
          "Family history: depression",
          "Family history: diabetes",
          "Family history: stroke",
          "Breastfed as a baby",
          "Exposed to maternal smoking",
          "Right-handed",
          "Cups of coffee per day",
          "Age completed full time education",
          "Gastric ulcer",
          "Age at menarche",
          "Age at voice breaking: older than average",
          "Age at voice breaking: younger than average")

df$cat=c("Environmental",
"Environmental",
"Environmental",
"Environmental",
"Comorbidity",
"Comorbidity",
"Environmental",
"Early life",
"Prodrome",
"Comorbidity",
"Comorbidity",
"Comorbidity",
"Prodrome",
"Environmental",
"Comorbidity",
"Family history",
"Family history",
"Family history",
"Family history",
"Family history",
"Early life",
"Early life",
"Early life",
"Environmental",
"Early life",
"Comorbidity",
"Early life",
"Early life",
"Early life")

df = df[order(df$cat),]
df$num = factor(c(1:nrow(df)))
factor(df$name)

df$Beta = as.numeric(as.character(df$Beta))
df$SE = as.numeric(as.character(df$SE))
overall_cc_estimates = df
p=ggplot(df,aes(Beta,num,fill=cat))+
  geom_errorbarh(mapping=aes(y=num,xmin=Beta-1.96*SE,xmax=Beta+1.96*SE),height=0.3)+
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
coef_df$fdr=p.adjust(coef_df$LR_P,method="fdr")
write_csv(coef_df,"table_1b.csv")

# model with all predictors

multivar_model = glm(data=selected_vars,
                     PD_Dx_after_rec~`Sex.0.0`+`Ethnic background.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+
                       factor(selected_vars$PD_FHx)+alcohol+factor(selected_vars$depression_Dx_pre_rec)+factor(epilepsy_Dx_pre_rec)+sleepiness+smoking_status+factor(selected_vars$Dementia_FHx),
                     family=binomial(link="logit"))
multi = data.frame(summary(multivar_model)$coefficients)
multi$name = rownames(multi)
write_csv(multi,"multivariate.csv")

# model with all predictors

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

# model function
make_model = function(x){
model = glm(data=white_only_selected_vars,PD_Dx_after_rec~`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+white_only_selected_vars[[x]],family=binomial(link="logit"))
}

make_model_nosex = function(x){
model = glm(data=white_only_selected_vars,PD_Dx_after_rec~`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+white_only_selected_vars[[x]],family=binomial(link="logit"))
}

# lr functions
get_lr = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[5]
}
get_lr_nosex = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[4]
}


# make models
smok=make_model("smoking_status")
pest=make_model("Pesticides")
alc=make_model("alcohol")
dm=make_model("DM_Dx_pre_rec")
epi=make_model("epilepsy_Dx_pre_rec")
bmi=make_model("Body mass index (BMI).0.0")
cbmi=make_model("Comparative body size at age 10.0.0")
sleepiness=make_model("sleepiness")
dep=make_model("depression_Dx_pre_rec")
anx=make_model("Anxiety_Dx_pre_rec")
mig=make_model("migraine_Dx_pre_rec")
constipation=make_model("constipation_Dx_pre_rec")
head_injury=make_model("head_injury_Dx_pre_rec")
Hypertension=make_model("Hypertension_Dx_pre_rec")
PD_FHx=make_model("PD_FHx")
Dementia_FHx=make_model("Dementia_FHx")
Depression_FHx=make_model("Depression_FHx")
Diabetes_FHx=make_model("Diabetes_FHx")
Stroke_FHx=make_model("Stroke_FHx")
breastfed=make_model("Breastfed as a baby.0.0")
mat_smok=make_model("Maternal smoking around birth.0.0")
handedness=make_model("handedness")
coffee=make_model("Coffee intake.0.0")
edu=make_model("Age completed full time education.0.0")
menarche=make_model_nosex("Age when periods started (menarche).0.0")
voicebreak=make_model_nosex("Relative age voice broke.0.0")
Gastric_ulcer=make_model("Gastric_ulcer_Dx_pre_rec")


models=list(smok,pest,alc,dm,epi,bmi,cbmi,sleepiness,dep,anx,mig,constipation,head_injury,Hypertension,PD_FHx,Dementia_FHx,Depression_FHx,Diabetes_FHx,Stroke_FHx,breastfed,mat_smok,handedness,coffee,edu,Gastric_ulcer)
coef_df = data.frame(models=c("smok","pest","alc","dm","epi","bmi","cbmi","sleepiness","dep","anx","mig","constipation","head_injury","Hypertension","PD_FHx","Dementia_FHx","Depression_FHx","Diabetes_FHx","Stroke_FHx","breastfed","mat_smok","handedness","coffee","edu","gastric ulcer"))
coef_df$LR_P = sapply(models,get_lr)
models2=list(menarche,voicebreak)
coef_df2 = data.frame(models=c("menarche","voicebreak"))
coef_df2$LR_P = sapply(models2,get_lr_nosex)
coef_df = bind_rows(coef_df,coef_df2)


df=rbind(summary(smok)$coefficients,summary(pest)$coefficients,summary(alc)$coefficients,summary(dm)$coefficients,summary(epi)$coefficients,summary(bmi)$coefficients,summary(cbmi)$coefficients,summary(sleepiness)$coefficients,summary(dep)$coefficients,summary(anx)$coefficients,summary(mig)$coefficients,summary(constipation)$coefficients,summary(head_injury)$coefficients,summary(Hypertension)$coefficients,summary(PD_FHx)$coefficients,summary(Dementia_FHx)$coefficients,summary(Depression_FHx)$coefficients,summary(Diabetes_FHx)$coefficients,summary(Stroke_FHx)$coefficients,summary(breastfed)$coefficients,summary(mat_smok)$coefficients,summary(handedness)$coefficients,summary(coffee)$coefficients,summary(edu)$coefficients,summary(Gastric_ulcer)$coefficients,summary(menarche)$coefficients,summary(voicebreak)$coefficients)
df = cbind(df,rownames(df))
df = data.frame(df)
colnames(df) = c("Beta","SE","Z","P","name")
rownames(df)=NULL

df=df %>% filter(!name %in% levels(df$name)[c(1,2,3,4)])

# caution: manual renaming
df$name=c("Smoking status: current",
          "Smoking status: previous",
          "Pesticide exposure",
          "Alcohol: >1 drink per week",
          "Diabetes",
          "Epilepsy",      
          "BMI",
          "Childhood obesity",
          "Excessive daytime sleepiness",
          "Depression",       
          "Anxiety",
          "Migraine",
          "Constipation",
          "Head Injury",
          "Hypertension",
          "Family history: PD",
          "Family History: dementia" ,
          "Family history: depression",
          "Family history: diabetes",
          "Family history: stroke",
          "Breastfed as a baby",
          "Exposed to maternal smoking",
          "Right-handed",
          "Cups of coffee per day",
          "Age completed full time education",
          "Gastric ulcer",
          "Age at menarche",
          "Age at voice breaking: older than average",
          "Age at voice breaking: younger than average")

df$cat=c("Environmental",
"Environmental",
"Environmental",
"Environmental",
"Comorbidity",
"Comorbidity",
"Environmental",
"Early life",
"Prodrome",
"Comorbidity",
"Comorbidity",
"Comorbidity",
"Prodrome",
"Environmental",
"Comorbidity",
"Family history",
"Family history",
"Family history",
"Family history",
"Family history",
"Early life",
"Early life",
"Early life",
"Environmental",
"Early life",
"Comorbidity",
"Early life",
"Early life",
"Early life")

df = df[order(df$cat),]
df$num = factor(c(1:nrow(df)))
factor(df$name)

df$Beta = as.numeric(as.character(df$Beta))
df$SE = as.numeric(as.character(df$SE))

p=ggplot(df,aes(Beta,num,fill=cat))+
  geom_errorbarh(mapping=aes(y=num,xmin=Beta-1.96*SE,xmax=Beta+1.96*SE),height=0.3)+
  geom_vline(xintercept=0,alpha=0.3)+
  geom_point(size=5,shape=22)+
  scale_fill_brewer(palette="Set2")+
  labs(x="Log(OR) for PD",y="Risk factor",fill="")+
  theme_classic()+
  scale_y_discrete(labels=df$name)


png(file="white_only_incident_case_control.png",height=12,width=12,res=300,units="in")
p
dev.off()


#############################################
#     2) remove <60s
#############################################

selected_vars_over_60s = selected_vars %>% filter(`Age at recruitment.0.0`>60)

# model function
make_model = function(x){
model = glm(data=selected_vars_over_60s,PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars_over_60s[[x]],family=binomial(link="logit"))
}

make_model_nosex = function(x){
model = glm(data=selected_vars_over_60s,PD_Dx_after_rec~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars_over_60s[[x]],family=binomial(link="logit"))
}

# lr functions
get_lr = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[6]
}
get_lr_nosex = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[5]
}


# make models
smok=make_model("smoking_status")
pest=make_model("Pesticides")
alc=make_model("alcohol")
dm=make_model("DM_Dx_pre_rec")
epi=make_model("epilepsy_Dx_pre_rec")
bmi=make_model("Body mass index (BMI).0.0")
cbmi=make_model("Comparative body size at age 10.0.0")
sleepiness=make_model("sleepiness")
dep=make_model("depression_Dx_pre_rec")
anx=make_model("Anxiety_Dx_pre_rec")
mig=make_model("migraine_Dx_pre_rec")
constipation=make_model("constipation_Dx_pre_rec")
head_injury=make_model("head_injury_Dx_pre_rec")
Hypertension=make_model("Hypertension_Dx_pre_rec")
PD_FHx=make_model("PD_FHx")
Dementia_FHx=make_model("Dementia_FHx")
Depression_FHx=make_model("Depression_FHx")
Diabetes_FHx=make_model("Diabetes_FHx")
Stroke_FHx=make_model("Stroke_FHx")
breastfed=make_model("Breastfed as a baby.0.0")
mat_smok=make_model("Maternal smoking around birth.0.0")
handedness=make_model("handedness")
coffee=make_model("Coffee intake.0.0")
edu=make_model("Age completed full time education.0.0")
menarche=make_model_nosex("Age when periods started (menarche).0.0")
voicebreak=make_model_nosex("Relative age voice broke.0.0")
Gastric_ulcer=make_model("Gastric_ulcer_Dx_pre_rec")


models=list(smok,pest,alc,dm,epi,bmi,cbmi,sleepiness,dep,anx,mig,constipation,head_injury,Hypertension,PD_FHx,Dementia_FHx,Depression_FHx,Diabetes_FHx,Stroke_FHx,breastfed,mat_smok,handedness,coffee,edu,Gastric_ulcer)
coef_df = data.frame(models=c("smok","pest","alc","dm","epi","bmi","cbmi","sleepiness","dep","anx","mig","constipation","head_injury","Hypertension","PD_FHx","Dementia_FHx","Depression_FHx","Diabetes_FHx","Stroke_FHx","breastfed","mat_smok","handedness","coffee","edu","gastric ulcer"))
coef_df$LR_P = sapply(models,get_lr)
models2=list(menarche,voicebreak)
coef_df2 = data.frame(models=c("menarche","voicebreak"))
coef_df2$LR_P = sapply(models2,get_lr_nosex)
coef_df = bind_rows(coef_df,coef_df2)


df=rbind(summary(smok)$coefficients,summary(pest)$coefficients,summary(alc)$coefficients,summary(dm)$coefficients,summary(epi)$coefficients,summary(bmi)$coefficients,summary(cbmi)$coefficients,summary(sleepiness)$coefficients,summary(dep)$coefficients,summary(anx)$coefficients,summary(mig)$coefficients,summary(constipation)$coefficients,summary(head_injury)$coefficients,summary(Hypertension)$coefficients,summary(PD_FHx)$coefficients,summary(Dementia_FHx)$coefficients,summary(Depression_FHx)$coefficients,summary(Diabetes_FHx)$coefficients,summary(Stroke_FHx)$coefficients,summary(breastfed)$coefficients,summary(mat_smok)$coefficients,summary(handedness)$coefficients,summary(coffee)$coefficients,summary(edu)$coefficients,summary(Gastric_ulcer)$coefficients,summary(menarche)$coefficients,summary(voicebreak)$coefficients)
df = cbind(df,rownames(df))
df = data.frame(df)
colnames(df) = c("Beta","SE","Z","P","name")
rownames(df)=NULL

df=df %>% filter(!name %in% levels(df$name)[c(1,2,3,4,14)])

# caution: manual renaming
df$name=c("Smoking status: current",
          "Smoking status: previous",
          "Pesticide exposure",
          "Alcohol: >1 drink per week",
          "Diabetes",
          "Epilepsy",      
          "BMI",
          "Childhood obesity",
          "Excessive daytime sleepiness",
          "Depression",       
          "Anxiety",
          "Migraine",
          "Constipation",
          "Head Injury",
          "Hypertension",
          "Family history: PD",
          "Family History: dementia" ,
          "Family history: depression",
          "Family history: diabetes",
          "Family history: stroke",
          "Breastfed as a baby",
          "Exposed to maternal smoking",
          "Right-handed",
          "Cups of coffee per day",
          "Age completed full time education",
          "Gastric ulcer",
          "Age at menarche",
          "Age at voice breaking: older than average",
          "Age at voice breaking: younger than average")

df$cat=c("Environmental",
"Environmental",
"Environmental",
"Environmental",
"Comorbidity",
"Comorbidity",
"Environmental",
"Early life",
"Prodrome",
"Comorbidity",
"Comorbidity",
"Comorbidity",
"Prodrome",
"Environmental",
"Comorbidity",
"Family history",
"Family history",
"Family history",
"Family history",
"Family history",
"Early life",
"Early life",
"Early life",
"Environmental",
"Early life",
"Comorbidity",
"Early life",
"Early life",
"Early life")

df = df[order(df$cat),]
df$num = factor(c(1:nrow(df)))
factor(df$name)

df$Beta = as.numeric(as.character(df$Beta))
df$SE = as.numeric(as.character(df$SE))
overall_cc_estimates = df
p=ggplot(df,aes(Beta,num,fill=cat))+
  geom_errorbarh(mapping=aes(y=num,xmin=Beta-1.96*SE,xmax=Beta+1.96*SE),height=0.3)+
  geom_vline(xintercept=0,alpha=0.3)+
  geom_point(size=5,shape=22)+
  scale_fill_brewer(palette="Set2")+
  labs(x="Log(OR) for PD",y="Risk factor",fill="")+
  theme_classic()+
  scale_y_discrete(labels=df$name)


png(file="over60s_incident_case_control.png",height=12,width=12,res=300,units="in")
p
dev.off()

#############################################
#     3) exclude self-report
#############################################

selected_vars_noself = selected_vars %>% filter(!(PD_Dx_after_rec==1 & `Source of parkinson's disease report.0.0`=="Self-reported only"))

# model function
make_model = function(x){
model = glm(data=selected_vars_noself,PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars_noself[[x]],family=binomial(link="logit"))
}

make_model_nosex = function(x){
model = glm(data=selected_vars_noself,PD_Dx_after_rec~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars_noself[[x]],family=binomial(link="logit"))
}

# lr functions
get_lr = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[6]
}
get_lr_nosex = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[5]
}


# make models
smok=make_model("smoking_status")
pest=make_model("Pesticides")
alc=make_model("alcohol")
dm=make_model("DM_Dx_pre_rec")
epi=make_model("epilepsy_Dx_pre_rec")
bmi=make_model("Body mass index (BMI).0.0")
cbmi=make_model("Comparative body size at age 10.0.0")
sleepiness=make_model("sleepiness")
dep=make_model("depression_Dx_pre_rec")
anx=make_model("Anxiety_Dx_pre_rec")
mig=make_model("migraine_Dx_pre_rec")
constipation=make_model("constipation_Dx_pre_rec")
head_injury=make_model("head_injury_Dx_pre_rec")
Hypertension=make_model("Hypertension_Dx_pre_rec")
PD_FHx=make_model("PD_FHx")
Dementia_FHx=make_model("Dementia_FHx")
Depression_FHx=make_model("Depression_FHx")
Diabetes_FHx=make_model("Diabetes_FHx")
Stroke_FHx=make_model("Stroke_FHx")
breastfed=make_model("Breastfed as a baby.0.0")
mat_smok=make_model("Maternal smoking around birth.0.0")
handedness=make_model("handedness")
coffee=make_model("Coffee intake.0.0")
edu=make_model("Age completed full time education.0.0")
menarche=make_model_nosex("Age when periods started (menarche).0.0")
voicebreak=make_model_nosex("Relative age voice broke.0.0")
Gastric_ulcer=make_model("Gastric_ulcer_Dx_pre_rec")


models=list(smok,pest,alc,dm,epi,bmi,cbmi,sleepiness,dep,anx,mig,constipation,head_injury,Hypertension,PD_FHx,Dementia_FHx,Depression_FHx,Diabetes_FHx,Stroke_FHx,breastfed,mat_smok,handedness,coffee,edu,Gastric_ulcer)
coef_df = data.frame(models=c("smok","pest","alc","dm","epi","bmi","cbmi","sleepiness","dep","anx","mig","constipation","head_injury","Hypertension","PD_FHx","Dementia_FHx","Depression_FHx","Diabetes_FHx","Stroke_FHx","breastfed","mat_smok","handedness","coffee","edu","gastric ulcer"))
coef_df$LR_P = sapply(models,get_lr)
models2=list(menarche,voicebreak)
coef_df2 = data.frame(models=c("menarche","voicebreak"))
coef_df2$LR_P = sapply(models2,get_lr_nosex)
coef_df = bind_rows(coef_df,coef_df2)


df=rbind(summary(smok)$coefficients,summary(pest)$coefficients,summary(alc)$coefficients,summary(dm)$coefficients,summary(epi)$coefficients,summary(bmi)$coefficients,summary(cbmi)$coefficients,summary(sleepiness)$coefficients,summary(dep)$coefficients,summary(anx)$coefficients,summary(mig)$coefficients,summary(constipation)$coefficients,summary(head_injury)$coefficients,summary(Hypertension)$coefficients,summary(PD_FHx)$coefficients,summary(Dementia_FHx)$coefficients,summary(Depression_FHx)$coefficients,summary(Diabetes_FHx)$coefficients,summary(Stroke_FHx)$coefficients,summary(breastfed)$coefficients,summary(mat_smok)$coefficients,summary(handedness)$coefficients,summary(coffee)$coefficients,summary(edu)$coefficients,summary(Gastric_ulcer)$coefficients,summary(menarche)$coefficients,summary(voicebreak)$coefficients)
df = cbind(df,rownames(df))
df = data.frame(df)
colnames(df) = c("Beta","SE","Z","P","name")
rownames(df)=NULL

df=df %>% filter(!name %in% levels(df$name)[c(1,2,3,4,14)])

# caution: manual renaming
df$name=c("Smoking status: current",
          "Smoking status: previous",
          "Pesticide exposure",
          "Alcohol: >1 drink per week",
          "Diabetes",
          "Epilepsy",      
          "BMI",
          "Childhood obesity",
          "Excessive daytime sleepiness",
          "Depression",       
          "Anxiety",
          "Migraine",
          "Constipation",
          "Head Injury",
          "Hypertension",
          "Family history: PD",
          "Family History: dementia" ,
          "Family history: depression",
          "Family history: diabetes",
          "Family history: stroke",
          "Breastfed as a baby",
          "Exposed to maternal smoking",
          "Right-handed",
          "Cups of coffee per day",
          "Age completed full time education",
          "Gastric ulcer",
          "Age at menarche",
          "Age at voice breaking: older than average",
          "Age at voice breaking: younger than average")

df$cat=c("Environmental",
"Environmental",
"Environmental",
"Environmental",
"Comorbidity",
"Comorbidity",
"Environmental",
"Early life",
"Prodrome",
"Comorbidity",
"Comorbidity",
"Comorbidity",
"Prodrome",
"Environmental",
"Comorbidity",
"Family history",
"Family history",
"Family history",
"Family history",
"Family history",
"Early life",
"Early life",
"Early life",
"Environmental",
"Early life",
"Comorbidity",
"Early life",
"Early life",
"Early life")

df = df[order(df$cat),]
df$num = factor(c(1:nrow(df)))
factor(df$name)

df$Beta = as.numeric(as.character(df$Beta))
df$SE = as.numeric(as.character(df$SE))
overall_cc_estimates = df
p=ggplot(df,aes(Beta,num,fill=cat))+
  geom_errorbarh(mapping=aes(y=num,xmin=Beta-1.96*SE,xmax=Beta+1.96*SE),height=0.3)+
  geom_vline(xintercept=0,alpha=0.3)+
  geom_point(size=5,shape=22)+
  scale_fill_brewer(palette="Set2")+
  labs(x="Log(OR) for PD",y="Risk factor",fill="")+
  theme_classic()+
  scale_y_discrete(labels=df$name)


png(file="noself_incident_case_control.png",height=12,width=12,res=300,units="in")
p
dev.off()

#############################################
#     4) matched analysis
#############################################

pd_cases = selected_vars %>% filter(PD_Dx_after_rec==1)
pd_controls = selected_vars %>% filter(PD_Dx_after_rec==0)

set.seed(1)
controls = data.frame()
for (i in 1:nrow(pd_cases)){
  print(paste0("matching case ",i," of ",nrow(pd_cases)))
  pd_row = pd_cases[i,]
  cont_row = pd_controls %>% 
    filter(!EID %in% controls$EID) %>% 
    filter(Sex.0.0 == pd_row$Sex.0.0) %>% 
    filter(`Age at recruitment.0.0` == pd_row$`Age at recruitment.0.0`) %>% 
    filter(`Ethnic background.0.0` == pd_row$`Ethnic background.0.0`)
  if(nrow(cont_row)<4){
    print("not enough matches found for this row. Skipping")
    next
  } else {
    cont_row = cont_row %>% sample_n(4,replace=FALSE)
    controls <<- rbind(controls,cont_row)
  }
}

selected_vars_matched = bind_rows(pd_cases,controls)

# model function
make_model = function(x){
model = glm(data=selected_vars_matched,PD_Dx_after_rec~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars_matched[[x]],family=binomial(link="logit"))
}

make_model_nosex = function(x){
model = glm(data=selected_vars_matched,PD_Dx_after_rec~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars_matched[[x]],family=binomial(link="logit"))
}

# lr functions
get_lr = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[6]
}
get_lr_nosex = function(x){
anova(x,test="Chisq")$`Pr(>Chi)`[5]
}


# make models
smok=make_model("smoking_status")
pest=make_model("Pesticides")
alc=make_model("alcohol")
dm=make_model("DM_Dx_pre_rec")
epi=make_model("epilepsy_Dx_pre_rec")
bmi=make_model("Body mass index (BMI).0.0")
cbmi=make_model("Comparative body size at age 10.0.0")
sleepiness=make_model("sleepiness")
dep=make_model("depression_Dx_pre_rec")
anx=make_model("Anxiety_Dx_pre_rec")
mig=make_model("migraine_Dx_pre_rec")
constipation=make_model("constipation_Dx_pre_rec")
head_injury=make_model("head_injury_Dx_pre_rec")
Hypertension=make_model("Hypertension_Dx_pre_rec")
PD_FHx=make_model("PD_FHx")
Dementia_FHx=make_model("Dementia_FHx")
Depression_FHx=make_model("Depression_FHx")
Diabetes_FHx=make_model("Diabetes_FHx")
Stroke_FHx=make_model("Stroke_FHx")
breastfed=make_model("Breastfed as a baby.0.0")
mat_smok=make_model("Maternal smoking around birth.0.0")
handedness=make_model("handedness")
coffee=make_model("Coffee intake.0.0")
edu=make_model("Age completed full time education.0.0")
menarche=make_model_nosex("Age when periods started (menarche).0.0")
voicebreak=make_model_nosex("Relative age voice broke.0.0")
Gastric_ulcer=make_model("Gastric_ulcer_Dx_pre_rec")


models=list(smok,pest,alc,dm,epi,bmi,cbmi,sleepiness,dep,anx,mig,constipation,head_injury,Hypertension,PD_FHx,Dementia_FHx,Depression_FHx,Diabetes_FHx,Stroke_FHx,breastfed,mat_smok,handedness,coffee,edu,Gastric_ulcer)
coef_df = data.frame(models=c("smok","pest","alc","dm","epi","bmi","cbmi","sleepiness","dep","anx","mig","constipation","head_injury","Hypertension","PD_FHx","Dementia_FHx","Depression_FHx","Diabetes_FHx","Stroke_FHx","breastfed","mat_smok","handedness","coffee","edu","gastric ulcer"))
coef_df$LR_P = sapply(models,get_lr)
models2=list(menarche,voicebreak)
coef_df2 = data.frame(models=c("menarche","voicebreak"))
coef_df2$LR_P = sapply(models2,get_lr_nosex)
coef_df = bind_rows(coef_df,coef_df2)


df=rbind(summary(smok)$coefficients,summary(pest)$coefficients,summary(alc)$coefficients,summary(dm)$coefficients,summary(epi)$coefficients,summary(bmi)$coefficients,summary(cbmi)$coefficients,summary(sleepiness)$coefficients,summary(dep)$coefficients,summary(anx)$coefficients,summary(mig)$coefficients,summary(constipation)$coefficients,summary(head_injury)$coefficients,summary(Hypertension)$coefficients,summary(PD_FHx)$coefficients,summary(Dementia_FHx)$coefficients,summary(Depression_FHx)$coefficients,summary(Diabetes_FHx)$coefficients,summary(Stroke_FHx)$coefficients,summary(breastfed)$coefficients,summary(mat_smok)$coefficients,summary(handedness)$coefficients,summary(coffee)$coefficients,summary(edu)$coefficients,summary(Gastric_ulcer)$coefficients,summary(menarche)$coefficients,summary(voicebreak)$coefficients)
df = cbind(df,rownames(df))
df = data.frame(df)
colnames(df) = c("Beta","SE","Z","P","name")
rownames(df)=NULL

df=df %>% filter(!name %in% levels(df$name)[c(1,2,3,4,14)])

# caution: manual renaming
df$name=c("Smoking status: current",
          "Smoking status: previous",
          "Pesticide exposure",
          "Alcohol: >1 drink per week",
          "Diabetes",
          "Epilepsy",      
          "BMI",
          "Childhood obesity",
          "Excessive daytime sleepiness",
          "Depression",       
          "Anxiety",
          "Migraine",
          "Constipation",
          "Head Injury",
          "Hypertension",
          "Family history: PD",
          "Family History: dementia" ,
          "Family history: depression",
          "Family history: diabetes",
          "Family history: stroke",
          "Breastfed as a baby",
          "Exposed to maternal smoking",
          "Right-handed",
          "Cups of coffee per day",
          "Age completed full time education",
          "Gastric ulcer",
          "Age at menarche",
          "Age at voice breaking: older than average",
          "Age at voice breaking: younger than average")

df$cat=c("Environmental",
"Environmental",
"Environmental",
"Environmental",
"Comorbidity",
"Comorbidity",
"Environmental",
"Early life",
"Prodrome",
"Comorbidity",
"Comorbidity",
"Comorbidity",
"Prodrome",
"Environmental",
"Comorbidity",
"Family history",
"Family history",
"Family history",
"Family history",
"Family history",
"Early life",
"Early life",
"Early life",
"Environmental",
"Early life",
"Comorbidity",
"Early life",
"Early life",
"Early life")

df = df[order(df$cat),]
df$num = factor(c(1:nrow(df)))
factor(df$name)

df$Beta = as.numeric(as.character(df$Beta))
df$SE = as.numeric(as.character(df$SE))
overall_cc_estimates = df
p=ggplot(df,aes(Beta,num,fill=cat))+
  geom_errorbarh(mapping=aes(y=num,xmin=Beta-1.96*SE,xmax=Beta+1.96*SE),height=0.3)+
  geom_vline(xintercept=0,alpha=0.3)+
  geom_point(size=5,shape=22)+
  scale_fill_brewer(palette="Set2")+
  labs(x="Log(OR) for PD",y="Risk factor",fill="")+
  theme_classic()+
  scale_y_discrete(labels=df$name)


png(file="noselfrep_case_control.png",height=12,width=12,res=300,units="in")
p
dev.off()



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


#################################
#      prs prep 
#################################
plot_table = selected_vars %>% filter(!is.na(PD_Dx_after_rec))

p = ggplot(plot_table,aes(`Genetic principal components.0.1`,`Genetic principal components.0.2`,fill=raw_ethnicity))+
  geom_point(shape=21,size=3,alpha=0.5)+
  theme_bw()+
  labs(x="PC1",y="PC2",fill="Self-reported ethnicity")+
  theme(legend.key.size = unit(1, "cm")) 

png("pc_plot_before_relatness_and_ancestry_exclusions.png",height=10,width=10,res=1000,units="in")
p
dev.off()

#############################################
# NOW USE TESTING SET
############################################
selected_vars = read_tsv("testing_set.tsv")

print("INCLUDED GROUP: TEST SET")
print("Incident cases")
print(table(selected_vars$PD_Dx_after_rec))
print("Prevalent cases")
print(table(selected_vars$PD_status))

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

########################################
# reapply predict algo to test data
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

# pc plots
plot_table = selected_vars %>% filter(!is.na(PD_Dx_after_rec))
p = ggplot(plot_table,aes(`Genetic principal components.0.1`,`Genetic principal components.0.2`,fill=raw_ethnicity))+
  geom_point(shape=21,size=3,alpha=0.5)+
  theme_bw()+
  labs(x="PC1",y="PC2",fill="Self-reported ethnicity")+
  theme(legend.key.size = unit(1, "cm")) 


png("pc_plot_1.png",height=10,width=10,res=1000,units="in")
p
dev.off()

#####################################
# analysis with best prs
#####################################

#choose best prs based on r2 and read it in
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/r2_04/summarised_PRS_results_pval0.00005")

selected_vars = selected_vars %>% select(-contains("prs"))
selected_vars$EID = as.numeric(as.character(selected_vars$EID))
selected_vars = selected_vars %>% left_join(prs,by="EID")
selected_vars = selected_vars %>% filter(!(is.na(PRS)))
selected_vars$PRS=rankNorm(selected_vars$PRS)
  
# save progress
write_tsv(selected_vars,"processed_pheno_file.tsv")
#selected_vars = read_tsv("processed_pheno_file.tsv")

# prs performance on new dataset
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
nag = nagelkerke(prs_model,null_model)$Pseudo.R.squared.for.model.vs.null[3]
print(paste0("PRS Nagelkerke R2 on test data is ",nag))
anova(prs_model,test="Chisq")

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

#################################
# calibration stats
#################################

# PREDICT + PCS
predict_pcs_model = glm(data=selected_vars,
                PD_Dx_after_rec~
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  log(baseline_risk),
                family=binomial(link="logit"))
# PCs
pcs_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`,
                family=binomial(link="logit"))

# PREDICT + PCS + PRS
prs_predict_pcs_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  PRS+
                  log(baseline_risk),
                family=binomial(link="logit"))

# PCS + PRS
prs_pcs_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  PRS,
                family=binomial(link="logit"))

make_prediction = function(x){
predict(x,newdata=selected_vars,type="response")
}

preds = data.frame("predict_pcs"=make_prediction(predict_pcs_model),
"pcs"=make_prediction(pcs_model),
"prs_predict_pcs"=make_prediction(prs_predict_pcs_model),
"prs_pcs"=make_prediction(prs_pcs_model),
"prs_decile"=selected_vars$prs_decile,
"PD"=selected_vars$PD_Dx_after_rec)


preds_summary = preds %>% group_by(prs_decile) %>% summarise(mean(predict_pcs),mean(pcs),mean(prs_predict_pcs),mean(prs_pcs))
tbl = table(selected_vars$prs_decile,selected_vars$PD_Dx_after_rec)
obs_risk = tbl[,2]/rowSums(tbl)

pred_df = data.frame(preds_summary,obs_risk)
pred_df$prs_decile = c(1:10)
library(reshape2)
pred_df = melt(pred_df,id="prs_decile")
pred_df$prs_decile = factor(pred_df$prs_decile)

calib_plot = ggplot(pred_df,aes(prs_decile,value,col=variable,group=variable))+geom_point(col="black",shape=22,alpha=0.3)+geom_line()+labs(x="PRS decile",y="PD risk (probability scale)")+scale_color_brewer(palette="Set2",labels=c("PREDICT-PD + PCs","PCs","PREDICT-PD + PCs + PRS","PCs + PRS","Observed risk"))+theme_classic()

png("/data/Wolfson-UKBB-Dobson/PD_pheno/calibration_plot.png",height=10,width=10,res=1000,units="in")
calib_plot
dev.off()

######################################
# discrimination
######################################

preds = preds %>% na.omit()
preds$PD=factor(preds$PD)
predictions = prediction(list(preds$predict_pcs,preds$pcs,preds$prs_predict_pcs,preds$prs_pcs),
list(preds$PD,preds$PD,preds$PD,preds$PD))
roc.perf = ROCR::performance(predictions, measure = "tpr", x.measure = "fpr")




PREDICT_PCS = data.frame(x=roc.perf@x.values[[1]],y=roc.perf@y.values[[1]],model="PREDICT-PD + PCs")
PCS = data.frame(x=roc.perf@x.values[[2]],y=roc.perf@y.values[[2]],model="PCs")
PREDICT_PCs_PRS = data.frame(x=roc.perf@x.values[[3]],y=roc.perf@y.values[[3]],model="PREDICT-PD + PCs + PRS")
PCs_PRS = data.frame(x=roc.perf@x.values[[4]],y=roc.perf@y.values[[4]],model="PCs + PRS")


df = bind_rows(PREDICT_PCS,PCS,PREDICT_PCs_PRS,PCs_PRS)

auc.perf = performance(predictions, measure = "auc")
auc.perf@y.values

png("/data/Wolfson-UKBB-Dobson/PD_pheno/discrimination_plot.png",height=10,width=10,res=1000,units="in")
ggplot(df,aes(x,y,col=model))+
geom_line()+
scale_color_brewer(palette="Set2")+
labs(x="False Positive Rate",y="True Positive Rate",col="Model")+
theme_classic()+
geom_abline()+
annotate("text",x=0.8,y=0.3,label="AUC",hjust=0)+
annotate("text",x=1,y=0.25,label=paste0("PCs: ",round(auc.perf@y.values[[2]],3)),hjust=1)+
annotate("text",x=1,y=0.2,label=paste0("PCs + PRS: ",round(auc.perf@y.values[[4]],3)),hjust=1)+
annotate("text",x=1,y=0.15,label=paste0("PREDICT+PCs: ",round(auc.perf@y.values[[1]],3)),hjust=1)+
annotate("text",x=1,y=0.1,label=paste0("PREDICT-PD + PCs + PRS: ",round(auc.perf@y.values[[3]],3)),hjust=1)
dev.off()




hist = ggplot(selected_vars,aes(PRS,fill=factor(PD_status)))+
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


null_model = glm(data=selected_vars,
                 PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+log(baseline_risk),
                family=binomial(link="logit"))
  
print("Nagelkerke for PRS + predict vs predict")
nagelkerke(prs_model,null_model)
summary(prs_model)



#############################
#age of onset
#############################

hist(selected_vars$age_at_pd_dx)
summary(selected_vars$age_at_pd_dx)
 
pd = selected_vars %>% filter(!is.na(age_at_pd_dx)) %>%
mutate(norm_age = rankNorm(age_at_pd_dx))
age_model=lm(data=pd,
             norm_age~`Age at recruitment.0.0`+
               `Sex.0.0`+
               `Genetic principal components.0.1`+
               `Genetic principal components.0.2`+
               `Genetic principal components.0.3`+
               `Genetic principal components.0.4`+
               PRS)
summary(age_model)
anova(age_model,test="Chisq")
age_model2=lm(data=pd,
             norm_age~PRS)

summary(age_model2)
age_plot=ggplot(pd,aes((PRS),norm_age))+geom_point()+theme_classic()+theme(text=element_text(size=14))+
  labs(y="Age at diagnosis Z score",x="PRS")+geom_smooth(method="lm",fill="red",alpha=0.3)

png("prs_age_of_dx.png",height=10,width=10,res=1000,units="in")
age_plot
dev.off()

#########################################
# use top prs excluding pd risk loci
#########################################
#choose best prs based on r2 and read it in
prs = read_table2("/data/Wolfson-UKBB-Dobson/pd_prs/top_prs_excluding_sig_vars/summarised_PRS_results_pval0.00005")

prs = prs %>% rename("prs_no_top_hits"=PRS)
selected_vars$EID = as.numeric(as.character(selected_vars$EID))
selected_vars = selected_vars %>% left_join(prs,by="EID")
selected_vars$prs_no_top_hits=rankNorm(selected_vars$prs_no_top_hits)

prs_no_top_hits_model = glm(data=selected_vars,
                PD_Dx_after_rec~`Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  log(baseline_risk)+prs_no_top_hits,
                family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                 PD_Dx_after_rec~`Genetic principal components.0.1`+
                   `Genetic principal components.0.2`+
                   `Genetic principal components.0.3`+
                   `Genetic principal components.0.4`+log(baseline_risk),
                 family=binomial(link="logit"))
print("Nagelkerke for prs_no_top_hits + predict vs predict")
nagelkerke(prs_no_top_hits_model,null_model)


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
selected_vars$prs_decile = cut2(selected_vars$PRS,g=2)
levels(selected_vars$prs_decile)=c(1:2)

make_model = function(x){
model = glm(data=selected_vars %>% filter(prs_decile==x),
                  PD_Dx_after_rec~+`Age at recruitment.0.0`+
                  Sex.0.0+
                   `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    DM_Dx_pre_rec,
                  family=binomial(link="logit"))
summary(model)$coefficients[9,c(1,2)]
}
        

tbl = sapply(c(1:2),make_model)
tbl = data.frame(t(tbl))
tbl$decile = c(1:2)
tbl$or=exp(tbl$Estimate)
tbl$lower_ci=exp(tbl$Estimate-1.96*tbl$Std..Error)
tbl$upper_ci=exp(tbl$Estimate+1.96*tbl$Std..Error)
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

# set up parallel
args=commandArgs(trailingOnly=TRUE)
print(paste0("Number of bootstrap iterations:",args[1]))
trials = as.numeric(as.character(args[1]))

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

######################################
# CV of PRS with excluded cases
######################################
# exclude related people
kin = read_table2("/data/Wolfson-UKBB-Dobson/helper_progs_and_key/ukb43101_rel_s488282.dat")
exclusion = kin %>% filter(Kinship>0.0442) %>% select(ID1) %>% rename("EID"="ID1") 

selected_vars = selected_vars %>% filter(`Genetic ethnic grouping.0.0`=="Caucasian") %>% filter(!EID %in% exclusion$EID)
set.seed(1)
prs_tuning_dataset = sample_frac(selected_vars,size=0.3,replace=FALSE)
print("Incident cases")
print(table(prs_tuning_dataset$PD_Dx_after_rec))
print("Prevalent cases")
print(table(prs_tuning_dataset$PD_status))
selected_vars = selected_vars %>% filter(!EID %in% prs_tuning_dataset$EID)
write_tsv(selected_vars,"testing_set.tsv")

library(doParallel)
registerDoParallel(cores=as.numeric(as.character(args[2])))


ses = c()
score_prs = function(pval,r2){

# read in prs
filename = paste0("/data/Wolfson-UKBB-Dobson/pd_prs/r2_",r2,"/summarised_PRS_results_pval",pval)
prs = read_table2(filename)

prs_tuning_dataset = prs_tuning_dataset %>% select(-contains("prs"))
prs_tuning_dataset$EID = as.numeric(as.character(prs_tuning_dataset$EID))
prs_tuning_dataset = prs_tuning_dataset %>% left_join(prs,by="EID")
prs_tuning_dataset = prs_tuning_dataset %>% filter(!(is.na(PRS)))
prs_tuning_dataset$PRS=rankNorm(prs_tuning_dataset$PRS)


r = foreach(icount(trials), .combine=cbind) %dopar% {
prs_tuning_dataset = sample_n(prs_tuning_dataset, size=nrow(prs_tuning_dataset), replace=TRUE)
null_model = glm(data=prs_tuning_dataset,
                   PD_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

prs_model = glm(data=prs_tuning_dataset,
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
}

se = sd(r)/sqrt(length(r))
ses <<- c(ses,se)
}

pvals = c("1","0.8","0.6","0.4","0.2","0.1","0.05","0.005", "0.0005", "0.00005")
r2 = c("01","02","04","06","08")

for(i in 1:length(pvals)){
  for(j in 1:length(r2)){
    score_prs(pval=pvals[i],r2=r2[j])
  }
}



# repeat to get actual values
absolute_nagelkerkes = c()
score_prs = function(pval,r2){

# read in prs
filename = paste0("/data/Wolfson-UKBB-Dobson/pd_prs/r2_",r2,"/summarised_PRS_results_pval",pval)
prs = read_table2(filename)

prs_tuning_dataset = prs_tuning_dataset %>% select(-contains("prs"))
prs_tuning_dataset$EID = as.numeric(as.character(prs_tuning_dataset$EID))
prs_tuning_dataset = prs_tuning_dataset %>% left_join(prs,by="EID")
prs_tuning_dataset = prs_tuning_dataset %>% filter(!(is.na(PRS)))
prs_tuning_dataset$PRS=rankNorm(prs_tuning_dataset$PRS)

null_model = glm(data=prs_tuning_dataset,
                   PD_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

prs_model = glm(data=prs_tuning_dataset,
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

absolute_nagelkerkes <<- c(absolute_nagelkerkes,nag)
}

for(i in 1:length(pvals)){
  for(j in 1:length(r2)){
    score_prs(pval=pvals[i],r2=r2[j])
  }
}

pvals = c(rep("1",5),rep("0.8",5),rep("0.6",5),rep("0.4",5),rep("0.2",5),rep("0.1",5),rep("0.05",5),rep("0.005",5),rep("0.0005",5),rep("0.00005",5))
r2s=rep(c("0.1","0.2","0.4","0.6","0.8"),5)
prs_parameters = data.frame("Pval"=pvals,
"R2"=r2s,
"SE"=ses,
"Nagelkerke_Pseudo_R2"=absolute_nagelkerkes)
prs_parameters = prs_parameters %>% mutate("Lower_CI"=absolute_nagelkerkes-1.96*SE) %>% 
mutate("Upper_CI"=absolute_nagelkerkes+1.96*SE)

prs_parameters$R2 = r2s

nagel_plot = ggplot(prs_parameters,aes(factor(Pval),Nagelkerke_Pseudo_R2,fill=factor(R2)))+
  geom_col(position=position_dodge(),col="black")+
  theme_classic()+
  geom_errorbar(aes(x=factor(Pval),ymin=Lower_CI,ymax=Upper_CI),position=position_dodge())+
  scale_fill_brewer(palette = "Set2")+
  labs(x="P value threshold",y="Nagelkerke PseudoR2",fill="Clumping R2 parameter")+
  theme(text=element_text(size=14))

png("bootstrapped_nagel_plot.png",height=10,width=10,res=1000,units="in")
nagel_plot
dev.off()

write_csv(prs_parameters,"prs_nagelkerke_training.csv")




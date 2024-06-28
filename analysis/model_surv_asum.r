## Import libraries
library('tidyverse')
library('here')
library('lubridate')
library('arrow')
library('dplyr')
library('readr')
library('fs')
library('survival')
library('survminer')
library('splines')
library('gtsummary')
library('ggpubr')
library('broom')

## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "graps"), showWarnings = FALSE, recurse = TRUE)
#high_risk_surv_data<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort.csv") %>%
high_risk_surv_data <- read_csv(here::here("output", "data", "high_risk_cohort.csv")) %>%
    select(patient_id, age_treated, imd, imd_num, drug, bmi, bmi_cat_num, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, 
    surv6m_days, surv6m_days_from_treat, surv6m_event, surv6m_event_underly, surv6m_event_num, surv6m_event_underly_num,
    surv12m_days, surv12m_days_from_treat, surv12m_event, surv12m_event_underly, surv12m_event_num, surv12m_event_underly_num,  
    surv24m_days, surv24m_days_from_treat, surv24m_event, surv24m_event_underly, surv24m_event_num, surv24m_event_underly_num,     
    calendar_day, calendar_wk, high_risk_num, diabetes, hypertension, chronic_cardiac_disease,
    chronic_respiratory_disease, autism, learning_disability, serious_mental_illness, dementia) %>% #age_treated_spline, calendar_day_spline, 
    mutate(age_treated_spline = ns(age_treated, df = 4),
    calendar_day_spline = ns(calendar_day, df = 4))%>% 
    mutate(across(c(imd_num, drug, bmi_cat_num, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, high_risk_num, diabetes,hypertension,chronic_cardiac_disease,
    chronic_respiratory_disease, autism, learning_disability, serious_mental_illness, dementia), as.factor))

str(high_risk_surv_data, list.len = ncol(high_risk_surv_data), give.attr= F)

cat("#freq_single(high_risk_surv_data$ethnicity_num)")
freq_single(high_risk_surv_data$ethnicity_num)

cat("#high_risk_surv_data$drug, high_risk_surv_data$surv6m_event_num")
table(high_risk_surv_data$drug, high_risk_surv_data$surv6m_event_num)
cat("#high_risk_surv_data$stp, high_risk_surv_data$surv6m_event_num")
table(high_risk_surv_data$stp, high_risk_surv_data$surv6m_event_num)

options(scipen = 999)
#strata(region_num)
cat("#summary(cox_model0)")
cox_model0 <- coxph(Surv(surv6m_days, surv6m_event_num) ~ (drug), data = high_risk_surv_data) 
summary(cox_model0)

cat("#1summary(cox_model_strata(region_num))")
cox_model_region<- coxph(Surv(surv6m_days, surv6m_event_num) ~ (drug)+ strata(region_num), data = high_risk_surv_data) 
summary(cox_model_region)
cox_model_region_fit<-cox.zph(cox_model_region)

ploting<- function(modl_fit, figname){
    plot<-ggcoxzph(fit)
    ggsave(here::here("output", "graps", paste0("plot_",figname,".png")))
 }


cat("#2summary(cox_model_strata(stp))")
cox_model_stp <- coxph(Surv(surv6m_days, surv6m_event_num) ~ (drug)+ strata(stp), data = high_risk_surv_data)
summary(cox_model_stp)

##age_treated, sex_num
cat("#Model01_1summary(cox_model1_age_sex_strata_region )")
cox_model1_age_sex_region <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
+ strata(region_num), data = high_risk_surv_data)
summary(cox_model1_age_sex_region)

cox_model1_age_sex_region_fit<-cox.zph(cox_model1_age_sex_region)

cat("#Model01_cox.zph(cox_model1_age_sex_region)")
cox_model1_age_sex_region_fit

cat("#Model02_1summary(cox_model1_age_sex_highrisk_(region_num)")
cox_model1_age_sex_highrisk_region <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
+ high_risk_num + strata(region_num), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk_region)

#cox_model1_age_sex_highrisk_region_fit<-cox.zph(cox_model1_age_sex_highrisk_region)


cat("#Model03_1summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region )")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
    + high_risk_num + covid_vacc_num + imd_num + ethnicity_num + ns(calendar_day, df = 4) + strata(region_num), data = high_risk_surv_data) 
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region)

#cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region_fit<-cox.zph(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region)

cat("#Model05_1summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_excl_calday_region )")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth__excl_calday_region <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
    + high_risk_num + covid_vacc_num + imd_num + ethnicity_num + strata(region_num), data = high_risk_surv_data) 
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth__excl_calday_region)

# Extract information using broom::tidy
#organised output#
surv0_regn1<-org_cox_mod(cox_model1_age_sex_region)
surv0_regn1_rd2<-org_cox_mod_rd2(cox_model1_age_sex_region)

surv0_regn2<-org_cox_mod(cox_model1_age_sex_highrisk_region)
surv0_regn2_rd2<-org_cox_mod_rd2(cox_model1_age_sex_highrisk_region)

surv0_regn3<-org_cox_mod(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region)
surv0_regn3_rd2<-org_cox_mod_rd2(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region)

# surv0_regn4<-org_cox_mod(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_region)
# surv0_regn4_rd2<-org_cox_mod_rd2(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_region)

surv0_regn5<-org_cox_mod(cox_model1_age_sex_highrisk_vacc_imd_reg_eth__excl_calday_region)
surv0_regn5_rd2<-org_cox_mod_rd2(cox_model1_age_sex_highrisk_vacc_imd_reg_eth__excl_calday_region)

# surv0_regn6<-org_cox_mod(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_excl_calday_region)
# surv0_regn6_rd2<-org_cox_mod_rd2(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_excl_calday_region)

surv0_regn<-rbind("#1surv0_regn",surv0_regn1, surv0_regn2, surv0_regn3, surv0_regn5)
surv0_regn_rd2<-rbind("#1surv0_regn",surv0_regn1_rd2, surv0_regn2_rd2, surv0_regn3_rd2, surv0_regn5_rd2)

# surv0_regn<-rbind("#1surv0_regn1", surv0_regn1,"#1surv0_regn2",surv0_regn2, "#1surv0_regn3", surv0_regn3,"#1surv0_regn4", surv0_regn4, "#1surv0_regn5", surv0_regn5, "#1surv0_regn6", surv0_regn6)
# surv0_regn_rd2<-rbind("#1surv0_regn1", surv0_regn1_rd2, "#1surv0_regn2", surv0_regn2_rd2, "#1surv0_regn3", surv0_regn3_rd2, "#1surv0_regn4", surv0_regn4_rd2, "#1surv0_regn5", surv0_regn5_rd2, "#1surv0_regn6", surv0_regn6_rd2)

ploting(cox_model1_age_sex_region_fit, "cox_model1_region")
# ploting(cox_model1_age_sex_highrisk_region_fit, "cox_model2_region")
# ploting(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region_fit, "cox_model3_region")

# Save dataset(s) ----surv0_regn
write.csv(high_risk_surv_data, here::here("output", "data", "high_risk_surv_data_plot.csv"))

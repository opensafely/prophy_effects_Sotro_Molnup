
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

## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)

#high_risk_surv_data2<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort.csv") %>%
high_risk_surv_data2 <- read_csv(here::here("output", "data", "high_risk_cohort.csv")) %>%
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

str(high_risk_surv_data2, list.len = ncol(high_risk_surv_data2), give.attr= F)

cat("#freq_single(high_risk_surv_data2$ethnicity_num)")
freq_single(high_risk_surv_data2$ethnicity_num)
##variables <- c("age_treat_gp_rc", "sex", "sex_num","surv6m_event", "ethnicity", "region", "total_covid_vacc_cat", "first_covid_treat_interve")
##variables2 <- c("imd", "first_covid_treat_interve","high_risk_group")

cat("#high_risk_surv_data2$drug, high_risk_surv_data2$surv24m_event_num")
table(high_risk_surv_data2$drug, high_risk_surv_data2$surv24m_event_num)
cat("#high_risk_surv_data2$stp, high_risk_surv_data2$surv24m_event_num")
table(high_risk_surv_data2$stp, high_risk_surv_data2$surv24m_event_num)

#strata(region_num)
cat("#summary(cox_model0)")
cox_model0 <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ (drug), data = high_risk_surv_data2) 
summary(cox_model0)

cat("#1summary(cox_model_strata(region_num))")
cox_model_region<- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ (drug)+ strata(region_num), data = high_risk_surv_data2)
summary(cox_model_region)


cat("#2summary(cox_model_strata(stp))")
cox_model_stp <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ (drug)+ strata(stp), data = high_risk_surv_data2)
summary(cox_model_stp)

##age_treated, sex_num
cat("#1summary(cox_model1_age_sex_strata(region_num) )")
cox_model1_age_sex_region <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated + sex_num 
+ strata(region_num), data = high_risk_surv_data2) 
summary(cox_model1_age_sex_region)

cat("#2summary(cox_model1_age_sex_strata(stp) )")
cox_model1_age_sex_stp <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated + sex_num 
+ strata(stp), data = high_risk_surv_data2) 
summary(cox_model1_age_sex_stp)


cat("#1summary(cox_model1_age_sex_highrisk_region_num )")
cox_model1_age_sex_highrisk_region <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated + sex_num 
+ high_risk_num + strata(region_num), data = high_risk_surv_data2)
summary(cox_model1_age_sex_highrisk_region)

cat("#2summary(cox_model1_age_sex_highrisk_stp )")
cox_model1_age_sex_highrisk_stp <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated + sex_num 
+ high_risk_num + strata(stp), data = high_risk_surv_data2)
summary(cox_model1_age_sex_highrisk_stp)


cat("#1summary(cox_model1_age_sex_highrisk_vacc_region )")
cox_model1_age_sex_highrisk_vacc_region <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated + sex_num 
+ high_risk_num + covid_vacc_num + strata(region_num), data = high_risk_surv_data2)
summary(cox_model1_age_sex_highrisk_vacc_region)

cat("#2summary(cox_model1_age_sex_highrisk_vacc_stp )")
cox_model1_age_sex_highrisk_vacc_stp <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated + sex_num 
+ high_risk_num + covid_vacc_num + strata(stp), data = high_risk_surv_data2)
summary(cox_model1_age_sex_highrisk_vacc_stp)

cat("#1summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region )")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated + sex_num 
+ high_risk_num + covid_vacc_num + imd_num + ethnicity_num + strata(region_num), data = high_risk_surv_data2)
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region)
cat("#2summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_stp )")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth_stp <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated + sex_num 
+ high_risk_num + covid_vacc_num + imd_num + ethnicity_num + strata(stp), data = high_risk_surv_data2)
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_stp)

cat("#1summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb)+ strata(region_num)")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated 
+ sex_num + high_risk_num + covid_vacc_num + imd_num + ethnicity_num + bmi_cat_num + diabetes + hypertension 
+ chronic_cardiac_disease + chronic_respiratory_disease + strata(region_num), data = high_risk_surv_data2)
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb)


cat("#1summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb)+ strata(stp)")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_stp <- coxph(Surv(surv24m_days_from_treat, surv24m_event_num) ~ drug + age_treated 
+ sex_num + high_risk_num + covid_vacc_num + imd_num + ethnicity_num + bmi_cat_num + diabetes + hypertension 
+ chronic_cardiac_disease + chronic_respiratory_disease + strata(stp), data = high_risk_surv_data2)
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_stp)

# Plot the survival curves
# ggsurvplot(survfit(cox_model), data = high_risk_surv_data2, pval = TRUE,
#            ggtheme = theme_minimal(), risk.table = TRUE,
#            conf.int = TRUE)

# Save dataset(s) ----
write.csv(high_risk_surv_data2, here::here("output", "data", "high_risk_surv_data02.csv"))

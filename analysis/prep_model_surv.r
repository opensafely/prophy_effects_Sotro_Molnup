
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

##surv6m_days, surv6m_days_from_treat, surv6m_event, surv6m_event_underly, surv6m_event_num, surv6m_event_underly_num

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
##variables <- c("age_treat_gp_rc", "sex", "sex_num","surv6m_event", "ethnicity", "region", "total_covid_vacc_cat", "first_covid_treat_interve")
##variables2 <- c("imd", "first_covid_treat_interve","high_risk_group")

cat("#high_risk_surv_data$drug, high_risk_surv_data$surv6m_event_num")
table(high_risk_surv_data$drug, high_risk_surv_data$surv6m_event_num)
cat("#high_risk_surv_data$stp, high_risk_surv_data$surv6m_event_num")
table(high_risk_surv_data$stp, high_risk_surv_data$surv6m_event_num)

# (1) age and sex; (2) high-risk groups; 
# (3) ethnic background, deprivation, vaccination status, calendar week;
# (4) body mass index, diabetes, hypertension, and chronic heart and lung diseases.

options(scipen = 999)
#strata(region_num)
cat("#summary(cox_model0)")
cox_model0 <- coxph(Surv(surv6m_days, surv6m_event_num) ~ (drug), data = high_risk_surv_data) 
summary(cox_model0)

cat("#1summary(cox_model_strata(region_num))")
cox_model_region<- coxph(Surv(surv6m_days, surv6m_event_num) ~ (drug)+ strata(region_num), data = high_risk_surv_data) 
summary(cox_model_region)

cat("#2summary(cox_model_strata(stp))")
cox_model_stp <- coxph(Surv(surv6m_days, surv6m_event_num) ~ (drug)+ strata(stp), data = high_risk_surv_data)
summary(cox_model_stp)

##age_treated, sex_num
cat("#Model01_1summary(cox_model1_age_sex_strata(region_num) )")
cox_model1_age_sex_region <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
+ strata(region_num), data = high_risk_surv_data)
summary(cox_model1_age_sex_region)

cat("#Model02_1summary(cox_model1_age_sex_highrisk_(region_num)")
cox_model1_age_sex_highrisk_region <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
+ high_risk_num + strata(region_num), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk_region)

cat("#Model03_1summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region )")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
    + high_risk_num + covid_vacc_num + imd_num + ethnicity_num + ns(calendar_day, df = 4) + strata(region_num), data = high_risk_surv_data) 
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_region)

cat("#Model04_1summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_region )+ strata(region_num)")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_region <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated 
+ sex_num + high_risk_num + covid_vacc_num + imd_num + ethnicity_num + ns(calendar_day, df = 4) + bmi_cat_num + diabetes + hypertension 
+ chronic_cardiac_disease + chronic_respiratory_disease + strata(region_num), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_region)

cat("#Model01_2summary(cox_model1_age_sex_strata(stp) )")
cox_model1_age_sex_stp <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
+ strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex_stp)

cat("#Model02_2summary(cox_model1_age_sex_highrisk_(stp)")
cox_model1_age_sex_highrisk_stp <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
+ high_risk_num + strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk_stp)

cat("#Model03_2summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_stp)")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth_stp <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated + sex_num 
    + high_risk_num + covid_vacc_num + imd_num + ethnicity_num + ns(calendar_day, df = 4) + strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_stp)

cat("##Model04_1summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb)+ strata(stp)")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_stp <- coxph(Surv(surv6m_days, surv6m_event_num) ~ drug + age_treated 
+ sex_num + high_risk_num + covid_vacc_num + imd_num + ethnicity_num + ns(calendar_day, df = 4) + bmi_cat_num + diabetes + hypertension 
+ chronic_cardiac_disease + chronic_respiratory_disease + strata(stp), data = high_risk_surv_data) %>% summary()
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb_stp)

#covid_vacc_num
cat("#freq_single(high_risk_surv_data$covid_vacc_num)")
freq_single(high_risk_surv_data$covid_vacc_num)
cat("#sum(is.na(covid_vacc_num)):")
sum(is.na(high_risk_surv_data$covid_vacc_num))
cat("#sum(!is.na(covid_vacc_num)):")
sum(!is.na(high_risk_surv_data$covid_vacc_num))

#covid_vacc_num1
cat("#freq_single(high_risk_surv_data$covid_vacc_num1)")
freq_single(high_risk_surv_data$covid_vacc_num1)
cat("#sum(is.na(covid_vacc_num1)):")
sum(is.na(high_risk_surv_data$covid_vacc_num1))
cat("#sum(!is.na(covid_vacc_num1)):")
sum(!is.na(high_risk_surv_data$covid_vacc_num1))

#imd_num
cat("#freq_single(high_risk_surv_data$imd_num)")
freq_single(high_risk_surv_data$imd_num)
cat("#sum(is.na(imd_num)):")
sum(is.na(high_risk_surv_data$imd_num))
cat("#sum(!is.na(imd_num)):")
sum(!is.na(high_risk_surv_data$imd_num))

#region_num
cat("#freq_single(high_risk_surv_data$region_num)")
freq_single(high_risk_surv_data$region_num)
cat("#sum(is.na(region_num)):")
sum(is.na(high_risk_surv_data$region_num))
cat("#sum(!is.na(region_num)):")
sum(!is.na(high_risk_surv_data$region_num))

#ethnicity_num
cat("#freq_single(high_risk_surv_data$ethnicity_num)")
freq_single(high_risk_surv_data$ethnicity_num)
cat("#sum(is.na(ethnicity_num)):")
sum(is.na(high_risk_surv_data$ethnicity_num))
cat("#sum(!is.na(ethnicity_num)):")
sum(!is.na(high_risk_surv_data$ethnicity_num))

#bmi
cat("#sum(is.na(bmi)):")
sum(is.na(high_risk_surv_data$bmi))
cat("#sum(!is.na(bmi)):")
sum(!is.na(high_risk_surv_data$bmi))
#bmi, bmi_cat_num
cat("#freq_single(high_risk_surv_data$bmi_cat_num)")
freq_single(high_risk_surv_data$bmi_cat_num)

#diabetes
cat("#freq_single(high_risk_surv_data$diabetes)")
freq_single(high_risk_surv_data$diabetes)
cat("#sum(is.na(diabetes)):")
sum(is.na(high_risk_surv_data$diabetes))
cat("#sum(!is.na(diabetes)):")
sum(!is.na(high_risk_surv_data$diabetes))

# hypertension
cat("#freq_single(high_risk_surv_data$hypertension)")
freq_single(high_risk_surv_data$hypertension)
cat("#sum(is.na(hypertension)):")
sum(is.na(high_risk_surv_data$hypertension))
cat("#sum(!is.na(hypertension)):")
sum(!is.na(high_risk_surv_data$hypertension))

 #+ chronic_cardiac_disease 
cat("#freq_single(high_risk_surv_data$chronic_cardiac_disease)")
freq_single(high_risk_surv_data$chronic_cardiac_disease)
cat("#sum(is.na(chronic_cardiac_disease)):")
sum(is.na(high_risk_surv_data$chronic_cardiac_disease))
cat("#sum(!is.na(chronic_cardiac_disease)):")
sum(!is.na(high_risk_surv_data$chronic_cardiac_disease))

 # chronic_respiratory_disease 
cat("#freq_single(high_risk_surv_data$chronic_respiratory_disease)")
freq_single(high_risk_surv_data$chronic_respiratory_disease)
cat("#sum(is.na(chronic_respiratory_disease)):")
sum(is.na(high_risk_surv_data$chronic_respiratory_disease))
cat("#sum(!is.na(chronic_respiratory_disease)):")
sum(!is.na(high_risk_surv_data$chronic_respiratory_disease))

 # autism 
 cat("#freq_single(high_risk_surv_data$autism)")
freq_single(high_risk_surv_data$autism)
cat("#sum(is.na(autism)):")
sum(is.na(high_risk_surv_data$autism))
cat("#sum(!is.na(autism)):")
sum(!is.na(high_risk_surv_data$autism))

# learning_disability 
cat("#freq_single(high_risk_surv_data$learning_disability)")
freq_single(high_risk_surv_data$learning_disability)
cat("#sum(is.na(learning_disability)):")
sum(is.na(high_risk_surv_data$learning_disability))
cat("#sum(!is.na(learning_disability)):")
sum(!is.na(high_risk_surv_data$learning_disability))

# serious_mental_illness 
cat("#freq_single(high_risk_surv_data$serious_mental_illness)")
freq_single(high_risk_surv_data$serious_mental_illness)
cat("#sum(is.na(serious_mental_illness)):")
sum(is.na(high_risk_surv_data$serious_mental_illness))
cat("#sum(!is.na(serious_mental_illness)):")
sum(!is.na(high_risk_surv_data$serious_mental_illness))

# dementia 
cat("#freq_single(high_risk_surv_data$dementia)")
freq_single(high_risk_surv_data$dementia)
cat("#sum(is.na(dementia)):")
sum(is.na(high_risk_surv_data$dementia))
cat("#sum(!is.na(dementia)):")
sum(!is.na(high_risk_surv_data$dementia))



# Plot the survival curves
# ggsurvplot(survfit(cox_model), data = high_risk_surv_data, pval = TRUE,
#            ggtheme = theme_minimal(), risk.table = TRUE,
#            conf.int = TRUE)

#####################################################################################
# Save dataset(s) ----
write.csv(high_risk_surv_data, here::here("output", "data", "high_risk_surv_data.csv"))
####################################################################################
#####################################################################################

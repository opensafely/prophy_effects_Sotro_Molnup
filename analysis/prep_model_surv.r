
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

#high_risk_surv_data<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort.csv") %>%
high_risk_surv_data <- read_csv(here::here("output", "data", "high_risk_cohort.csv")) %>%
    select(patient_id, age_treated, imd, imd_num, drug, bmi, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, 
    surv_event_num, surv_event_underly, calendar_day,calendar_wk, surv_days, surv_from_treat_days, surv_event, high_risk_num, diabetes, hypertension, chronic_cardiac_disease,
    chronic_respiratory_disease, autism, learning_disability, serious_mental_illness, dementia) %>% #age_treated_spline, calendar_day_spline, 
    mutate(age_treated_spline = ns(age_treated, df = 4),
    calendar_day_spline = ns(calendar_day, df = 4))%>% 
    mutate(across(c(imd_num, drug, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, high_risk_num, diabetes,hypertension,chronic_cardiac_disease,
    chronic_respiratory_disease, autism, learning_disability, serious_mental_illness, dementia), as.factor))

str(high_risk_surv_data, list.len = ncol(high_risk_surv_data), give.attr= F)

cat("#freq_single(high_risk_surv_data$ethnicity_num)")
freq_single(high_risk_surv_data$ethnicity_num)
##variables <- c("age_treat_gp_rc", "sex", "sex_num","surv_event", "ethnicity", "region", "total_covid_vacc_cat", "first_covid_treat_interve")
##variables2 <- c("imd", "first_covid_treat_interve","high_risk_group")
#####################################################################################
#surv_days,surv_from_treat_days,surv_event
cat("#surv_days-high_risk_surv_data:")
mean(as.numeric(high_risk_surv_data$surv_days),na.rm=T)
cat("#surv_days-sd-high_risk_surv_data:")
sd(as.numeric(high_risk_surv_data$surv_days),na.rm=T)
cat("#surv_days-IQR-high_risk_surv_data:")
IQR(as.numeric(high_risk_surv_data$surv_days), na.rm=T)

cat("##surv_days-sum(is.na):")
sum(is.na(high_risk_surv_data$surv_days))
cat("##surv_days-sum(!is.na):")
sum(!is.na(high_risk_surv_data$surv_days))

cat("#surv_days-sum(is.na):")
sum(is.na(high_risk_surv_data$surv_from_treat_days))
cat("#surv_days-sum(!is.na):")
sum(!is.na(high_risk_surv_data$surv_from_treat_days))

cat("#sum(is.na(surv_event)):")
sum(is.na(high_risk_surv_data$surv_event))
cat("#sum(!is.na(surv_event)):")
sum(!is.na(high_risk_surv_data$surv_event))

cat("#sum(is.na(drug)):")
sum(is.na(high_risk_surv_data$drug))
cat("#sum(!is.na(drug)):")
sum(!is.na(high_risk_surv_data$drug))

cat("#sum(is.na(stp)):")
sum(is.na(high_risk_surv_data$stp))
cat("#sum(!is.na(stp)):")
sum(!is.na(high_risk_surv_data$stp))

#surv_days,surv_from_treat_days,surv_event
cat("#surv_from_treat_days-high_risk_surv_data:")
mean(as.numeric(high_risk_surv_data$surv_from_treat_days),na.rm=T)
cat("#surv_days-sd-high_risk_surv_data:")
sd(as.numeric(high_risk_surv_data$surv_from_treat_days),na.rm=T)
cat("#surv_days-IQR-high_risk_surv_data:")
IQR(as.numeric(high_risk_surv_data$surv_from_treat_days), na.rm=T)


# # cat("#summary(cox_model0)")
cox_model0 <- coxph(Surv(surv_days, surv_event_num) ~ (drug), data = high_risk_surv_data)
summary(cox_model0)

cat("#summary(cox_model_strata(stp))")
cox_model <- coxph(Surv(surv_days, surv_event_num) ~ (drug)+ strata(stp), data = high_risk_surv_data)
summary(cox_model)

table(high_risk_surv_data$drug, high_risk_surv_data$surv_event_num)
table(high_risk_surv_data$stp, high_risk_surv_data$surv_event_num)

# #age_treated, sex_num
cat("#summary(cox_model1_age_sex )")
cox_model1_age_sex <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num+ strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex)

cat("#summary(cox_model1_age_sex_highrisk )")
cox_model1_age_sex_highrisk <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num + high_risk_num + strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk)

cat("#summary(cox_model1_age_sex_highrisk_vacc )")
cox_model1_age_sex_highrisk_vacc <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num + high_risk_num + covid_vacc_num + strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk_vacc)

# cat("#summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth )")
# cox_model1_age_sex_highrisk_vacc_imd_reg_eth <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num + high_risk_num + covid_vacc_num + imd_num + region_num + ethnicity_num + strata(stp), data = high_risk_surv_data)
# summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth)

# cat("#summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb )")
# cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num + high_risk_num + covid_vacc_num + imd_num + region_num + ethnicity_num + diabetes + hypertension + chronic_cardiac_disease + chronic_respiratory_disease + autism + learning_disability + serious_mental_illness + dementia + strata(stp), data = high_risk_surv_data)
# summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb)

# imd_num, drug, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, high_risk_num, diabetes,hypertension,chronic_cardiac_disease,
#     chronic_respiratory_disease, autism, learning_disability, serious_mental_illness, dementia

# Plot the survival curves
# ggsurvplot(survfit(cox_model), data = high_risk_surv_data, pval = TRUE,
#            ggtheme = theme_minimal(), risk.table = TRUE,
#            conf.int = TRUE)


#####################################################################################
# Save dataset(s) ----
write.csv(high_risk_surv_data, here::here("output", "data", "high_risk_surv_data.csv"))
####################################################################################
#####################################################################################

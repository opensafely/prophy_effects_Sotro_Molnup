
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
library('purrr')
library('tidyr')
## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)
#c("age_treated", "bmi" , "calendar_wk", "vacc_last_treat_days")
#high_risk_basecomp_data<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort.csv") %>%
high_risk_basecomp_data <- read_csv(here::here("output", "data", "high_risk_cohort.csv")) %>%
    select(patient_id, age_treated, imd, imd_num, drug, bmi, bmi_cat_num, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, 
    calendar_day, calendar_wk, vacc_last_treat_days, high_risk_num, diabetes, hypertension, chronic_cardiac_disease,
    chronic_respiratory_disease, autism, serious_mental_illness, first_covid_treat_interve) %>% #age_treated_spline, calendar_day_spline, 
    mutate(across(c(imd, imd_num, drug, bmi_cat_num, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, high_risk_num, diabetes, hypertension, chronic_cardiac_disease,
    chronic_respiratory_disease, autism, serious_mental_illness, first_covid_treat_interve), as.factor)) %>% 
    mutate(across(c(bmi,calendar_wk), as.numeric))

str(high_risk_basecomp_data, list.len = ncol(high_risk_basecomp_data), give.attr= F)

sum_output <- high_risk_basecomp_data %>% select(age_treated,bmi,calendar_wk,vacc_last_treat_days)%>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(across(value, list(mean_sd = ~ sum_var(.)$mean_sd, 
                               median_iqr = ~ sum_var(.)$median_iqr), 
                   .names = "{col}_{fn}"))

options(scipen = 999)

cont_vars1 <- c("age_treated","calendar_wk", "vacc_last_treat_days")
t_test_output <- ttests(data=high_risk_basecomp_data, cont_vars1, ("drug"))
ranksum_results <- map_df(cont_vars1, ~ ranksum_test(high_risk_basecomp_data, .x, "drug"))

cat_vars3 <- c("sex_num", "imd_num", "bmi_cat_num", "region_num","ethnicity_num","covid_vacc_num","high_risk_num","diabetes",
 "hypertension","chronic_cardiac_disease", "autism","serious_mental_illness")

chi_sq_output <- map_df(cat_vars3, ~ chi_sq(high_risk_basecomp_data, .x, "drug"))

# Save dataset(s) ----
write.csv(sum_output, here::here("output", "tables", "sum_continu_vars_output.csv"))
write.csv(t_test_output, here::here("output", "tables", "t_test_output.csv"))
write.csv(chi_sq_output, here::here("output", "tables", "chi_sq_output.csv"))
write.csv(high_risk_basecomp_data, here::here("output", "data", "high_risk_basecomp_data.csv"))




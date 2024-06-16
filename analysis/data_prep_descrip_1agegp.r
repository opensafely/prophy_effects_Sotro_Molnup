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

## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)

#high_risk_vars_age<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort.csv") %>% 
high_risk_vars_age <- read_csv(here::here("output", "data", "high_risk_cohort.csv")) %>%
  select(patient_id, age_treated, sex, age_treated_group, drug) 


#bmi_rc_cat,bmi_cat_num
high_risk_vars_age_rc<-high_risk_vars_age %>%  
    mutate(
    age_50a_cat = ifelse((age_treated<55),"age<55","age>=55"),
    age_60a_cat = ifelse((age_treated<60),"age<60","age>=60"),
    age_5ygroup_cat = fct_explicit_na(cut(as.numeric(age_treated), breaks=c(0,24.999,29.999,34.999,39.999,44.999,49.999,
    54.999,59.999,64.999,69.999,74.999,79.999,200 ),
     labels=c("18-24","25-29","30-34", "35-39","40-44","45-49","50-54", "55-59","60-64", 
     "65-69","70-74","75-79","80-200"),include.lowest = T)))

data_molnup<-high_risk_vars_age_rc %>% filter(drug == 0 )
data_sotro<-high_risk_vars_age_rc %>% filter(drug == 1 )

cat("#freq_single-high_risk_vars_age_rc$age_50a_cat\n") 
freq_single(high_risk_vars_age_rc$age_50a_cat)
cat("#freq_single-high_risk_vars_age_rc$age_60a_cat\n") 
freq_single(high_risk_vars_age_rc$age_60a_cat)
cat("#freq_single-high_risk_vars_age_rc$age_5ygroup_cat\n") 
freq_single(high_risk_vars_age_rc$age_5ygroup_cat)

cat("#freq_single-data_molnup$age_50a_cat\n") 
freq_single(data_molnup$age_50a_cat)
cat("#freq_single-data_molnup$age_60a_cat\n") 
freq_single(data_molnup$age_60a_cat)
cat("#freq_single-data_molnup$age_5ygroup_cat\n") 
freq_single(data_molnup$age_5ygroup_cat)

cat("#freq_single-data_sotro$age_50a_cat\n") 
freq_single(data_sotro$age_50a_cat)
cat("#freq_single-data_sotro$age_60a_cat\n") 
freq_single(data_sotro$age_60a_cat)
cat("#freq_single-data_sotro$age_5ygroup_cat\n") 
freq_single(data_sotro$age_5ygroup_cat)

# Save dataset(s) ----
write.csv(high_risk_vars_age_rc, here::here("output", "tables", "high_risk_vars_age_rc.csv"), row.names = FALSE)

## Import libraries
library('tidyverse')
library('here')
library('lubridate')
library('arrow')
library('dplyr')
library('readr')
library('fs')
library('gtsummary')

## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)

#high_risk_vars_age<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort.csv") %>% 
high_risk_vars_age <- read_csv(here::here("output", "data", "high_risk_cohort.csv")) %>%
  select(patient_id, age_treated, sex, age_treated_group, first_covid_treat_interve, drug) 


#bmi_rc_cat,bmi_cat_num
high_risk_vars_age_rc<-high_risk_vars_age %>%  
    mutate(
    age_50a_cat = ifelse((age_treated<55),"age<55","age>=55"),
    age_60a_cat = ifelse((age_treated<60),"age<60","age>=60"),
    age_5ygroup_cat = fct_explicit_na(cut(as.numeric(age_treated), breaks=c(0,24.999,29.999,34.999,39.999,44.999,49.999,
    54.999,59.999,64.999,69.999,74.999,79.999,200 ),
     labels=c("18-24","25-29","30-34", "35-39","40-44","45-49","50-54", "55-59","60-64", 
     "65-69","70-74","75-79","80-200"),include.lowest = T)))

vars_age=c("age_50a_cat","age_60a_cat","age_5ygroup_cat")
high_risk_vars_age_rc_overall <- gen_sum(high_risk_vars_age_rc, var=vars_age)
high_risk_vars_age_rc_bydrug <- gen_sum(high_risk_vars_age_rc, var=vars_age, by_var = "first_covid_treat_interve")
len_c<-dim(as_tibble(high_risk_vars_age_rc_bydrug))[1]
high_risk_vars_age_rc_table <- cbind((as_tibble(high_risk_vars_age_rc_overall)[1:len_c,]),(as_tibble(high_risk_vars_age_rc_bydrug)))

# Save dataset(s) ----
write.csv(high_risk_vars_age_rc, here::here("output", "data", "high_risk_vars_age_rc.csv"), row.names = FALSE)
write.csv(high_risk_vars_age_rc_table, here::here("output", "tables", "high_risk_vars_age_rc_table.csv"), row.names = FALSE)
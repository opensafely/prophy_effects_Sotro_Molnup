## Import libraries
library('tidyverse')
library('here')
library('lubridate')
library('arrow')
library('dplyr')
library('readr')
library('fs')

## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)

## Read in data 

#sel_data <- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/dataset_table.csv.gz") %>% #,
sel_data <- read_csv(here::here("output", "data", "dataset_table.csv.gz")) %>%
  select(patient_id,age_treated, sex, age_treated_group, ethnicity_snome,ethnicity_snome_cat, had_first_covid_treat, first_covid_treat_interve,first_covid_treat_date, first_covid_treat_status,
  first_molnupiravir_date, first_sotrovimab_date, date_of_first_admis_af_treat, covid_first_admi_af_treat_alldiag_firstdate, apcs_admis_60daf_treat_alldiag_firstdate, ccare_covid_first_af_treat_alldiag_date,
  hosp_covid_date,  hosp_covid_classfic, hosp_covid_pdiag, had_ccare_covid, ccare_covid_date, hosp_allcause_date,
  hosp_allcause_classfic, hosp_allcause_pdiag,  hospitalise_disc_covid, hospitalise_disc_allcause, ons_dead_date,  underly_deathcause,
  death_cause_covid, ons_dead_tr60d_6mon_covid_treat,ons_dead_tr60d_6mon_covid_treat2, ons_dead_trstart_60d_covid_treat,  ons_dead_trstart_30d_covid_treat
  ) 

dim(sel_data)
str(sel_data,list.len= ncol(sel_data),give.attr = F)

freq_single(sel_data$had_first_covid_treat)
freq_single(sel_data$first_covid_treat_interve)
#cat("# hosp_covid_date_count#\n")
sum(!is.na(sel_data$hosp_covid_date))
sum(!is.na(sel_data$hosp_allcause_date))
sum(!is.na(sel_data$ons_dead_date))
freq_single(sel_data$underly_deathcause)
freq_single(sel_data$death_cause_covid)
freq_single(sel_data$ons_dead_tr60d_6mon_covid_treat)
freq_single(sel_data$ons_dead_tr60d_6mon_covid_treat2)
freq_single(sel_data$ons_dead_trstart_60d_covid_treat)
freq_single(sel_data$ons_dead_trstart_30d_covid_treat)

# Save dataset(s) ----
write.csv(sel_data, here::here("output", "tables", "data4analyse.csv"), row.names = FALSE)
write_rds(sel_data, here::here("output", "data", "data4analyse.rds"), compress = "gz")

# write.csv(sel_data, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/cohort_data4analyse.csv"), row.names = FALSE)
# write_rds(sel_data, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/sel_data.rds"), compress = "gz")
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

#sel_data0 <- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/dataset_table.csv.gz") %>% #,
sel_data0 <- read_csv(here::here("output", "data", "dataset_table.csv.gz")) %>%
  select(patient_id,age_treated, sex, age_treated_group, ethnicity_snome,ethnicity_snome_cat,ethnicity_combined, if_old_covid_treat,old_covid_treat, had_first_covid_treat, first_covid_treat_interve,first_covid_treat_date, first_covid_treat_status,
  first_molnupiravir_date, first_sotrovimab_date, date_of_first_admis_af_treat, covid_first_admi_af_treat_alldiag_firstdate, apcs_admis_60daf_treat_alldiag_firstdate, ccare_covid_first_af_treat_alldiag_date,
  hosp_covid_date,  hosp_covid_classfic, hosp_covid_pdiag, had_ccare_covid, ccare_covid_date, hosp_allcause_date,
  hosp_allcause_classfic, hosp_allcause_pdiag, hospitalise_disc_covid, hospitalise_disc_allcause, ons_dead_date, underly_deathcause,
  death_cause_covid, allcause_death_60d_6m,covid_death_60d_6m, allcause_death_under60d, allcause_death_under30d, bmi, is_censored, censored,
  had_dialysis, had_kidney_transplant, transplant_thymus_opcs4,transplant_thymus_opcs4_count,transplant_thymus_opcs4_a, transplant_thymus_opcs4_2,
  transplant_conjunctiva_y_code_opcs4, transplant_conjunctiva_y_code_opcs4_count,transplant_conjunctiva_opcs4, transplant_conjunctiva_opcs4_count,
  transplant_conjunctiva_opcs4_a,transplant_conjunctiva_opcs4_2) 

cat("#total\n") #old_covid_treat
dim(sel_data0)

cat("#is_censored\n")
freq_single(sel_data0$is_censored)

cat("#censored\n")
freq_single(sel_data0$censored)

cat("# if_old_covid_treat")
freq_single(sel_data0$if_old_covid_treat)
sel_data <- sel_data0 %>% filter(censored== 0 ) %>% filter(old_covid_treat== 0 )

cat("#total\n")
dim(sel_data)

cat("#is_censored\n")
freq_single(sel_data$is_censored)

cat("# str-START#\n")
str(sel_data,list.len= ncol(sel_data),give.attr = F)
cat("# str-END#\n")

cat("# if_old_covid_treat")
freq_single(sel_data$if_old_covid_treat)

cat("#had_first_covid_treat-non_hospitalised,2021-12-16--2022-02-10:")
freq_single(sel_data$had_first_covid_treat)

cat("#interventions: Molnupiravir/Sotrovimab")
freq_single(sel_data$first_covid_treat_interve)

cat("# hosp_covid_date_count#\n")
sum(!is.na(sel_data$hosp_covid_date))

cat("# hosp_allcause_date_count#\n")
sum(!is.na(sel_data$hosp_allcause_date))

cat("# ons_dead_count#\n")
sum(!is.na(sel_data$ons_dead_date))

cat("#death_cause_covid:")
freq_single(sel_data$death_cause_covid)

cat("all-cause death 60d-6m:")
freq_single(sel_data$allcause_death_60d_6m)

cat("covid_death_60d_6m:")
freq_single(sel_data$covid_death_60d_6m)

cat("allcause_death_under60d")
freq_single(sel_data$allcause_death_under60d)

cat("allcause_death_under30d")
freq_single(sel_data$allcause_death_under30d)

cat("#age-summary:")
summary(as.numeric(sel_data$age_treated),na.rm=T)

cat("#age-group:")
freq_single(sel_data$age_treated_group)

cat("#sex:")
freq_single(sel_data$sex)

cat("#ethnicity_combined:")
freq_single(sel_data$ethnicity_combined)

cat("#ethnicity_snome:")
freq_single(sel_data$ethnicity_snome)

cat("#ethnicity_snome_cat:")
freq_single(as.character(sel_data$ethnicity_snome_cat))

cat("#ethnicity_ctv3:")
freq_single(as.character(sel_data$ethnicity_ctv3))

cat("#bmi")
summary(sel_data$bmi)

cat("had_dialysis")
freq_single(sel_data$had_dialysis)

cat("had_kidney_transplant")
freq_single(sel_data$had_kidney_transplant)

cat("transplant_thymus_opcs4")
sum(!is.na(sel_data$transplant_thymus_opcs4))

cat("transplant_thymus_opcs4_count")
freq_single(as.factor(sel_data$transplant_thymus_opcs4_count))
summary(as.numeric(sel_data$transplant_thymus_opcs4_count))

cat("compare_transplant_thymus_opcs4")

identical(sel_data$transplant_thymus_opcs4_a, sel_data$transplant_thymus_opcs4_2)
###
cat("transplant_conjunctiva_y_code_opcs4")
sum(!is.na(sel_data$transplant_conjunctiva_y_code_opcs4))

cat("transplant_conjunctiva_y_code_opcs4_count")
freq_single(as.factor(sel_data$transplant_conjunctiva_y_code_opcs4_count))
summary(as.numeric(sel_data$transplant_conjunctiva_y_code_opcs4_count))


####
cat("transplant_conjunctiva_opcs4")
sum(!is.na(sel_data$transplant_conjunctiva_opcs4))

cat("transplant_conjunctiva_opcs4_count")
freq_single(as.factor(sel_data$transplant_conjunctiva_opcs4_count))
summary(as.numeric(sel_data$transplant_conjunctiva_opcs4_count))

cat("compare_transplant_conjunctiva_opcs4")
identical(sel_data$transplant_conjunctiva_opcs4_a, sel_data$transplant_conjunctiva_opcs4_2)

# Save dataset(s) ----
write.csv(sel_data, here::here("output", "tables", "data4analyse.csv"), row.names = FALSE)
write_rds(sel_data, here::here("output", "data", "data4analyse.rds"), compress = "gz")

# write.csv(sel_data, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/cohort_data4analyse.csv"), row.names = FALSE)
# write_rds(sel_data, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/sel_data.rds"), compress = "gz")

  # had_dialysis, had_kidney_transplant, transplant_thymus_opcs4,transplant_thymus_opcs4_count,transplant_thymus_opcs4_a, transplant_thymus_opcs4_2,
  # transplant_conjunctiva_y_code_opcs4, transplant_conjunctiva_y_code_opcs4_count,transplant_conjunctiva_y_code_opcs4_df,
  # transplant_conjunctiva_opcs4, transplant_conjunctiva_opcs4_count, transplant_conjunctiva_opcs4_a,transplant_conjunctiva_opcs4_2) 
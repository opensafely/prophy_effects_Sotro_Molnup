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

#df_vars0 <- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/dataset_table.csv.gz") %>% #,
df_vars0 <- read_csv(here::here("output", "data", "dataset_table.csv.gz")) %>%
  select(patient_id,age_treated, sex, age_treated_group, ethnicity_snome,ethnicity_snome_cat,ethnicity, imd1, imd, stp, if_old_covid_treat,old_covid_treat, had_first_covid_treat, first_covid_treat_interve,first_covid_treat_status,
  treat_date, first_molnupiravir_date, first_sotrovimab_date, date_of_first_admis_af_treat, covid_first_admi_af_treat_alldiag_firstdate, apcs_admis_60daf_treat_alldiag_firstdate, ccare_covid_first_af_treat_alldiag_date,
  hosp_covid_date,  hosp_covid_classfic, hosp_covid_pdiag, had_ccare_covid, ccare_covid_date, hosp_allcause_date,
  hosp_allcause_classfic, hosp_allcause_pdiag, hospitalise_disc_covid, hospitalise_disc_allcause, ons_dead_date, underly_deathcause,
  death_cause_covid, allcause_death_60d_6m,covid_death_60d_6m, allcause_death_under60d, allcause_death_under30d, bmi, is_censored, censored,
  had_dialysis, had_kidney_transplant, transplant_thymus_opcs4,transplant_thymus_opcs4_count,transplant_thymus_opcs4_a, transplant_thymus_opcs4_2,
  transplant_conjunctiva_y_code_opcs4, transplant_conjunctiva_y_code_opcs4_count,transplant_conjunctiva_opcs4, transplant_conjunctiva_opcs4_count,
  transplant_conjunctiva_opcs4_a,transplant_conjunctiva_opcs4_2,high_risk_MOL_last,high_risk_SOT02_last,
  oral_steroid_drugs_nhsd, oral_steroid_drugs_nhsd_check, oral_steroid_drug_nhsd_3m_count,oral_steroid_drug_nhsd_12m_count, 
  immunosuppresant_drugs_nhsd,  immunosuppresant_drugs_nhsd_ever, oral_steroid_drugs_nhsd_ever,
  is_codelist_highrisk, highrisk_codelist,is_codelist_highrisk_ever,highrisk_codelist_ever) 

cat("#oral_steroid_drugs_nhsd_check\n") #
freq_single(df_vars0$oral_steroid_drugs_nhsd_check)

df_vars0$highrisk_therap_MOL <- grepl("IMID|solid organ recipients|haematologic malignancy|Patients with a haematological diseases \\(sic\\)|sickle cell disease|stem cell transplant recipient|immune deficiencies|primary immune deficiencies|solid cancer", df_vars0$high_risk_MOL_last, ignore.case = TRUE)
df_vars0$highrisk_therap_SOT02 <- grepl("IMID|solid organ recipients|haematologic malignancy|Patients with a haematological diseases \\(sic\\)|sickle cell disease|stem cell transplant recipient|immune deficiencies|primary immune deficiencies|solid cancer", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)

df_vars0$had_highrisk_therap<-(df_vars0$highrisk_therap_MOL|df_vars0$highrisk_therap_SOT02)
df_vars0<-df_vars0 %>% mutate(highrisk_therap = as.integer(had_highrisk_therap))

#is_highrisk,highrisk,is_highrisk_ever,highrisk_ever
df_vars0$is_highrisk = (df_vars0$had_highrisk_therap|df_vars0$is_codelist_highrisk)
df_vars0$is_highrisk_ever = (df_vars0$had_highrisk_therap|df_vars0$is_codelist_highrisk_ever)

df_vars0$highrisk = as.integer(df_vars0$is_highrisk)
df_vars0$highrisk_ever = as.integer(df_vars0$is_highrisk_ever)

##IMID #Immune Mediated Inflammatory Diseases
df_vars0$high_risk_therap_IMID = as.integer((grepl("IMID", df_vars0$high_risk_MOL_last, ignore.case = TRUE))|(grepl("IMID", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)))
freq_single(df_vars0$high_risk_therap_IMID)

#solid organ recipients-SOR,
df_vars0$high_risk_therap_SOR = as.integer((grepl("solid organ recipients", df_vars0$high_risk_MOL_last, ignore.case = TRUE))|(grepl("solid organ recipients", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)))

#Haematologic malignancy
df_vars0$high_risk_therap_HMAL = as.integer((grepl("haematologic malignancy", df_vars0$high_risk_MOL_last, ignore.case = TRUE))|(grepl("haematologic malignancy", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)))

#patients with a haematological diseases (sic)
df_vars0$high_risk_therap_HMDs = as.integer((grepl("Patients with a haematological diseases \\(sic\\)", df_vars0$high_risk_MOL_last, ignore.case = TRUE))|(grepl("Patients with a haematological diseases \\(sic\\)", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)))

#sickle cell disease-SCD
df_vars0$high_risk_therap_SCD = as.integer((grepl("sickle cell disease", df_vars0$high_risk_MOL_last, ignore.case = TRUE))|(grepl("sickle cell disease", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)))

#stem cell transplant recipient -SCTR
df_vars0$high_risk_therap_SCTR= as.integer((grepl("stem cell transplant recipient", df_vars0$high_risk_MOL_last, ignore.case = TRUE))|(grepl("stem cell transplant recipient", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)))

#immune deficiencies
df_vars0$high_risk_therap_IMDs = as.integer((grepl("immune deficiencies", df_vars0$high_risk_MOL_last, ignore.case = TRUE))|(grepl("immune deficiencies", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)))

#primary immune deficiencies
df_vars0$high_risk_therap_solid_cancer= as.integer((grepl("primary immune deficiencies", df_vars0$high_risk_MOL_last, ignore.case = TRUE))|(grepl("primary immune deficiencies", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)))

# cancer
df_vars0$high_risk_therap_solid_cancer= as.integer((grepl("solid cancer", df_vars0$high_risk_MOL_last, ignore.case = TRUE))|(grepl("solid cancer", df_vars0$high_risk_SOT02_last, ignore.case = TRUE)))

cat("#total-df_vars0\n") #old_covid_treat
dim(df_vars0)

cat("#is_censored\n")
freq_single(df_vars0$is_censored)

cat("#censored\n")
freq_single(df_vars0$censored)

cat("#imd1-missing\n")
sum(is.na(df_vars0$imd1))

cat("#stp-missing\n")
sum(is.na(df_vars0$stp))

cat("# if_old_covid_treat")
freq_single(df_vars0$if_old_covid_treat)
df_vars <- df_vars0 %>% filter(censored== 0 ) %>% filter(old_covid_treat== 0 ) %>% filter(!is.na(imd1)) %>% filter(!is.na(stp))

cat("#total-dim(df_vars)\n")
dim(df_vars)

cat("#is_censored\n")
freq_single(df_vars$is_censored)

cat("# str-START#\n")
str(df_vars,list.len= ncol(df_vars),give.attr = F)
cat("# str-END#\n")

cat("# if_old_covid_treat")
freq_single(df_vars$if_old_covid_treat)

cat("#had_first_covid_treat-non_hospitalised,2021-12-16--2022-02-10:")
freq_single(df_vars$had_first_covid_treat)

cat("#interventions: Molnupiravir/Sotrovimab")
freq_single(df_vars$first_covid_treat_interve)

cat("# hosp_covid_date_count#\n")
sum(!is.na(df_vars$hosp_covid_date))

cat("# hosp_allcause_date_count#\n")
sum(!is.na(df_vars$hosp_allcause_date))

cat("# ons_dead_count#\n")
sum(!is.na(df_vars$ons_dead_date))

cat("#death_cause_covid:\n")
freq_single(df_vars$death_cause_covid)

cat("all-cause death 60d-6m:\n")
freq_single(df_vars$allcause_death_60d_6m)

cat("covid_death_60d_6m:")
freq_single(df_vars$covid_death_60d_6m)

cat("allcause_death_under60d")
freq_single(df_vars$allcause_death_under60d)

cat("allcause_death_under30d")
freq_single(df_vars$allcause_death_under30d)

cat("#age-summary:")
summary(as.numeric(df_vars$age_treated),na.rm=T)

cat("#age-group:")
freq_single(df_vars$age_treated_group)

cat("#sex:")
freq_single(df_vars$sex)

cat("#imd:")
freq_single(df_vars$imd)

cat("#ethnicity_combined:")
freq_single(df_vars$ethnicity)

cat("#ethnicity_snome:")
freq_single(df_vars$ethnicity_snome)

cat("#ethnicity_snome_cat:")
freq_single(as.character(df_vars$ethnicity_snome_cat))

cat("#ethnicity_ctv3:")
freq_single(as.character(df_vars$ethnicity_ctv3))

cat("#bmi")
summary(df_vars$bmi)

cat("had_dialysis")
freq_single(df_vars$had_dialysis)

cat("had_kidney_transplant")
freq_single(df_vars$had_kidney_transplant)

cat("transplant_thymus_opcs4")
sum(!is.na(df_vars$transplant_thymus_opcs4))

cat("transplant_thymus_opcs4_count")
freq_single(as.factor(df_vars$transplant_thymus_opcs4_count))
summary(as.numeric(df_vars$transplant_thymus_opcs4_count))

cat("compare_transplant_thymus_opcs4")
identical(df_vars$transplant_thymus_opcs4_a, df_vars$transplant_thymus_opcs4_2)

cat("transplant_conjunctiva_y_code_opcs4")
sum(!is.na(df_vars$transplant_conjunctiva_y_code_opcs4))

cat("transplant_conjunctiva_y_code_opcs4_count")
freq_single(as.factor(df_vars$transplant_conjunctiva_y_code_opcs4_count))
summary(as.numeric(df_vars$transplant_conjunctiva_y_code_opcs4_count))

cat("transplant_conjunctiva_opcs4")
sum(!is.na(df_vars$transplant_conjunctiva_opcs4))

cat("transplant_conjunctiva_opcs4_count")
freq_single(as.factor(df_vars$transplant_conjunctiva_opcs4_count))
summary(as.numeric(df_vars$transplant_conjunctiva_opcs4_count))

cat("compare_transplant_conjunctiva_opcs4")
identical(df_vars$transplant_conjunctiva_opcs4_a, df_vars$transplant_conjunctiva_opcs4_2)

high_risk_cohort <- df_vars %>% filter(highrisk== 1 ) 
high_risk_ever_cohort <- df_vars %>% filter(highrisk_ever== 1 ) 

cat("dim(high_risk_cohort)\n" )
dim(high_risk_cohort)

cat("dim(high_risk_ever_cohort)\n")
dim(high_risk_ever_cohort)

cat("# hosp_covid_date_count -high_risk_cohort#\n")
sum(!is.na(high_risk_cohort$hosp_covid_date))

cat("# hosp_allcause_date_count-high_risk_cohort#\n")
sum(!is.na(high_risk_cohort$hosp_allcause_date))

cat("# ons_dead_count-high_risk_cohort#\n")
sum(!is.na(high_risk_cohort$ons_dead_date))

cat("#death_cause_covid:-high_risk_cohort\n")
freq_single(high_risk_cohort$death_cause_covid)

cat("all-cause death 60d-6m:-high_risk_cohort\n")
freq_single(high_risk_cohort$allcause_death_60d_6m)

cat("covid_death_60d_6m:-high_risk_cohort")
freq_single(high_risk_cohort$covid_death_60d_6m)

cat("allcause_death_under60d-high_risk_cohort")
freq_single(high_risk_cohort$allcause_death_under60d)

cat("allcause_death_under30d-high_risk_cohort")
freq_single(high_risk_cohort$allcause_death_under30d)
# Save dataset(s) ----
write.csv(df_vars, here::here("output", "tables", "data4analyse.csv"), row.names = FALSE)
write_rds(df_vars, here::here("output", "data", "data4analyse.rds"), compress = "gz")
write_rds(high_risk_cohort, here::here("output", "data", "high_risk_cohort.rds"), compress = "gz")
write_rds(high_risk_ever_cohort, here::here("output", "data", "high_risk_ever_cohort.rds"), compress = "gz")
#write.csv(df_vars, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/cohort_data4analyse.csv"), row.names = FALSE)
# write_rds(df_vars, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/df_vars.rds"), compress = "gz")

  # had_dialysis, had_kidney_transplant, transplant_thymus_opcs4,transplant_thymus_opcs4_count,transplant_thymus_opcs4_a, transplant_thymus_opcs4_2,
  # transplant_conjunctiva_y_code_opcs4, transplant_conjunctiva_y_code_opcs4_count,
  # transplant_conjunctiva_opcs4, transplant_conjunctiva_opcs4_count, transplant_conjunctiva_opcs4_a,transplant_conjunctiva_opcs4_2) 
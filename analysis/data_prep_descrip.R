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

## Read in data ##end_date_6mon, start_date_60d
####hosp_covid60d6m_date, hosp_allcause60d6m_date(OUTCOME)
#death_covid_cause_60d6m_date, death_covid_underly_60d6m_date (OUTCOMEs)
#df_vars00<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/dataset_table.csv.gz") %>% #,
df_vars00 <- read_csv(here::here("output", "data", "dataset_table.csv.gz")) %>%
  select(patient_id,age_treated, sex, age_treated_group, ethnicity_snome,ethnicity_snome_cat,ethnicity, imd1, imd, stp,region, 
  if_old_covid_treat,old_covid_treat, had_first_covid_treat, first_covid_treat_interve,drug, first_covid_treat_status,
  end_date_6mon, start_date_60d, treat_date, first_molnupiravir_date, first_sotrovimab_date, date_of_first_admis_af_treat, hosp_af_treat_alldiag_date,
  hosp_60daf_treat_alldiag_date, ccare_covid_first_af_treat_alldiag_date, hosp_covid60d6m_date, hosp_covid60d6m_classfic,
  hosp_covid60d6m_pdiag, had_ccare_covid60d6m, ccare_covid60d6m_date, hosp_allcause60d6m_date, hosp_allcause60d6m_classfic, hosp_allcause60d6m_pdiag, 
  hospitalise_disc_covid, hospitalise_disc_allcause, ons_dead_date, underly_deathcause_code, death_cause_covid, was_allcause_death_60d_6m,
  allcause_death_60d_6m, was_covid_death_60d_6m, covid_death_60d_6m, was_allcause_death_under60d, allcause_death_under60d, 
  allcause_death_under30d, bmi, is_censored,censored, molnu_censored_date, sotro_censored_date,
  had_dialysis, had_kidney_transplant, transplant_thymus_opcs4,transplant_thymus_opcs4_count,transplant_thymus_opcs4_a, 
  transplant_thymus_opcs4_2, transplant_conjunctiva_y_code_opcs4, transplant_conjunctiva_y_code_opcs4_count,transplant_conjunctiva_opcs4,
  transplant_conjunctiva_opcs4_count, transplant_conjunctiva_opcs4_2, high_risk_MOL_last,high_risk_SOT02_last, oral_steroid_drugs_nhsd, 
  oral_steroid_drugs_nhsd_check, oral_steroid_drug_nhsd_3m_count,oral_steroid_drug_nhsd_12m_count, immunosuppresant_drugs_nhsd, 
  immunosuppresant_drugs_nhsd_ever, oral_steroid_drugs_nhsd_ever, is_codelist_highrisk, highrisk_codelist, is_codelist_highrisk_ever, 
  highrisk_codelist_ever, total_covid_vacc, total_covid_vacc_cat, covid_vacc1_date,covid_vacc2_date,covid_vacc3_date,
  covid_vacc4_date, covid_vacc_last_date) 


df_vars0<-df_vars00 %>%  
    mutate(
      underly_covid_deathcause0_1 = ifelse(underly_deathcause_code %in% c("U071", "U072", "U099", "U109"), 1, 0),
      death_cause_covid0_1 = ifelse(death_cause_covid %in% ("TRUE"), 1,0 ), #) %>% 
      test_date_format = ons_dead_date,
      death_covid_underly_60d6m_date =as.Date (ifelse(((underly_covid_deathcause0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <end_date_6mon)), as.character(ons_dead_date), NA)),
      death_covid_cause_60d6m_date = as.Date (ifelse(((death_cause_covid0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <= end_date_6mon)), as.character(ons_dead_date), NA)), 
      censored_date_molnu = as.Date (ifelse(((!is.na(molnu_censored_date)) & (molnu_censored_date >start_date_60d) & (molnu_censored_date <= end_date_6mon)), as.character(ons_dead_date), NA)),
      censored_date_sotro = as.Date (ifelse(((!is.na(sotro_censored_date)) & (sotro_censored_date >start_date_60d) & (sotro_censored_date <= end_date_6mon)), as.character(ons_dead_date), NA)),
      surv_end_covid_cause_date = as.Date(pmin(hosp_covid60d6m_date,death_covid_cause_60d6m_date, censored_date_molnu, censored_date_sotro, end_date_6mon, na.rm = TRUE)),
      surv_end_covid_underly_date = as.Date(pmin(hosp_covid60d6m_date,death_covid_underly_60d6m_date, censored_date_molnu, censored_date_sotro, end_date_6mon, na.rm = TRUE)),
      censored_bf_dead = ifelse(((censored_date_molnu == surv_end_covid_cause_date) |(censored_date_sotro == surv_end_covid_cause_date)),1,0),
      surv_days = as.numeric(difftime(surv_end_covid_cause_date, start_date_60d, units = "days")),
      surv_from_treat_days = (as.numeric(difftime(surv_end_covid_cause_date, treat_date, units = "days"))),
      surv_event = ifelse((((!is.na(death_covid_cause_60d6m_date)) &(censored_bf_dead == 0))|((!is.na(hosp_covid60d6m_date)) & (censored_bf_dead == 0))), 1,0),
      surv_event_underly = ifelse((((!is.na(death_covid_underly_60d6m_date)) & (censored_bf_dead == 0)) |((!is.na(hosp_covid60d6m_date)) & (censored_bf_dead == 0))),1,0)
     )


cat("#df_vars0$surv_event\n") 
freq_single(df_vars0$surv_event)

cat("#oral_steroid_drugs_nhsd_check\n") #
freq_single(df_vars0$oral_steroid_drugs_nhsd_check)

cat("#high_risk_MOL_last\n") 
freq_single(df_vars0$high_risk_MOL_last)

cat("#high_risk_SOT02_last\n") 
freq_single(df_vars0$high_risk_SOT02_last)

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

#df_vars0$allcause_death_under60d = as.integer(df_vars0$allcause_death_under60d)
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

cat("# df_vars0-if_old_covid_treat")
freq_single(df_vars0$if_old_covid_treat)

cat("df_vars0-allcause_death_under60d")
freq_single(df_vars0$allcause_death_under60d)

cat("df_vars0-allcause_death_under30d")
freq_single(df_vars0$allcause_death_under30d)

cat("highrisk\n")
freq_single(df_vars0$highrisk)
cat("highrisk_ever\n")
freq_single(df_vars0$highrisk_ever)

cat("#cohort-removed censored/previously treated)\n")

##cohort-#(censored== 0 ) 
df_vars <- df_vars0 %>% filter(old_covid_treat == 0 )  %>% filter(!is.na(stp))%>% filter(allcause_death_under60d != 1) #%>% filter(highrisk ==1 )

cat("#total-dim(df_vars)\n")
dim(df_vars)

cat("#str-START-df_vars#\n")
str(df_vars,list.len= ncol(df_vars),give.attr = F)
cat("#str-END#\n")

cat("#is_censored-df_vars\n")
freq_single(df_vars$is_censored)

cat("#if_old_covid_treat-df_vars")
freq_single(df_vars$if_old_covid_treat)

cat("#had_first_covid_treat-non_hospitalised,2021-12-16--2022-02-10: -df_vars")
freq_single(df_vars$had_first_covid_treat)

cat("#interventions: Molnupiravir/Sotrovimab-df_vars")
freq_single(df_vars$first_covid_treat_interve)

cat("#df_vars$surv_event-outcome\n") 
freq_single(df_vars$surv_event)

cat("#hosp_covid_date60d-6mon_count-df_vars#\n")
sum(!is.na(df_vars$hosp_covid60d6m_date))

cat("#hosp_allcause_date_count-df_vars#\n")
sum(!is.na(df_vars$hosp_allcause60d6m_date))

cat("#ons_dead_count-df_vars#\n")
sum(!is.na(df_vars$ons_dead_date))

cat("#death_cause_covid-df_vars:\n")
freq_single(df_vars$death_cause_covid)

cat("all-cause death 60d-6m-df_vars:\n")
freq_single(df_vars$allcause_death_60d_6m)

cat("covid_death_60d_6m-df_vars:")
freq_single(df_vars$covid_death_60d_6m)

cat("allcause_death_under60d-df_vars")
freq_single(df_vars$allcause_death_under60d)

cat("allcause_death_under30d-df_vars")
freq_single(df_vars$allcause_death_under30d)

cat("#age-summary-df_vars:")
summary(as.numeric(df_vars$age_treated),na.rm=T )
mean(as.numeric(df_vars$age_treated),na.rm=T )
sd(as.numeric(df_vars$age_treated),na.rm=T )
IQR(as.numeric(df_vars$age_treated), na.rm=T )

cat("#age-group-df_vars:")
freq_single(df_vars$age_treated_group)

cat("#sex-df_vars:")
freq_single(df_vars$sex)

cat("#imd-df_vars:")
freq_single(df_vars$imd)

cat("#ethnicity-df_vars:")
freq_single(df_vars$ethnicity)

cat("#ethnicity_snome-df_vars:")
freq_single(df_vars$ethnicity_snome)

cat("#ethnicity_snome_cat-df_vars:")
freq_single(as.character(df_vars$ethnicity_snome_cat))

cat("#ethnicity_ctv3-df_vars:")
freq_single(as.character(df_vars$ethnicity_ctv3))

#region
cat("#region-df_vars:")
freq_single(as.character(df_vars$region))

#bmi
cat("#bmi-df_vars")
summary(df_vars$bmi)
mean(as.numeric(df_vars$bmi),na.rm=T)
sd(as.numeric(df_vars$bmi),na.rm=T)
IQR(as.numeric(df_vars$bmi), na.rm=T)

cat("had_dialysis-df_vars")
freq_single(df_vars$had_dialysis)

cat("had_kidney_transplant-df_vars")
freq_single(df_vars$had_kidney_transplant)

cat("transplant_thymus_opcs4-df_vars")
sum(!is.na(df_vars$transplant_thymus_opcs4))

cat("transplant_thymus_opcs4_count-df_vars")
freq_single(as.factor(df_vars$transplant_thymus_opcs4_count))
summary(as.numeric(df_vars$transplant_thymus_opcs4_count))

cat("compare_transplant_thymus_opcs4-df_vars")
identical(df_vars$transplant_thymus_opcs4_a, df_vars$transplant_thymus_opcs4_2)

cat("transplant_conjunctiva_y_code_opcs4-df_vars")
sum(!is.na(df_vars$transplant_conjunctiva_y_code_opcs4))

cat("transplant_conjunctiva_y_code_opcs4_count-df_vars")
freq_single(as.factor(df_vars$transplant_conjunctiva_y_code_opcs4_count))
summary(as.numeric(df_vars$transplant_conjunctiva_y_code_opcs4_count))

cat("transplant_conjunctiva_opcs4-df_vars")
sum(!is.na(df_vars$transplant_conjunctiva_opcs4))

cat("transplant_conjunctiva_opcs4_count-df_vars")
freq_single(as.factor(df_vars$transplant_conjunctiva_opcs4_count))
summary(as.numeric(df_vars$transplant_conjunctiva_opcs4_count))

cat("compare_transplant_conjunctiva_opcs4-df_vars")
identical(df_vars$transplant_conjunctiva_opcs4_a, df_vars$transplant_conjunctiva_opcs4_2)

##high_risk_cohort)
cat("(high_risk_cohort)\n" )
high_risk_cohort <- df_vars %>% filter(highrisk== 1 ) 
high_risk_ever_cohort <- df_vars %>% filter(highrisk_ever== 1 ) 

cat("dim(high_risk_cohort)\n" )
dim(high_risk_cohort)

cat("dim(high_risk_ever_cohort)\n")
dim(high_risk_ever_cohort)

cat("#str-START-high_risk_cohort#\n")
str(high_risk_cohort,list.len= ncol(high_risk_cohort),give.attr = F)
cat("#str-END#\n")

##outcomes

cat("#high_risk_cohort$surv_event-total-outcome\n") 
freq_single(high_risk_cohort$surv_event)

cat("#hosp_covid_date60d_6mon_count-high_risk_cohort- #\n")
sum(!is.na(high_risk_cohort$hosp_covid60d6m_date))

cat("#hosp_allcause_date60d_6mon_count-high_risk_cohort-#\n")
sum(!is.na(high_risk_cohort$hosp_allcause60d6m_date))

cat("#ons_dead_count_anytime-high_risk_cohort-#\n")
sum(!is.na(high_risk_cohort$ons_dead_date))

cat("#death_cause_covid-high_risk_cohort\n")
freq_single(high_risk_cohort$death_cause_covid)

cat("all-cause death 60d-6m:-high_risk_cohort\n")
freq_single(high_risk_cohort$allcause_death_60d_6m)

cat("covid_death_60d_6m:-high_risk_cohort")
freq_single(high_risk_cohort$covid_death_60d_6m)

cat("allcause_death_under60d-high_risk_cohort")
freq_single(high_risk_cohort$allcause_death_under60d)

cat("allcause_death_under30d-high_risk_cohort")
freq_single(high_risk_cohort$allcause_death_under30d)

cat("sex_imd-table-high_risk_cohort")
dt_sex_imd<-high_risk_cohort %>%
 dplyr::count(sex,imd) %>%
 dplyr::group_by(sex) %>% 
 dplyr::mutate(prop = prop.table(n)) #%>% print(n = nrow(dt))
print(dt_sex_imd)

##first_covid_treat_interve
cat("#first_covid_treat_interve-high_risk_cohort-")
freq_single(high_risk_cohort$first_covid_treat_interve)

#total_covid_vacc_cat#
cat("#total_covid_vacc_cat-high_risk_cohort-")
freq_single(high_risk_cohort$total_covid_vacc_cat)

##cohort_molnup
cohort_molnup<-high_risk_cohort %>% filter(drug== 0 )
cat("#total-dim(cohort_molnup)\n")
dim(cohort_molnup)

# cat("#str-START-cohort_molnup#\n")
# str(cohort_molnup,list.len= ncol(cohort_molnup),give.attr = F)
# cat("#str-END#\n")

#outcomes
cat("#cohort_molnup$surv_event-total-outcome\n") 
freq_single(cohort_molnup$surv_event)

cat("#hosp_covid_date60d_6mon_count-cohort_molnup- #\n")
sum(!is.na(cohort_molnup$hosp_covid60d6m_date))

cat("#hosp_allcause_date60d_6mon_count-cohort_molnup-#\n")
sum(!is.na(cohort_molnup$hosp_allcause60d6m_date))

cat("#ons_dead_count_anytime-cohort_molnup-#\n")
sum(!is.na(cohort_molnup$ons_dead_date))

cat("#death_cause_covid-cohort_molnup\n")
freq_single(cohort_molnup$death_cause_covid)

cat("all-cause death 60d-6m:-cohort_molnup\n")
freq_single(cohort_molnup$allcause_death_60d_6m)

cat("covid_death_60d_6m:-cohort_molnup")
freq_single(cohort_molnup$covid_death_60d_6m)

cat("allcause_death_under60d-cohort_molnup")
freq_single(cohort_molnup$allcause_death_under60d)

cat("allcause_death_under30d-cohort_molnup")
freq_single(cohort_molnup$allcause_death_under30d)

cat("#age-summary-cohort_molnup:")
summary(as.numeric(cohort_molnup$age_treated),na.rm=T)
cat("#age-mean-cohort_molnup:")
mean(as.numeric(cohort_molnup$age_treated),na.rm=T)
cat("#age-sd-cohort_molnup:")
sd(as.numeric(cohort_molnup$age_treated),na.rm=T)
cat("#age-IQR-cohort_molnup:")
IQR(as.numeric(cohort_molnup$age_treated), na.rm=T)

cat("#age-group-cohort_molnup:")
freq_single(cohort_molnup$age_treated_group)

cat("#sex-cohort_molnup:")
freq_single(cohort_molnup$sex)

cat("#imd-cohort_molnup:")
freq_single(cohort_molnup$imd)

cat("#ethnicity-cohort_molnup:")
freq_single(cohort_molnup$ethnicity)

cat("#ethnicity_snome-cohort_molnup:")
freq_single(cohort_molnup$ethnicity_snome)

cat("#ethnicity_snome_cat-cohort_molnup:")
freq_single(as.character(cohort_molnup$ethnicity_snome_cat))

cat("#ethnicity_ctv3-cohort_molnup:")
freq_single(as.character(cohort_molnup$ethnicity_ctv3))

cat("#region-cohort_molnup:")
freq_single(as.character(cohort_molnup$region))

cat("#bmi-cohort_molnup")
#summary(cohort_molnup$bmi)
summary(as.numeric(cohort_molnup$bmi),na.rm=T)
mean(as.numeric(cohort_molnup$bmi),na.rm=T)
sd(as.numeric(cohort_molnup$bmi),na.rm=T)
IQR(as.numeric(cohort_molnup$bmi), na.rm=T)

cat("#total_covid_vacc_cat-cohort_molnup")
freq_single(cohort_molnup$total_covid_vacc_cat)

###cohort_sotro
cohort_sotro<-high_risk_cohort %>% filter(drug== 1 )
##outcomes
cat("#cohort_sotro$surv_event-total-outcome\n") 
freq_single(cohort_sotro$surv_event)

cat("#total-dim(cohort_sotro)\n")
dim(cohort_sotro)

cat("#str-START-cohort_sotro#\n")
str(cohort_sotro,list.len= ncol(cohort_sotro),give.attr = F)
cat("#str-END#\n")

cat("#hosp_covid_date60d_6mon_count-cohort_sotro- #\n")
sum(!is.na(cohort_sotro$hosp_covid60d6m_date))

cat("#hosp_allcause_date60d_6mon_count-cohort_sotro-#\n")
sum(!is.na(cohort_sotro$hosp_allcause60d6m_date))

cat("#ons_dead_count_anytime-cohort_sotro-#\n")
sum(!is.na(cohort_sotro$ons_dead_date))

cat("#death_cause_covid-cohort_sotro\n")
freq_single(cohort_sotro$death_cause_covid)

cat("all-cause death 60d-6m:-cohort_sotro\n")
freq_single(cohort_sotro$allcause_death_60d_6m)

cat("covid_death_60d_6m:-cohort_sotro")
freq_single(cohort_sotro$covid_death_60d_6m)

cat("allcause_death_under60d-cohort_sotro")
freq_single(cohort_sotro$allcause_death_under60d)

cat("allcause_death_under30d-cohort_sotro")
freq_single(cohort_sotro$allcause_death_under30d)

cat("#age-summary-cohort_sotro:")
summary(as.numeric(cohort_sotro$age_treated),na.rm=T)

cat("#age-mean-cohort_sotro:")
mean(as.numeric(cohort_sotro$age_treated),na.rm=T)
cat("#age-sd-cohort_sotro:")
sd(as.numeric(cohort_sotro$age_treated),na.rm=T)
cat("#age-IQR-cohort_sotro:")
IQR(as.numeric(cohort_sotro$age_treated), na.rm=T)

cat("#age-group-cohort_sotro:")
freq_single(cohort_sotro$age_treated_group)

cat("#sex-cohort_sotro:")
freq_single(cohort_sotro$sex)

cat("#imd-cohort_sotro:")
freq_single(cohort_sotro$imd)

cat("#ethnicity-cohort_sotro:")
freq_single(cohort_sotro$ethnicity)

cat("#ethnicity_snome-cohort_sotro:")
freq_single(cohort_sotro$ethnicity_snome)

cat("#ethnicity_snome_cat-cohort_sotro:")
freq_single(as.character(cohort_sotro$ethnicity_snome_cat))

cat("#ethnicity_ctv3-cohort_sotro:")
freq_single(as.character(cohort_sotro$ethnicity_ctv3))

cat("#region-cohort_sotro:")
freq_single(as.character(cohort_sotro$region))

cat("#bmi-cohort_sotro")
summary(as.numeric(cohort_sotro$bmi))
mean(as.numeric(cohort_sotro$bmi),na.rm=T)
sd(as.numeric(cohort_sotro$bmi),na.rm=T)
IQR(as.numeric(cohort_sotro$bmi), na.rm=T)

cat("#total_covid_vacc_cat-cohort_sotro")
freq_single(cohort_sotro$total_covid_vacc_cat)

# Save dataset(s) ----
write.csv(df_vars, here::here("output", "tables", "data4analyse.csv"), row.names = FALSE)
write_rds(df_vars, here::here("output", "data", "data4analyse.rds"), compress = "gz")
write_rds(high_risk_cohort, here::here("output", "data", "high_risk_cohort.rds"), compress = "gz")
write_rds(high_risk_ever_cohort, here::here("output", "data", "high_risk_ever_cohort.rds"), compress = "gz")

# write.csv(df_vars, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/cohort_data4analyse.csv"), row.names = FALSE)

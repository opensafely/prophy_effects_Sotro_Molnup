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

## import funs
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)

##hosp_covid_bf30d_date,hosp_covid_bf30d_classfic,hosp_covid_bf30d_pdiag,had_ccare_covid_bf30d,
##hosp_covid_bf60d_date,hosp_covid_bf60d_classfic,hosp_covid_bf60d_pdiag,had_ccare_covid_bf60d,
#hosp_covid60d6m_date

#df_vars00<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/dataset_table.csv.gz") %>% 
df_vars00 <- read_csv(here::here("output", "data", "dataset_table.csv.gz")) %>%
  select(patient_id,age_treated, sex, age_treated_group, ethnicity, imd1, imd, stp,region, 
  if_old_covid_treat,old_covid_treat, had_first_covid_treat, first_covid_treat_interve,drug, first_covid_treat_status,
  end_date_6mon, end_date_12mon, end_date_24mon, start_date_60d, treat_date, first_molnupiravir_date, first_sotrovimab_date, date_of_first_admis_af_treat, hosp_af_treat_alldiag_date,
  hosp_60daf_treat_alldiag_date, ccare_covid_first_af_treat_alldiag_date,hosp_covid_bf30d_date, hosp_covid_bf30d_classfic, hosp_covid_bf30d_pdiag, had_ccare_covid_bf30d,
  hosp_covid_bf60d_date, hosp_covid_bf60d_classfic, hosp_covid_bf60d_pdiag, had_ccare_covid_bf60d, 
  hosp_covid60d6m_date, hosp_covid60d6m_classfic, hosp_covid60d12m_date, had_ccare_covid60d12m, hosp_covid60d12m_classfic, hosp_covid60d24m_date, hosp_covid60d24m_classfic,had_ccare_covid60d24m,
  hosp_covid60d6m_pdiag, had_ccare_covid60d6m, ccare_covid60d6m_date, hosp_allcause60d6m_date, hosp_allcause60d6m_classfic, hosp_allcause60d6m_pdiag, 
  hospitalise_disc_covid, hospitalise_disc_allcause, ons_dead_date, underly_deathcause_code, death_cause_covid, was_allcause_death_60d_6m,
  allcause_death_60d_6m, was_covid_death_60d_6m, covid_death_60d_6m, was_allcause_death_under60d, allcause_death_under60d, allcause_death_under30d, bmi, bmi_date, 
  is_molnu_pt6m_censored, is_sotro_pt6m_censored, molnu_pt6m_censored, sotro_pt6m_censored, molnu_pt6m_censored_date, sotro_pt6m_censored_date,
  is_molnu_pt12m_censored, is_sotro_pt12m_censored, molnu_pt12m_censored, sotro_pt12m_censored, molnu_pt12m_censored_date, sotro_pt12m_censored_date,
  is_molnu_pt24m_censored, is_sotro_pt24m_censored, molnu_pt24m_censored, sotro_pt24m_censored, molnu_pt24m_censored_date, sotro_pt24m_censored_date,
  risk_cohort, had_dialysis, had_kidney_transplant, transplant_thymus_opcs4,transplant_thymus_opcs4_count,transplant_thymus_opcs4_a, 
  transplant_thymus_opcs4_2, transplant_conjunctiva_y_code_opcs4, transplant_conjunctiva_y_code_opcs4_count,transplant_conjunctiva_opcs4,
  transplant_conjunctiva_opcs4_count, transplant_conjunctiva_opcs4_2, oral_steroid_drugs_nhsd, 
  oral_steroid_drugs_nhsd_check, oral_steroid_drug_nhsd_3m_count,oral_steroid_drug_nhsd_12m_count, immunosuppresant_drugs_nhsd, 
  immunosuppresant_drugs_nhsd_ever, oral_steroid_drugs_nhsd_ever, is_codelist_highrisk, highrisk_codelist, is_codelist_highrisk_ever, 
  highrisk_codelist_ever, total_covid_vacc, total_covid_vacc_cat, covid_vacc1_date,covid_vacc2_date,covid_vacc3_date,
  covid_vacc4_date, covid_vacc_last_date, imid, imid_ever,dialysis,kidney_transplant,solid_organ_transplant_new,haema_disease,
  haema_disease_ever, immunosupression_new,solid_cancer_new,solid_cancer_ever, had_imid, had_imid_ever, had_dialysis,had_kidney_transplant,
  had_solid_organ_transplant_new, had_haema_disease, had_immunosupression_new, had_solid_cancer_new, had_solid_cancer_ever,
  had_diabetes, had_hypertension,had_chronic_cardiac_disease, had_chronic_respiratory_disease, had_autism,had_learning_disability,
  had_serious_mental_illness, had_dementia, rural_urban, had_housebound, housebound_lastdate, no_longer_housebound_lastdate, moved_into_care_home_lastdate) 

##hosp_covid_bf30d_date,had_ccare_covid_bf30d,
##hosp_covid_bf60d_date,had_ccare_covid_bf60d,
#hosp_covid60d6m_date #had_ccare_covid60d6m
##hosp_covid60d12m_date, #had_ccare_covid60d12m
#hosp_covid60d24m_date, #had_ccare_covid60d24m

df_vars01<-df_vars00 %>%  
    mutate(
    bmi_num_rc = ifelse((bmi<10|bmi>60),NA,bmi),
    bmi_rc_cat = fct_explicit_na(cut(as.numeric(bmi_num_rc), breaks=c(0,18.4999,24.999,29.999,60),
    labels=c("0underweight","1normal","2overweight", "3obese"),include.lowest = T), 
                                na_level = "unknown"),
    bmi_cat_num = ifelse(bmi_rc_cat == "0underweight", 0, 
    ifelse(bmi_rc_cat == "1normal", 1, 
    ifelse(bmi_rc_cat == "2overweight", 2,
    ifelse(bmi_rc_cat == "3obese", 3, 4)))),
    sex_num = ifelse(sex %in% ("female"), 1, 0),
    ethnicity_num = ifelse(ethnicity == "Asian or Asian British", 0, 
    ifelse(ethnicity == "Black or Black British", 1, 
    ifelse(ethnicity == "Chinese or Other Ethnic Groups", 2,
    ifelse(ethnicity == "Mixed", 3,
    ifelse(ethnicity == "unknown", 4, 5))))),
    imd_num = ifelse(imd == "1 (most deprived)", 0, 
    ifelse(imd == "2", 1, 
    ifelse(imd == "3", 2,
    ifelse(imd == "4", 3,
    ifelse(imd == "5 (least deprived)", 4,5))))),
    region_num = ifelse(region == "East", 0, 
    ifelse(region == "East Midlands", 1, 
    ifelse(region == "London", 2,
    ifelse(region == "North East", 3,
    ifelse(region == "North West", 4,
    ifelse(region == "South East",5,
    ifelse(region == "South West",6,
    ifelse(region == "West Midlands",7,8)))))))),
    covid_vacc_num = ifelse(total_covid_vacc == 1, 1,  
    ifelse(total_covid_vacc == 2, 2,
    ifelse(total_covid_vacc >= 3, 3, 0))),
    covid_vacc_num1 = ifelse(total_covid_vacc_cat == "unvaccinated", 0, 
    ifelse(total_covid_vacc_cat == "One vaccination", 1, 
    ifelse(total_covid_vacc_cat == "Two vaccination", 2, 3))),
    underly_covid_deathcause0_1 = ifelse(underly_deathcause_code %in% c("U071", "U072", "U099", "U109"), 1, 0),
    death_cause_covid0_1 = ifelse(death_cause_covid %in% ("TRUE"), 1,0 ), 
    death_covid_underly_60d6m_date =as.Date(ifelse(((underly_covid_deathcause0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <end_date_6mon)), as.character(ons_dead_date), NA)),
    death_covid_cause_60d6m_date = as.Date (ifelse(((death_cause_covid0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <= end_date_6mon)), as.character(ons_dead_date), NA)), 
    censored6m_date_molnu = as.Date(ifelse(((molnu_pt6m_censored== 1) & (molnu_pt6m_censored_date <death_covid_cause_60d6m_date))
    |((molnu_pt6m_censored== 1) & (molnu_pt6m_censored_date <hosp_covid60d6m_date)), as.character(molnu_pt6m_censored_date), NA)),
    censored6m_date_sotro= as.Date (ifelse(((sotro_pt6m_censored== 1) & (sotro_pt6m_censored_date <death_covid_cause_60d6m_date))
    |((sotro_pt6m_censored== 1) & (sotro_pt6m_censored_date <hosp_covid60d6m_date)), as.character(molnu_pt6m_censored_date), NA)),
    surv6m_end_covid_cause_date = as.Date(pmin(hosp_covid60d6m_date,death_covid_cause_60d6m_date, censored6m_date_molnu, censored6m_date_sotro, end_date_6mon, na.rm = TRUE)),
    surv6m_end_covid_underly_date = as.Date(pmin(hosp_covid60d6m_date,death_covid_underly_60d6m_date, censored6m_date_molnu, censored6m_date_sotro, end_date_6mon, na.rm = TRUE)),
    censored6m_bf_dead_hosp = ifelse(((!is.na(censored6m_date_molnu) & (censored6m_date_molnu == surv6m_end_covid_cause_date)) 
    |((!is.na(censored6m_date_sotro)) & censored6m_date_sotro == surv6m_end_covid_cause_date)),1,0),
    surv6m_days = as.numeric(difftime(surv6m_end_covid_cause_date, start_date_60d, units = "days")),
    surv6m_days_from_treat = as.numeric(difftime(surv6m_end_covid_cause_date, treat_date, units = "days")),
    surv6m_event =as.character(ifelse((((!is.na(death_covid_cause_60d6m_date)) & (censored6m_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d6m_date)) & (censored6m_bf_dead_hosp == 0))), "1yes","2no")),
    surv6m_event_underly = as.character(ifelse((((!is.na(death_covid_underly_60d6m_date)) & (censored6m_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d6m_date)) & (censored6m_bf_dead_hosp == 0))),"1yes","2no")),
    surv6m_event_num = as.numeric(ifelse(surv6m_event == "1yes", 1, 0)),
    surv6m_event_underly_num = as.numeric(ifelse(surv6m_event_underly == "1yes", 1, 0)),
    calendar_day = as.numeric(difftime(start_date_60d, as.Date("2021-12-15"),units = "days")),
    calendar_wk = as.numeric(difftime(start_date_60d, as.Date("2021-12-15"),units = "weeks")),
    age_treat_gp_rc = ifelse(age_treated_group=="90+","80-89",age_treated_group),
    had_hosp_covid60d6m = ifelse(!is.na(hosp_covid60d6m_date),"1yes","2no"),
    had_hosp_covid60d6m_01 = ifelse(!is.na(hosp_covid60d6m_date),1,0),
    had_hosp_allcause60d6m = ifelse(!is.na(hosp_allcause60d6m_date),"1yes","2no"),
    had__hosp_allcause60d6m_01 = ifelse(!is.na(hosp_allcause60d6m_date),1,0),
    had_ons_dead_date = ifelse(!is.na(ons_dead_date),"1yes","2no"),
    had__ons_dead_date_01 = ifelse(!is.na(ons_dead_date),1,0),
    imd = as.character(imd),
    had_housebound_r_num = ifelse(((housebound_lastdate > no_longer_housebound_lastdate) & (housebound_lastdate > moved_into_care_home_lastdate)),1,0),
    vacc_last_treat_days = as.numeric(difftime(covid_vacc_last_date, treat_date, units = "days")),
    had_hosp_covid_bf30d_date = as.character(ifelse(!is.na(hosp_covid_bf30d_date),"1yes","2no")),
    had_hosp_covid_bf60d_date = as.character(ifelse(!is.na(hosp_covid_bf60d_date),"1yes","2no")),
    had_hosp_covid60d6m_date = as.character(ifelse(!is.na(hosp_covid60d6m_date),"1yes","2no")),
    had_hosp_covid60d12m_date = as.character(ifelse(!is.na(hosp_covid60d12m_date),"1yes","2no")),
    had_hosp_covid60d24m_date = as.character(ifelse(!is.na(hosp_covid60d24m_date),"1yes","2no")),

) %>%  #12month
mutate(
    death_covid_underly_60d12m_date =as.Date(ifelse(((underly_covid_deathcause0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <end_date_12mon)), as.character(ons_dead_date), NA)),
    death_covid_cause_60d12m_date = as.Date (ifelse(((death_cause_covid0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <= end_date_12mon)), as.character(ons_dead_date), NA)), 
    censored12m_date_molnu = as.Date(ifelse(((molnu_pt12m_censored== 1) & (molnu_pt12m_censored_date <death_covid_cause_60d12m_date))
    |((molnu_pt12m_censored== 1) & (molnu_pt12m_censored_date <hosp_covid60d12m_date)), as.character(molnu_pt12m_censored_date), NA)),
    censored12m_date_sotro= as.Date (ifelse(((sotro_pt12m_censored== 1) & (sotro_pt12m_censored_date <death_covid_cause_60d12m_date))
    |((sotro_pt12m_censored== 1) & (sotro_pt12m_censored_date <hosp_covid60d12m_date)), as.character(molnu_pt12m_censored_date), NA)),
    surv12m_end_covid_cause_date = as.Date(pmin(hosp_covid60d12m_date,death_covid_cause_60d12m_date, censored12m_date_molnu, censored12m_date_sotro, end_date_12mon, na.rm = TRUE)),
    surv12m_end_covid_underly_date = as.Date(pmin(hosp_covid60d12m_date,death_covid_underly_60d12m_date, censored12m_date_molnu, censored12m_date_sotro, end_date_12mon, na.rm = TRUE)),
    censored12m_bf_dead_hosp = ifelse(((!is.na(censored12m_date_molnu) & (censored12m_date_molnu == surv12m_end_covid_cause_date)) 
    |((!is.na(censored12m_date_sotro)) & censored12m_date_sotro == surv12m_end_covid_cause_date)),1,0),
    surv12m_days = as.numeric(difftime(surv12m_end_covid_cause_date, start_date_60d, units = "days")),
    surv12m_days_from_treat = as.numeric(difftime(surv12m_end_covid_cause_date, treat_date, units = "days")),
    surv12m_event =as.character(ifelse((((!is.na(death_covid_cause_60d12m_date)) & (censored12m_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d12m_date)) & (censored12m_bf_dead_hosp == 0))), "1yes","2no")),
    surv12m_event_underly = as.character(ifelse((((!is.na(death_covid_underly_60d12m_date)) & (censored12m_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d12m_date)) & (censored12m_bf_dead_hosp == 0))),"1yes","2no")),
    surv12m_event_num = as.numeric(ifelse(surv12m_event == "1yes", 1, 0)),
    surv12m_event_underly_num = as.numeric(ifelse(surv12m_event_underly == "1yes", 1, 0)),
) %>%  #24month
mutate(
    death_covid_underly_60d24m_date =as.Date(ifelse(((underly_covid_deathcause0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <end_date_24mon)), as.character(ons_dead_date), NA)),
    death_covid_cause_60d24m_date = as.Date (ifelse(((death_cause_covid0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <= end_date_24mon)), as.character(ons_dead_date), NA)), 
    censored24m_date_molnu = as.Date(ifelse(((molnu_pt24m_censored== 1) & (molnu_pt24m_censored_date <death_covid_cause_60d24m_date))
    |((molnu_pt24m_censored== 1) & (molnu_pt24m_censored_date <hosp_covid60d24m_date)), as.character(molnu_pt24m_censored_date), NA)),
    censored24m_date_sotro= as.Date (ifelse(((sotro_pt24m_censored== 1) & (sotro_pt24m_censored_date <death_covid_cause_60d24m_date))
    |((sotro_pt24m_censored== 1) & (sotro_pt24m_censored_date <hosp_covid60d24m_date)), as.character(molnu_pt24m_censored_date), NA)),
    surv24m_end_covid_cause_date = as.Date(pmin(hosp_covid60d24m_date,death_covid_cause_60d24m_date, censored24m_date_molnu, censored24m_date_sotro, end_date_24mon, na.rm = TRUE)),
    surv24m_end_covid_underly_date = as.Date(pmin(hosp_covid60d24m_date,death_covid_underly_60d24m_date, censored24m_date_molnu, censored24m_date_sotro, end_date_24mon, na.rm = TRUE)),
    censored24m_bf_dead_hosp = ifelse(((!is.na(censored24m_date_molnu) & (censored24m_date_molnu == surv24m_end_covid_cause_date)) 
    |((!is.na(censored24m_date_sotro)) & censored24m_date_sotro == surv24m_end_covid_cause_date)),1,0),
    surv24m_days = as.numeric(difftime(surv24m_end_covid_cause_date, start_date_60d, units = "days")),
    surv24m_days_from_treat = as.numeric(difftime(surv24m_end_covid_cause_date, treat_date, units = "days")),
    surv24m_event =as.character(ifelse((((!is.na(death_covid_cause_60d24m_date)) & (censored24m_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d24m_date)) & (censored24m_bf_dead_hosp == 0))), "1yes","2no")),
    surv24m_event_underly = as.character(ifelse((((!is.na(death_covid_underly_60d24m_date)) & (censored24m_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d24m_date)) & (censored24m_bf_dead_hosp == 0))),"1yes","2no")),
    surv24m_event_num = as.numeric(ifelse(surv24m_event == "1yes", 1, 0)),
    surv24m_event_underly_num = as.numeric(ifelse(surv24m_event_underly == "1yes", 1, 0)) 
)

df_vars0 <- df_vars01 %>%
  mutate(had_highrisk_therap = grepl("IMID|solid organ recipients|haematologic malignancy|Patients with a haematological diseases \\(sic\\)|sickle cell disease|stem cell transplant recipient|immune deficiencies|primary immune deficiencies|solid cancer", risk_cohort, ignore.case = TRUE),
  highrisk_therap = as.integer(had_highrisk_therap),
  is_highrisk=((highrisk_therap == 1)|(is_codelist_highrisk == 1)),
  is_highrisk_ever = ((highrisk_therap == 1)|(is_codelist_highrisk_ever == 1)),
  highrisk = as.integer(is_highrisk),
  highrisk_ever = as.integer(is_highrisk_ever),
  therap_IMID = as.integer((grepl("IMID", risk_cohort, ignore.case = TRUE))), #IMID #Immune Mediated Inflammatory Diseases
  therap_SOR = as.integer((grepl("solid organ recipients",risk_cohort, ignore.case = TRUE))), #solid organ recipients-SOR,
  therap_HMAL = as.integer((grepl("haematologic malignancy", risk_cohort, ignore.case = TRUE))), #Haematologic malignancy
  therap_HMDs = as.integer((grepl("Patients with a haematological diseases \\(sic\\)", risk_cohort, ignore.case = TRUE))), #patients with a haematological diseases (sic)
  therap_SCD = as.integer((grepl("sickle cell disease", risk_cohort, ignore.case = TRUE))), #sickle cell disease-SCD
  therap_SCTR = as.integer((grepl("stem cell transplant recipient", risk_cohort, ignore.case = TRUE))), #stem cell transplant recipient -SCTR
  therap_IMDs = as.integer((grepl("immune deficiencies", risk_cohort, ignore.case = TRUE))), #immune deficiencies
  therap_PID = as.integer((grepl("primary immune deficiencies", risk_cohort, ignore.case = TRUE))), #primary immune deficiencies (PID)
  therap_solid_cancer = as.integer((grepl("solid cancer", risk_cohort, ignore.case = TRUE)))# solid_cancer
) %>% 
  mutate(imid_therap_code = ifelse(rowSums(cbind(imid,therap_IMID),na.rm = TRUE)>= 1, 1, 0),
  dialysis_therap_code = dialysis, kidney_transplant_therap_code = kidney_transplant,
  solid_organ_transplant_therap_code = ifelse(rowSums(cbind(solid_organ_transplant_new,therap_SOR),na.rm = TRUE)>= 1, 1, 0),
  haema_disease_therap_code = ifelse(rowSums(cbind(haema_disease,therap_HMDs,therap_HMAL,therap_SCD,therap_SCTR),na.rm = TRUE)>= 1, 1, 0),
  immunosupression_therap_code =ifelse(rowSums(cbind(immunosupression_new,therap_IMDs,therap_PID),na.rm = TRUE)>= 1, 1, 0),
  solid_cancer_therap_code = ifelse(rowSums(cbind(solid_cancer_new,therap_solid_cancer),na.rm = TRUE)>= 1, 1, 0),
  imid_ever_therap_code = ifelse(rowSums(cbind(imid_ever,therap_IMID),na.rm = TRUE)>= 1, 1, 0),
  solid_cancer_ever_therap_code = ifelse(rowSums(cbind(solid_cancer_ever,therap_solid_cancer),na.rm = TRUE)>= 1, 1, 0),
  high_risk_group = ifelse(imid_therap_code==1, "imid_therap_code",
                           ifelse(dialysis_therap_code==1, "dialysis_therap_code",
                           ifelse(kidney_transplant_therap_code==1, "kidney_transplant_code",
                           ifelse(solid_organ_transplant_therap_code==1, "solid_organ_transplant_therap_code",
                           ifelse(haema_disease_therap_code==1, "haema_disease_therap_code",
                           ifelse(immunosupression_therap_code==1, "immunosupression_therap_code",
                           ifelse(solid_cancer_therap_code==1, "solid_cancer_therap_code", NA))))))),
  high_risk_num = ifelse(high_risk_group == "imid_therap_code", 0, 
    ifelse(high_risk_group == "dialysis_therap_code", 1,
    ifelse(high_risk_group == "kidney_transplant_code", 2,  
    ifelse(high_risk_group == "solid_organ_transplant_therap_code", 3,
    ifelse(high_risk_group == "haema_disease_therap_code", 4,
    ifelse(high_risk_group == "immunosupression_therap_code", 5,6)))))),
  diabetes = as.integer(had_diabetes), #as.factor()
  hypertension = as.integer(had_hypertension),
  chronic_cardiac_disease = as.integer(had_chronic_cardiac_disease),
  chronic_respiratory_disease = as.integer(had_chronic_respiratory_disease),
  autism = as.integer(had_autism),
  learning_disability = as.integer(had_learning_disability),
  serious_mental_illness = as.integer(had_serious_mental_illness),
  dementia = as.integer(had_dementia),
  housebound = as.integer(had_housebound),                                                                                                   
)

cat("#freq_single-df_vars0$allcause_death_under60d\n") 
freq_single(df_vars0$allcause_death_under60d)

df_vars <- df_vars0 %>% filter(old_covid_treat == 0 )  %>% filter(!is.na(stp))%>% filter(allcause_death_under60d != 1) 
high_risk_cohort <- df_vars %>% filter(highrisk == 1 ) 
high_risk_ever_cohort <- df_vars %>% filter(highrisk_ever == 1 ) 

high_risk_cohort_inc60ddeath <- df_vars0 %>% filter(old_covid_treat == 0 ) %>% filter(!is.na(stp)) %>% filter(highrisk == 1) %>% ###mutate
   mutate(surv6m_event_num = ifelse(allcause_death_under60d == 1, 0,surv6m_event_num),
    surv12m_event_num = ifelse(allcause_death_under60d == 1, 0,surv12m_event_num), 
    surv24m_event_num = ifelse(allcause_death_under60d == 1, 0,surv24m_event_num)
)

cohort_molnup<-high_risk_cohort %>% filter(drug == 0 )
cohort_sotro<-high_risk_cohort %>% filter(drug == 1 )


cat("#summary(high_risk_cohort$calendar_day)\n") 
summary(high_risk_cohort$calendar_day)

cat("#summary(high_risk_cohort_inc60ddeath$first_covid_treat_interve)\n") 
freq_single(high_risk_cohort_inc60ddeath$first_covid_treat_interve)

## Clinical and demographics table
vars <- c("age_treat_gp_rc", "sex","surv6m_event", "surv12m_event", "surv24m_event","ethnicity", "region", 
"total_covid_vacc_cat", "bmi_rc_cat","bmi_cat_num","imd", "high_risk_group","diabetes","hypertension","chronic_cardiac_disease",
"chronic_respiratory_disease","autism","learning_disability","serious_mental_illness","dementia","first_covid_treat_interve")

vars2 <- c("age_treated","age_treat_gp_rc", "sex","surv6m_event", "surv12m_event", "surv24m_event","ethnicity", "region", 
"total_covid_vacc_cat", "bmi_rc_cat","bmi_cat_num","imd", "high_risk_group","diabetes","hypertension","chronic_cardiac_disease",
"chronic_respiratory_disease","autism","learning_disability","serious_mental_illness","dementia","first_covid_treat_interve")

vars3 <- c("had_hosp_covid_bf30d_date","had_hosp_covid_bf60d_date", "had_hosp_covid60d6m_date","had_hosp_covid60d12m_date", "had_hosp_covid60d24m_date", 
"had_ccare_covid_bf30d", "had_ccare_covid_bf60d","had_ccare_covid60d6m","had_ccare_covid60d12m", "had_ccare_covid60d24m","allcause_death_under60d", "death_cause_covid0_1", "first_covid_treat_interve")


high_risk_cohort_data <- high_risk_cohort %>%
  select(all_of(vars)) %>%
    mutate(
    across(where(is.character), as.factor)
) 

high_risk_cohort_tb1_overall <- proc_rm_b8(high_risk_cohort_data,var=vars)
high_risk_cohort_tb1_bydrug <- proc_rm_b8_str(high_risk_cohort_data, "first_covid_treat_interve")
len_a<-dim(as_tibble(high_risk_cohort_tb1_bydrug))[1]
high_risk_cohort_tb1 <- cbind((as_tibble(high_risk_cohort_tb1_overall)[1:len_a,]),(as_tibble(high_risk_cohort_tb1_bydrug)))


high_risk_cohort_sum_bydrug <- gen_sum_num(high_risk_cohort, var=vars, by_var = "first_covid_treat_interve")
high_risk_cohort_sum <-as_tibble(high_risk_cohort_sum_bydrug) %>% 
  mutate(across(tail(names(.), 2), ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)
)

high_risk_cohort_sum_rd_m10 <- as_tibble(high_risk_cohort_sum_bydrug) %>% 
  mutate(across(tail(names(.), 2), ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(across(where(is.numeric), roundmid_any10)) %>%
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)
)

#############
high_risk_cohort_sum_bydrug_added <- gen_sum_num(high_risk_cohort, var=vars3, by_var = "first_covid_treat_interve")
high_risk_cohort_sum_added <-as_tibble(high_risk_cohort_sum_bydrug_added) %>% 
  mutate(across(tail(names(.), 2), ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)
)

high_risk_cohort_sum_rd_m10_added <- as_tibble(high_risk_cohort_sum_bydrug_added) %>% 
  mutate(across(tail(names(.), 2), ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(across(where(is.numeric), roundmid_any10)) %>%
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)
)

###################
high_risk_extend_bydrug <- gen_sum_num(high_risk_cohort_inc60ddeath, var=vars, by_var = "first_covid_treat_interve")
high_risk_extend_rd_m10 <- as_tibble(high_risk_extend_bydrug) %>% 
  mutate(across(tail(names(.), 2), ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(across(where(is.numeric), roundmid_any10)) %>%
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  )
cat("#str(high_risk_cohort_sum_rd_m10)\n") 
str(high_risk_cohort_sum_rd_m10, list.len = ncol(high_risk_cohort_sum_rd_m10), give.attr= F)

cat("#str(high_risk_cohort)\n") 
str(high_risk_cohort, list.len = ncol(high_risk_cohort), give.attr= F)

# Save dataset(s) ----
write_csv(high_risk_cohort_sum, here::here("output", "tables", "table1_sum_num.csv"))
write_csv(high_risk_cohort_sum_rd_m10, here::here("output", "tables", "table1_sum_rd_m10.csv"))
write_csv(high_risk_cohort_sum_added, here::here("output", "tables", "table1_sum_num_added.csv"))
write_csv(high_risk_cohort_sum_rd_m10_added, here::here("output", "tables", "table1_sum_rd_m10_added.csv"))

write_csv(high_risk_extend_rd_m10, here::here("output", "tables", "table2_high_risk_extend_rd_m10.csv"))
write_csv(high_risk_cohort_tb1, here::here("output", "tables", "table1_redacted_under8.csv"))
write.csv(df_vars0, here::here("output", "data", "data4analyses.csv"))
write.csv(high_risk_cohort, here::here("output", "data", "high_risk_cohort.csv"))
write.csv(high_risk_cohort_inc60ddeath, here::here("output", "data", "high_risk_cohort_inc60ddeath.csv"))
write.csv(high_risk_ever_cohort, here::here("output", "data", "high_risk_ever_cohort.csv"))

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

#df_vars00<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/dataset_table.csv.gz") %>% #,
df_vars00 <- read_csv(here::here("output", "data", "dataset_table.csv.gz")) %>%
  select(patient_id,age_treated, sex, age_treated_group, ethnicity, imd1, imd, stp,region, 
  if_old_covid_treat,old_covid_treat, had_first_covid_treat, first_covid_treat_interve,drug, first_covid_treat_status,
  end_date_6mon, start_date_60d, treat_date, first_molnupiravir_date, first_sotrovimab_date, date_of_first_admis_af_treat, hosp_af_treat_alldiag_date,
  hosp_60daf_treat_alldiag_date, ccare_covid_first_af_treat_alldiag_date, hosp_covid60d6m_date, hosp_covid60d6m_classfic,
  hosp_covid60d6m_pdiag, had_ccare_covid60d6m, ccare_covid60d6m_date, hosp_allcause60d6m_date, hosp_allcause60d6m_classfic, hosp_allcause60d6m_pdiag, 
  hospitalise_disc_covid, hospitalise_disc_allcause, ons_dead_date, underly_deathcause_code, death_cause_covid, was_allcause_death_60d_6m,
  allcause_death_60d_6m, was_covid_death_60d_6m, covid_death_60d_6m, was_allcause_death_under60d, allcause_death_under60d, 
  allcause_death_under30d, bmi, bmi_date, is_molnu_pt_censored,is_sotro_pt_censored,molnu_pt_censored, sotro_pt_censored, molnu_pt_censored_date, sotro_pt_censored_date,risk_cohort,
  had_dialysis, had_kidney_transplant, transplant_thymus_opcs4,transplant_thymus_opcs4_count,transplant_thymus_opcs4_a, 
  transplant_thymus_opcs4_2, transplant_conjunctiva_y_code_opcs4, transplant_conjunctiva_y_code_opcs4_count,transplant_conjunctiva_opcs4,
  transplant_conjunctiva_opcs4_count, transplant_conjunctiva_opcs4_2,  oral_steroid_drugs_nhsd, 
  oral_steroid_drugs_nhsd_check, oral_steroid_drug_nhsd_3m_count,oral_steroid_drug_nhsd_12m_count, immunosuppresant_drugs_nhsd, 
  immunosuppresant_drugs_nhsd_ever, oral_steroid_drugs_nhsd_ever, is_codelist_highrisk, highrisk_codelist, is_codelist_highrisk_ever, 
  highrisk_codelist_ever, total_covid_vacc, total_covid_vacc_cat, covid_vacc1_date,covid_vacc2_date,covid_vacc3_date,
  covid_vacc4_date, covid_vacc_last_date, imid, imid_ever,dialysis,kidney_transplant,solid_organ_transplant_new,haema_disease,
  haema_disease_ever, immunosupression_new,solid_cancer_new,solid_cancer_ever, had_imid, had_imid_ever, had_dialysis,had_kidney_transplant,
  had_solid_organ_transplant_new, had_haema_disease, had_immunosupression_new, had_solid_cancer_new, had_solid_cancer_ever,
  had_diabetes, had_hypertension,had_chronic_cardiac_disease, had_chronic_respiratory_disease, had_autism,had_learning_disability,
  had_serious_mental_illness, had_dementia, had_housebound,, housebound_lastdate, no_longer_housebound_lastdate, moved_into_care_home_lastdate) 

# > freq_single(high_risk_cohort$imd)
#                    [,1]   [,2]
# 1 (most deprived)     9  12.16
# 2                     8  10.81
# 3                     7   9.46
# 4                    11  14.86
# 5 (least deprived)    8  10.81
# unknown              31  41.89
# total                74 100.00

# > freq_single(high_risk_cohort$region)
#                          [,1]   [,2]
# East                        3   4.05
# East Midlands              12  16.22
# London                      9  12.16
# North East                  6   8.11
# North West                  9  12.16
# South East                  8  10.81
# South West                 12  16.22
# West Midlands               8  10.81
# Yorkshire and The Humber    7   9.46
# total                      74 100.00

df_vars0<-df_vars00 %>%  
    mutate(
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
    underly_covid_deathcause0_1 = ifelse(underly_deathcause_code %in% c("U071", "U072", "U099", "U109"), 1, 0),
    death_cause_covid0_1 = ifelse(death_cause_covid %in% ("TRUE"), 1,0 ), 
    test_date_format = ons_dead_date,
    death_covid_underly_60d6m_date =as.Date(ifelse(((underly_covid_deathcause0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <end_date_6mon)), as.character(ons_dead_date), NA)),
    death_covid_cause_60d6m_date = as.Date (ifelse(((death_cause_covid0_1 == 1) & (ons_dead_date >start_date_60d) & (ons_dead_date <= end_date_6mon)), as.character(ons_dead_date), NA)), 
    censored_date_molnu = as.Date(ifelse(((molnu_pt_censored== 1) & (molnu_pt_censored_date <death_covid_cause_60d6m_date))
    |((molnu_pt_censored== 1) & (molnu_pt_censored_date <hosp_covid60d6m_date)), as.character(molnu_pt_censored_date), NA)),
    censored_date_sotro= as.Date (ifelse(((sotro_pt_censored== 1) & (sotro_pt_censored_date <death_covid_cause_60d6m_date))
    |((sotro_pt_censored== 1) & (sotro_pt_censored_date <hosp_covid60d6m_date)), as.character(molnu_pt_censored_date), NA)),
    surv_end_covid_cause_date = as.Date(pmin(hosp_covid60d6m_date,death_covid_cause_60d6m_date, censored_date_molnu, censored_date_sotro, end_date_6mon, na.rm = TRUE)),
    surv_end_covid_underly_date = as.Date(pmin(hosp_covid60d6m_date,death_covid_underly_60d6m_date, censored_date_molnu, censored_date_sotro, end_date_6mon, na.rm = TRUE)),
    censored_bf_dead_hosp = ifelse(((!is.na(censored_date_molnu) & (censored_date_molnu == surv_end_covid_cause_date)) 
    |((!is.na(censored_date_sotro)) & censored_date_sotro == surv_end_covid_cause_date)),1,0),
    surv_days = as.numeric(difftime(surv_end_covid_cause_date, start_date_60d, units = "days")),
    surv_from_treat_days = as.numeric(difftime(surv_end_covid_cause_date, treat_date, units = "days")),
    surv_event =as.character(ifelse((((!is.na(death_covid_cause_60d6m_date)) & (censored_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d6m_date)) & (censored_bf_dead_hosp == 0))), "1yes","2no")),
    surv_event_underly = as.character(ifelse((((!is.na(death_covid_underly_60d6m_date)) & (censored_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d6m_date)) & (censored_bf_dead_hosp == 0))),"1yes","2no")),
    surv_event_num = as.numeric(ifelse(surv_event == "1yes", 1, 0)),
    surv_event_underly = as.numeric(ifelse(surv_event_underly == "1yes", 1, 0)),
    calendar_day = as.numeric(difftime(start_date_60d, as.Date("2021-12-16"),units = "days")),
    calendar_wk = as.numeric(difftime(start_date_60d, as.Date("2021-12-16"),units = "weeks")),
    age_treated_spline = ns(age_treated, df = 4),
    calendar_day_spline = ns(calendar_day, df = 4),
    age_treat_gp_rc = ifelse(age_treated_group=="90+","80-89",age_treated_group),
    had_hosp_covid60d6m = ifelse(!is.na(hosp_covid60d6m_date),"1yes","2no"),
    had_hosp_covid60d6m_01 = ifelse(!is.na(hosp_covid60d6m_date),1,0),
    had_hosp_allcause60d6m = ifelse(!is.na(hosp_allcause60d6m_date),"1yes","2no"),
    had__hosp_allcause60d6m_01 = ifelse(!is.na(hosp_allcause60d6m_date),1,0),
    had_ons_dead_date = ifelse(!is.na(ons_dead_date),"1yes","2no"),
    had__ons_dead_date_01 = ifelse(!is.na(ons_dead_date),1,0),
    imd = as.character(imd),
    had_housebound_r_num = ifelse(((housebound_lastdate > no_longer_housebound_lastdate) & (housebound_lastdate > moved_into_care_home_lastdate)),1,0)
) 


cat("#df_vars0$surv_event\n") 
freq_single_nrst5(df_vars0$surv_event)
cat("#risk_cohort\n") 
freq_single_nrst5(df_vars0$risk_cohort)

#df_vars0$highrisk_therap_cohort <- grepl("IMID|solid organ recipients|haematologic malignancy|Patients with a haematological diseases \\(sic\\)|sickle cell disease|stem cell transplant recipient|immune deficiencies|primary immune deficiencies|solid cancer", df_vars0$risk_cohort, ignore.case = TRUE)
df_vars0 <- df_vars0 %>%
  mutate(had_highrisk_therap = grepl("IMID|solid organ recipients|haematologic malignancy|primary immune deficiencies|solid cancer", risk_cohort, ignore.case = TRUE),
  highrisk_therap = as.integer(had_highrisk_therap),
  is_highrisk=(had_highrisk_therap|is_codelist_highrisk),
  is_highrisk_ever = (had_highrisk_therap|is_codelist_highrisk_ever),
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
)%>% ##high-risk-cohort-therap-codelist
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
                           ifelse(solid_organ_transplant_therap_code==1, "solid_organ_transplant_therap_code",
                           ifelse(haema_disease_therap_code==1, "haema_disease_therap_code",
                           ifelse(immunosupression_therap_code==1, "immunosupression_therap_code",
                           ifelse(solid_cancer_therap_code==1, "solid_cancer_therap_code", NA)))))),
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

#sex_num,ethnicity_num,imd_num,region_num,diabetes,hypertension,chronic_cardiac_disease,chronic_respiratory_disease
#autism,learning_disability,serious_mental_illness,dementia,housebound

#   had_diabetes, had_hypertension,had_chronic_cardiac_disease, had_chronic_respiratory_disease, had_autism,had_learning_disability,
#   had_serious_mental_illness, had_dementia, had_housebound
# #imid_therap_code,dialysis_therap_code,solid_organ_transplant_therap_code,haema_disease_therap_code
#immunosupression_therap_code,solid_cancer_therap_code,imid_ever_therap_code,solid_cancer_ever_therap_code

df_vars <- df_vars0 %>% filter(old_covid_treat == 0 )  %>% filter(!is.na(stp))%>% filter(allcause_death_under60d != 1) 
high_risk_cohort <- df_vars %>% filter(highrisk == 1 ) 
high_risk_ever_cohort <- df_vars %>% filter(highrisk_ever == 1 ) 
##cohort_molnup
cohort_molnup<-high_risk_cohort %>% filter(drug == 0 )
##cohort_sotro
cohort_sotro<-high_risk_cohort %>% filter(drug == 1 )

#freq_single(high_risk_cohort$diabetes)


cat("#high_risk_cohort$imd\n") 
freq_single(high_risk_cohort$imd)

cat("#high_risk_cohort$housebound\n") 
freq_single(high_risk_cohort$housebound)

cat("#freq_single-high_risk_cohort$had_housebound_r_num\n") 
freq_single(high_risk_cohort$had_housebound_r_num)

## Clinical and demographics table
variables <- c("age_treat_gp_rc", "sex","surv_event", "ethnicity", "region", "total_covid_vacc_cat", "first_covid_treat_interve")
high_risk_cohort_data <- high_risk_cohort %>%
  select(all_of(variables)) %>%
    mutate(
    across(where(is.character), as.factor)
    ) 

high_risk_cohort_data_sum <-high_risk_cohort_data %>%
  select(all_of(variables)) %>%
  tbl_summary()

proc_rm_b8 <- function(data,variables=variables) {
  result <- data %>%
    select(all_of(variables)) %>%
    mutate(
      across(where(is.factor), ~ {
        freq <- table(.x)
        levels_to_replace <- names(freq[freq < 8])
        .x <- factor(.x, levels = levels(.x))
        .x[.x %in% levels_to_replace] <- NA
        .x
      })
    ) %>%
    ungroup()
  result %>%
    tbl_summary(
    missing = "no"
    )
}

proc_rm_b8_str <- function(data, group_var = first_covid_treat_interve) {
  data %>% group_by(first_covid_treat_interve) %>%
  mutate(
    across(where(is.factor), ~ {
      # Calculate frequencies within each first_covid_treat_interve group
      freq <- table(.x)  
      levels_to_replace <- names(freq[freq < 8])  
      .x <- factor(.x, levels = levels(.x))
      .x[.x %in% levels_to_replace] <- NA
      .x
    })
  ) %>%
  ungroup() %>%
  tbl_summary(
    by = group_var,  
    missing = "no"  
  )
}

high_risk_cohort_tb1b <- proc_rm_b8(high_risk_cohort_data,variables=variables)
high_risk_cohort_tb1 <- proc_rm_b8_str(high_risk_cohort_data, "first_covid_treat_interve")

# Merge the tibbles
high_risk_cohort_tb1_all <- left_join((as_tibble(high_risk_cohort_tb1b)),(as_tibble(high_risk_cohort_tb1)), by = "**Characteristic**")
print(high_risk_cohort_tb1_all)

#imid_therap_code,dialysis_therap_code,solid_organ_transplant_therap_code,haema_disease_therap_code
#immunosupression_therap_code,solid_cancer_therap_code,imid_ever_therap_code,solid_cancer_ever_therap_code
# variables2 <- c("imd", "first_covid_treat_interve","imid_therap_code","dialysis_therap_code","solid_organ_transplant_therap_code","haema_disease_therap_code",
# "immunosupression_therap_code","solid_cancer_therap_code","imid_ever_therap_code","solid_cancer_ever_therap_code")

variables2 <- c("imd", "first_covid_treat_interve","high_risk_group")

high_risk_cohort_data2 <- high_risk_cohort %>%
  select(all_of(variables2)) %>%
    mutate(
    across(all_of(variables2), as.factor)
    ) 

high_risk_cohort_tb1b_2 <- proc_rm_b8(high_risk_cohort_data2,variables=variables2)
high_risk_cohort_tb1_2 <- proc_rm_b8_str(high_risk_cohort_data2, "first_covid_treat_interve")
high_risk_cohort_tb1_2_all <- left_join((as_tibble(high_risk_cohort_tb1b_2)),(as_tibble(high_risk_cohort_tb1_2)), by = "**Characteristic**")
print(high_risk_cohort_tb1_2_all)
#write.csv(high_risk_cohort_tb1_2_all, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort_tb1_2_all.csv"), row.names = FALSE)

# Save dataset(s) ----
write_csv(high_risk_cohort_tb1_all, here::here("output", "tables", "table1_redacted_8b.csv"))
write_csv(high_risk_cohort_tb1_2_all, here::here("output", "tables", "table1b_redacted_8b.csv"))
write.csv(df_vars, here::here("output", "data", "data4analyse.csv"), row.names = FALSE)
write_rds(df_vars, here::here("output", "data", "data4analyse.rds"), compress = "gz")
write.csv(high_risk_cohort, here::here("output", "data", "high_risk_cohort.csv"), row.names = FALSE)
write.csv(high_risk_ever_cohort, here::here("output", "data", "high_risk_ever_cohort.csv"), row.names = FALSE)
# #write.csv(high_risk_cohort_tb1_df, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort_tb1.csv"), row.names = FALSE)
# write.csv(df_vars, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/cohort_data4analyse.csv"), row.names = FALSE)


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

## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)

#df_vars00<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/dataset_table.csv.gz") %>% #,
df_vars00 <- read_csv(here::here("output", "data", "dataset_table.csv.gz")) %>%
  select(patient_id,age_treated, sex, age_treated_group, ethnicity_snome,ethnicity_snome_cat,ethnicity, imd1, imd, stp,region, 
  if_old_covid_treat,old_covid_treat, had_first_covid_treat, first_covid_treat_interve,drug, first_covid_treat_status,
  end_date_6mon, start_date_60d, treat_date, first_molnupiravir_date, first_sotrovimab_date, date_of_first_admis_af_treat, hosp_af_treat_alldiag_date,
  hosp_60daf_treat_alldiag_date, ccare_covid_first_af_treat_alldiag_date, hosp_covid60d6m_date, hosp_covid60d6m_classfic,
  hosp_covid60d6m_pdiag, had_ccare_covid60d6m, ccare_covid60d6m_date, hosp_allcause60d6m_date, hosp_allcause60d6m_classfic, hosp_allcause60d6m_pdiag, 
  hospitalise_disc_covid, hospitalise_disc_allcause, ons_dead_date, underly_deathcause_code, death_cause_covid, was_allcause_death_60d_6m,
  allcause_death_60d_6m, was_covid_death_60d_6m, covid_death_60d_6m, was_allcause_death_under60d, allcause_death_under60d, 
  allcause_death_under30d, bmi, is_molnu_pt_censored,is_sotro_pt_censored,molnu_pt_censored, sotro_pt_censored, molnu_pt_censored_date, sotro_pt_censored_date,risk_cohort,
  had_dialysis, had_kidney_transplant, transplant_thymus_opcs4,transplant_thymus_opcs4_count,transplant_thymus_opcs4_a, 
  transplant_thymus_opcs4_2, transplant_conjunctiva_y_code_opcs4, transplant_conjunctiva_y_code_opcs4_count,transplant_conjunctiva_opcs4,
  transplant_conjunctiva_opcs4_count, transplant_conjunctiva_opcs4_2,  oral_steroid_drugs_nhsd, 
  oral_steroid_drugs_nhsd_check, oral_steroid_drug_nhsd_3m_count,oral_steroid_drug_nhsd_12m_count, immunosuppresant_drugs_nhsd, 
  immunosuppresant_drugs_nhsd_ever, oral_steroid_drugs_nhsd_ever, is_codelist_highrisk, highrisk_codelist, is_codelist_highrisk_ever, 
  highrisk_codelist_ever, total_covid_vacc, total_covid_vacc_cat, covid_vacc1_date,covid_vacc2_date,covid_vacc3_date,
  covid_vacc4_date, covid_vacc_last_date, imid, imid_ever,dialysis,kidney_transplant,solid_organ_transplant_new,haema_disease,
  haema_disease_ever, immunosupression_new,solid_cancer_new,solid_cancer_ever,
  had_imid,had_imid_ever,had_dialysis,had_kidney_transplant,had_solid_organ_transplant_new,had_haema_disease,had_immunosupression_new,
  had_solid_cancer_new,had_solid_cancer_ever) 

df_vars0<-df_vars00 %>%  
    mutate(
    sex01 = ifelse(sex %in% ("female"), 1, 0),
    underly_covid_deathcause0_1 = ifelse(underly_deathcause_code %in% c("U071", "U072", "U099", "U109"), 1, 0),
    death_cause_covid0_1 = ifelse(death_cause_covid %in% ("TRUE"), 1,0 ), #) %>% 
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
    surv_event = ifelse((((!is.na(death_covid_cause_60d6m_date)) & (censored_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d6m_date)) & (censored_bf_dead_hosp == 0))), 1,0),
    surv_event_underly = ifelse((((!is.na(death_covid_underly_60d6m_date)) & (censored_bf_dead_hosp == 0))
    |((!is.na(hosp_covid60d6m_date)) & (censored_bf_dead_hosp == 0))),1,0),
    calendar_day =as.numeric(difftime(start_date_60d, as.Date("2021-12-16"),units = "days")),
    calendar_wk =as.numeric(difftime(start_date_60d, as.Date("2021-12-16"),units = "weeks")),
    age_treated_spline = ns(age_treated, df = 4),
    calendar_day_spline = ns(calendar_day, df = 4)
)

#r2nrst5
cat("#df_vars0$surv_event\n") 
freq_single_nrst5(df_vars0$surv_event)

cat("#oral_steroid_drugs_nhsd_check\n") #
freq_single_nrst5(df_vars0$oral_steroid_drugs_nhsd_check)

cat("#risk_cohort\n") 
freq_single_nrst5(df_vars0$risk_cohort)

df_vars0$highrisk_therap_cohort <- grepl("IMID|solid organ recipients|haematologic malignancy|Patients with a haematological diseases \\(sic\\)|sickle cell disease|stem cell transplant recipient|immune deficiencies|primary immune deficiencies|solid cancer", df_vars0$risk_cohort, ignore.case = TRUE)

df_vars0$had_highrisk_therap<-(df_vars0$highrisk_therap_cohort)
df_vars0<-df_vars0 %>% mutate(highrisk_therap = as.integer(had_highrisk_therap))

cat("#had_highrisk_therap\n") 
freq_single(df_vars0$had_highrisk_therap)

cat("#had_highrisk_therap-round5\n") 
freq_single_nrst5(df_vars0$had_highrisk_therap)

#is_highrisk,highrisk,is_highrisk_ever,highrisk_ever
df_vars0$is_highrisk = (df_vars0$had_highrisk_therap|df_vars0$is_codelist_highrisk)
df_vars0$is_highrisk_ever = (df_vars0$had_highrisk_therap|df_vars0$is_codelist_highrisk_ever)

df_vars0$highrisk = as.integer(df_vars0$is_highrisk)
df_vars0$highrisk_ever = as.integer(df_vars0$is_highrisk_ever)

##IMID #Immune Mediated Inflammatory Diseases
df_vars0$therap_IMID = as.integer((grepl("IMID", df_vars0$risk_cohort, ignore.case = TRUE)))

cat("#therap_IMID\n") 
freq_single(df_vars0$therap_IMID)
cat("#therap_IMID-round5\n") 
freq_single_nrst5(df_vars0$therap_IMID)

#solid organ recipients-SOR,
df_vars0$therap_SOR = as.integer((grepl("solid organ recipients", df_vars0$risk_cohort, ignore.case = TRUE)))

cat("#therap_SOR\n") 
freq_single(df_vars0$therap_SOR)
cat("#therap_SOR-round5\n") 
freq_single_nrst5(df_vars0$therap_SOR)

#Haematologic malignancy
df_vars0$therap_HMAL = as.integer((grepl("haematologic malignancy", df_vars0$risk_cohort, ignore.case = TRUE)))
cat("#therap_HMAL\n") 
freq_single(df_vars0$therap_HMAL)
cat("#therap_HMAL-round5\n") 
freq_single_nrst5(df_vars0$therap_HMAL)

#patients with a haematological diseases (sic)
df_vars0$therap_HMDs = as.integer((grepl("Patients with a haematological diseases \\(sic\\)", df_vars0$risk_cohort, ignore.case = TRUE)))
cat("#therap_HMDs\n") 
freq_single(df_vars0$therap_HMDs)
cat("#therap_HMDs-round5\n") 
freq_single_nrst5(df_vars0$therap_HMDs)

#sickle cell disease-SCD
df_vars0$therap_SCD = as.integer((grepl("sickle cell disease", df_vars0$risk_cohort, ignore.case = TRUE)))
cat("#therap_SCD\n") 
freq_single(df_vars0$therap_SCD)
cat("#therap_SCD-round5\n") 
freq_single_nrst5(df_vars0$therap_SCD)

#stem cell transplant recipient -SCTR
df_vars0$therap_SCTR= as.integer((grepl("stem cell transplant recipient", df_vars0$risk_cohort, ignore.case = TRUE)))
cat("#therap_SCTR\n") 
freq_single(df_vars0$therap_SCTR)
cat("#therap_SCTR-round5\n") 
freq_single_nrst5(df_vars0$therap_SCTR)

#immune deficiencies
df_vars0$therap_IMDs = as.integer((grepl("immune deficiencies", df_vars0$risk_cohort, ignore.case = TRUE)))
cat("#therap_IMDs\n") 
freq_single(df_vars0$therap_IMDs)
cat("#therap_IMDs-round5\n") 
freq_single_nrst5(df_vars0$therap_IMDs)

#primary immune deficiencies (PID)
df_vars0$therap_PID= as.integer((grepl("primary immune deficiencies", df_vars0$risk_cohort, ignore.case = TRUE)))
cat("#therap_PID\n") 
freq_single(df_vars0$therap_PID)
cat("#therap_PID-round5\n") 
freq_single_nrst5(df_vars0$therap_PID)

# solid_cancer
df_vars0$therap_solid_cancer= as.integer((grepl("solid cancer", df_vars0$risk_cohort, ignore.case = TRUE)))
cat("#therap_solid_cancer\n") 
freq_single(df_vars0$therap_solid_cancer)
cat("#therap_solid_cancer-round5\n") 
freq_single_nrst5(df_vars0$therap_solid_cancer)

##high-risk-cohort-therap-codelist
df_vars0 <-df_vars0 %>% mutate(imid_therap_code = ifelse(rowSums(cbind(imid,therap_IMID),na.rm = TRUE)>= 1, 1, 0),
  dialysis_therap_code = dialysis, kidney_transplant_therap_code = kidney_transplant,
  solid_organ_transplant_therap_code = ifelse(rowSums(cbind(solid_organ_transplant_new,therap_SOR),na.rm = TRUE)>= 1, 1, 0),
  haema_disease_therap_code = ifelse(rowSums(cbind(haema_disease,therap_HMDs,therap_HMAL,therap_SCD,therap_SCTR),na.rm = TRUE)>= 1, 1, 0),
  immunosupression_therap_code =ifelse(rowSums(cbind(immunosupression_new,therap_IMDs,therap_PID),na.rm = TRUE)>= 1, 1, 0),
  solid_cancer_therap_code = ifelse(rowSums(cbind(solid_cancer_new,therap_solid_cancer),na.rm = TRUE)>= 1, 1, 0),
  imid_ever_therap_code = ifelse(rowSums(cbind(imid_ever,therap_IMID),na.rm = TRUE)>= 1, 1, 0),
  solid_cancer_ever_therap_code =ifelse(rowSums(cbind(solid_cancer_ever,therap_solid_cancer),na.rm = TRUE)>= 1, 1, 0)
)


cat("#imid_therap_code\n") 
freq_single(df_vars0$imid_therap_code)
cat("#imid_therap_code-round5\n") 
freq_single_nrst5(df_vars0$imid_therap_code)

cat("#imid_ever_therap_code\n") 
freq_single(df_vars0$imid_ever_therap_code)
cat("#imid_ever_therap_code-round5\n") 
freq_single_nrst5(df_vars0$imid_ever_therap_code)

cat("#dialysis_therap_code\n") 
freq_single(df_vars0$dialysis_therap_code)
cat("#dialysis_therap_code-round5\n") 
freq_single_nrst5(df_vars0$dialysis_therap_code)

cat("#solid_organ_transplant_therap_code\n") 
freq_single(df_vars0$solid_organ_transplant_therap_code)
cat("#solid_organ_transplant_therap_code-round5\n") 
freq_single_nrst5(df_vars0$solid_organ_transplant_therap_code)

cat("#haema_disease_therap_code\n") 
freq_single(df_vars0$haema_disease_therap_code)
cat("#haema_disease_therap_code-round5\n") 
freq_single_nrst5(df_vars0$haema_disease_therap_code)

cat("#immunosupression_therap_code\n") 
freq_single(df_vars0$immunosupression_therap_code)
cat("#immunosupression_therap_code-round5\n") 
freq_single_nrst5(df_vars0$immunosupression_therap_code)

cat("#solid_cancer_therap_code\n") 
freq_single(df_vars0$solid_cancer_therap_code)
cat("#solid_cancer_therap_code-round5\n") 
freq_single_nrst5(df_vars0$solid_cancer_therap_code)

cat("#solid_cancer_ever_therap_code\n") 
freq_single(df_vars0$solid_cancer_ever_therap_code)
cat("#solid_cancer_ever_therap_code-round5\n") 
freq_single_nrst5(df_vars0$solid_cancer_ever_therap_code)

cat("#total-df_vars0\n") #old_covid_treat
dim(df_vars0)

# cat("#is_censored\n")
# freq_single_nrst5(df_vars0$is_censored)

cat("#df_vars0$censored_bf_dead_hosp-round5\n")
freq_single_nrst5(df_vars0$censored_bf_dead_hosp)

cat("#imd1-missing\n")
sum(is.na(df_vars0$imd1))

cat("#stp-missing\n")
sum(is.na(df_vars0$stp))

cat("# df_vars0-if_old_covid_treat-round5")
freq_single_nrst5(df_vars0$if_old_covid_treat)

cat("df_vars0-allcause_death_under60d-round5")
freq_single_nrst5(df_vars0$allcause_death_under60d)

cat("df_vars0-allcause_death_under30d-round5")
freq_single_nrst5(df_vars0$allcause_death_under30d)

cat("highrisk-round5\n")
freq_single_nrst5(df_vars0$highrisk)
cat("highrisk_ever-round5\n")
freq_single_nrst5(df_vars0$highrisk_ever)


df_vars <- df_vars0 %>% filter(old_covid_treat == 0 )  %>% filter(!is.na(stp))%>% filter(allcause_death_under60d != 1) 
cat("#total-dim(df_vars)\n")
dim(df_vars)

cat("#str-START-df_vars#\n")
str(df_vars,list.len= ncol(df_vars),give.attr = F)
cat("#str-END#\n")

# cat("#is_censored-df_vars\n")
# freq_single_nrst5(df_vars$is_censored)

cat("#if_old_covid_treat--df_vars")
freq_single(df_vars$if_old_covid_treat)
cat("#if_old_covid_treat--round5-df_vars")
freq_single_nrst5(df_vars$if_old_covid_treat)

cat("#had_first_covid_treat-round5-non_hospi,2021-12-16--2022-02-10-df_vars")
freq_single(df_vars$had_first_covid_treat)
cat("#had_first_covid_treat-round5-non_hospi,2021-12-16--2022-02-10-round5-df_vars")
freq_single_nrst5(df_vars$had_first_covid_treat)

cat("#interventions: Molnupiravir/Sotrovimab-df_vars")
freq_single(df_vars$first_covid_treat_interve)

cat("#interventions: Molnupiravir/Sotrovimab--round5-df_vars")
freq_single_nrst5(df_vars$first_covid_treat_interve)

cat("#df_vars$surv_event-outcome\n") 
freq_single(df_vars$surv_event)

cat("#df_vars$surv_event-outcome--round5-df_vars\n") 
freq_single_nrst5(df_vars$surv_event)

cat("#hosp_covid_date60d-6mon_count-df_vars#\n")
(sum(!is.na(df_vars$hosp_covid60d6m_date)))

cat("#hosp_covid_date60d-6mon_count-df_vars-round5#\n")
r2nrst5(sum(!is.na(df_vars$hosp_covid60d6m_date)))

cat("#hosp_allcause_date_count-df_vars#\n")
(sum(!is.na(df_vars$hosp_allcause60d6m_date)))

cat("#hosp_allcause_date_count-df_vars-round5#\n")
r2nrst5(sum(!is.na(df_vars$hosp_allcause60d6m_date)))

cat("#ons_dead_count-df_vars#\n")
sum(!is.na(df_vars$ons_dead_date))

cat("#ons_dead_count-df_vars-round5#\n")
r2nrst5(sum(!is.na(df_vars$ons_dead_date)))

cat("#death_cause_covid-df_vars\n")
freq_single(df_vars$death_cause_covid)

cat("#death_cause_covid-df_vars-round5-round5\n")
freq_single_nrst5(df_vars$death_cause_covid)

cat("all-cause death 60d-6m-df_vars\n")
freq_single(df_vars$allcause_death_60d_6m)

cat("all-cause death 60d-6m-df_vars-round5\n")
freq_single_nrst5(df_vars$allcause_death_60d_6m)

cat("covid_death_60d_6m-df_vars")
freq_single(df_vars$covid_death_60d_6m)

cat("covid_death_60d_6m-df_vars-round5")
freq_single_nrst5(df_vars$covid_death_60d_6m)

cat("allcause_death_under60d-df_vars")
freq_single(df_vars$allcause_death_under60d)

cat("allcause_death_under60d-df_vars-round5")
freq_single_nrst5(df_vars$allcause_death_under60d)

cat("allcause_death_under30d-df_vars")
freq_single(df_vars$allcause_death_under30d)

cat("allcause_death_under30d-df_vars-round5")
freq_single_nrst5(df_vars$allcause_death_under30d)

cat("#age-summary-df_vars:")
summary(as.numeric(df_vars$age_treated),na.rm=T )
mean(as.numeric(df_vars$age_treated),na.rm=T )
sd(as.numeric(df_vars$age_treated),na.rm=T )
IQR(as.numeric(df_vars$age_treated), na.rm=T )

cat("#age-group-df_vars")
freq_single(df_vars$age_treated_group)

cat("#age-group-df_vars-round5")
freq_single_nrst5(df_vars$age_treated_group)

cat("#sex-df_vars")
freq_single(df_vars$sex)

cat("#sex-df_vars-round5")
freq_single_nrst5(df_vars$sex)

cat("#imd-df_vars:")
freq_single(df_vars$imd)

cat("#imd-df_vars-round5")
freq_single_nrst5(df_vars$imd)

cat("#ethnicity-df_vars:")
freq_single(df_vars$ethnicity)

cat("#ethnicity-df_vars-round5")
freq_single_nrst5(df_vars$ethnicity)

#region
cat("#region-df_vars:")
freq_single(as.character(df_vars$region))

cat("#region-df_vars-round5")
freq_single_nrst5(as.character(df_vars$region))

#bmi
cat("#bmi-df_vars")
summary(df_vars$bmi)
mean(as.numeric(df_vars$bmi),na.rm=T)
sd(as.numeric(df_vars$bmi),na.rm=T)
IQR(as.numeric(df_vars$bmi), na.rm=T)

cat("had_dialysis-df_vars")
freq_single(df_vars$had_dialysis)

cat("had_dialysis-df_vars-round5")
freq_single_nrst5(df_vars$had_dialysis)

cat("had_kidney_transplant-df_vars")
freq_single(df_vars$had_kidney_transplant)

cat("had_kidney_transplant-df_vars-round5")
freq_single_nrst5(df_vars$had_kidney_transplant)

cat("transplant_thymus_opcs4-df_vars")
sum(!is.na(df_vars$transplant_thymus_opcs4))

cat("transplant_thymus_opcs4_count-df_vars")
freq_single_nrst5(as.factor(df_vars$transplant_thymus_opcs4_count))
summary(as.numeric(df_vars$transplant_thymus_opcs4_count))

cat("compare_transplant_thymus_opcs4-df_vars")
identical(df_vars$transplant_thymus_opcs4_a, df_vars$transplant_thymus_opcs4_2)

cat("transplant_conjunctiva_y_code_opcs4-df_vars")
sum(!is.na(df_vars$transplant_conjunctiva_y_code_opcs4))

cat("transplant_conjunctiva_y_code_opcs4_count-df_vars")
freq_single_nrst5(as.factor(df_vars$transplant_conjunctiva_y_code_opcs4_count))
summary(as.numeric(df_vars$transplant_conjunctiva_y_code_opcs4_count))

cat("transplant_conjunctiva_opcs4-df_vars")
sum(!is.na(df_vars$transplant_conjunctiva_opcs4))

cat("transplant_conjunctiva_opcs4_count-df_vars")
freq_single_nrst5(as.factor(df_vars$transplant_conjunctiva_opcs4_count))
summary(as.numeric(df_vars$transplant_conjunctiva_opcs4_count))

cat("compare_transplant_conjunctiva_opcs4-df_vars")
identical(df_vars$transplant_conjunctiva_opcs4_a, df_vars$transplant_conjunctiva_opcs4_2)

#censored_bf_dead_hosp#df_vars
cat("#df_vars$censored_bf_dead_hosp")
freq_single(df_vars$censored_bf_dead_hosp)

cat("#df_vars$censored_bf_dead_hosp-round5")
freq_single_nrst5(df_vars$censored_bf_dead_hosp)

##high_risk_cohort
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
# cat("sex_imd-table-high_risk_cohort")
# dt_sex_imd<-high_risk_cohort %>%
#  dplyr::count(sex,imd) %>%
#  dplyr::group_by(sex) %>% 
#  dplyr::mutate(prop = prop.table(n)) #%>% print(n = nrow(dt))
# print(dt_sex_imd)

##first_covid_treat_interve
cat("#first_covid_treat_interve-high_risk_cohort")
freq_single(high_risk_cohort$first_covid_treat_interve)

cat("#first_covid_treat_interve-high_risk_cohort-round5")
freq_single_nrst5(high_risk_cohort$first_covid_treat_interve)

#outcomes
cat("#high_risk_cohort$surv_event-total-outcome\n") 
freq_single(high_risk_cohort$surv_event)
cat("#high_risk_cohort$surv_event-total-outcome-round5\n") 
freq_single_nrst5(high_risk_cohort$surv_event)

cat("#hosp_covid_date60d_6mon_count-high_risk_cohort#\n")
(sum(!is.na(high_risk_cohort$hosp_covid60d6m_date)))
cat("#hosp_covid_date60d_6mon_count-high_risk_cohort-round5 #\n")
r2nrst5(sum(!is.na(high_risk_cohort$hosp_covid60d6m_date)))

cat("#hosp_allcause_date60d_6mon_count-high_risk_cohort\n")
(sum(!is.na(high_risk_cohort$hosp_allcause60d6m_date)))
cat("#hosp_allcause_date60d_6mon_count-high_risk_cohort-round5#\n")
r2nrst5(sum(!is.na(high_risk_cohort$hosp_allcause60d6m_date)))

cat("#ons_dead_count_anytime-high_risk_cohort#\n")
sum(!is.na(high_risk_cohort$ons_dead_date))
cat("#ons_dead_count_anytime-high_risk_cohort-round5#\n")
r2nrst5(sum(!is.na(high_risk_cohort$ons_dead_date)))

cat("#death_cause_covid-high_risk_cohort\n")
freq_single(high_risk_cohort$death_cause_covid)
cat("#death_cause_covid-high_risk_cohort-round5\n")
freq_single_nrst5(high_risk_cohort$death_cause_covid)

cat("all-cause death 60d-6m:-high_risk_cohort\n")
freq_single(high_risk_cohort$allcause_death_60d_6m)
cat("all-cause death 60d-6m:-high_risk_cohort-round5\n")
freq_single_nrst5(high_risk_cohort$allcause_death_60d_6m)

cat("covid_death_60d_6m:-high_risk_cohort\n")
freq_single(high_risk_cohort$covid_death_60d_6m)
cat("covid_death_60d_6m:-high_risk_cohort-round5\n")
freq_single_nrst5(high_risk_cohort$covid_death_60d_6m)

cat("allcause_death_under60d-high_risk_cohort\n")
freq_single(high_risk_cohort$allcause_death_under60d)
cat("allcause_death_under60d-high_risk_cohort-round5\n")
freq_single_nrst5(high_risk_cohort$allcause_death_under60d)

cat("allcause_death_under30d-high_risk_cohort\n")
freq_single(high_risk_cohort$allcause_death_under30d)
cat("allcause_death_under30d-high_risk_cohort-round5\n")
freq_single_nrst5(high_risk_cohort$allcause_death_under30d)

cat("#age-summary-high_risk_cohort")
summary(as.numeric(high_risk_cohort$age_treated),na.rm=T)
cat("#age-mean-high_risk_cohort")
mean(as.numeric(high_risk_cohort$age_treated),na.rm=T)
cat("#age-sd-high_risk_cohort")
sd(as.numeric(high_risk_cohort$age_treated),na.rm=T)
cat("#age-IQR-high_risk_cohort")
IQR(as.numeric(high_risk_cohort$age_treated), na.rm=T)

cat("#age-group-high_risk_cohort")
freq_single(high_risk_cohort$age_treated_group)
cat("#age-group-high_risk_cohort-round5\n")
freq_single_nrst5(high_risk_cohort$age_treated_group)

cat("#sex-high_risk_cohort")
freq_single(high_risk_cohort$sex)
cat("#sex-high_risk_cohort-round5\n")
freq_single_nrst5(high_risk_cohort$sex)

cat("#imd-high_risk_cohort")
freq_single(high_risk_cohort$imd)
cat("#imd-high_risk_cohort-round5")
freq_single_nrst5(high_risk_cohort$imd)

cat("#ethnicity-high_risk_cohort")
freq_single(high_risk_cohort$ethnicity)
cat("#ethnicity-high_risk_cohort-round5\n")
freq_single_nrst5(high_risk_cohort$ethnicity)

cat("#region-high_risk_cohort")
freq_single(as.character(high_risk_cohort$region))
cat("#region-high_risk_cohort-round5\n")
freq_single_nrst5(as.character(high_risk_cohort$region))

cat("#bmi-high_risk_cohort")
#summary(high_risk_cohort$bmi)
summary(as.numeric(high_risk_cohort$bmi),na.rm=T)
mean(as.numeric(high_risk_cohort$bmi),na.rm=T)
sd(as.numeric(high_risk_cohort$bmi),na.rm=T)
IQR(as.numeric(high_risk_cohort$bmi), na.rm=T)

cat("#total_covid_vacc_cat-high_risk_cohort")
freq_single(high_risk_cohort$total_covid_vacc_cat)
cat("#total_covid_vacc_cat-high_risk_cohort-round5")
freq_single_nrst5(high_risk_cohort$total_covid_vacc_cat)

cat("#high_risk_cohort$stp")
freq_single(high_risk_cohort$stp)
cat("#high_risk_cohort$stp-round5")
freq_single_nrst5(high_risk_cohort$stp)

cat("#length_unique_high_risk_cohort$stp")
length(unique(high_risk_cohort$stp))

#censored_bf_dead_hosp
cat("#high_risk_cohort$censored_bf_dead_hosp")
freq_single(high_risk_cohort$censored_bf_dead_hosp)
cat("#high_risk_cohort$censored_bf_dead_hosp-round5")
freq_single_nrst5(high_risk_cohort$censored_bf_dead_hosp)

##high-risk-cohort-therap-codelist#high_risk_cohort
cat("#imid_therap_code\n") 
freq_single(high_risk_cohort$imid_therap_code)
cat("#imid_therap_code-round5\n") 
freq_single_nrst5(high_risk_cohort$imid_therap_code)

cat("#imid_ever_therap_code\n") 
freq_single(high_risk_cohort$imid_ever_therap_code)
cat("#imid_ever_therap_code-round5\n") 
freq_single_nrst5(high_risk_cohort$imid_ever_therap_code)

cat("#dialysis_therap_code\n") 
freq_single(high_risk_cohort$dialysis_therap_code)
cat("#dialysis_therap_code-round5\n") 
freq_single_nrst5(high_risk_cohort$dialysis_therap_code)

cat("#solid_organ_transplant_therap_code\n") 
freq_single(high_risk_cohort$solid_organ_transplant_therap_code)
cat("#solid_organ_transplant_therap_code-round5\n") 
freq_single_nrst5(high_risk_cohort$solid_organ_transplant_therap_code)

cat("#haema_disease_therap_code\n") 
freq_single(high_risk_cohort$haema_disease_therap_code)
cat("#haema_disease_therap_code-round5\n") 
freq_single_nrst5(high_risk_cohort$haema_disease_therap_code)

cat("#immunosupression_therap_code\n") 
freq_single(high_risk_cohort$immunosupression_therap_code)
cat("#immunosupression_therap_code-round5\n") 
freq_single_nrst5(high_risk_cohort$immunosupression_therap_code)

cat("#solid_cancer_therap_code\n") 
freq_single(high_risk_cohort$solid_cancer_therap_code)
cat("#solid_cancer_therap_code-round5\n") 
freq_single_nrst5(high_risk_cohort$solid_cancer_therap_code)

cat("#solid_cancer_ever_therap_code\n") 
freq_single(high_risk_cohort$solid_cancer_ever_therap_code)
cat("#solid_cancer_ever_therap_code-round5\n") 
freq_single_nrst5(high_risk_cohort$solid_cancer_ever_therap_code)
##################################################


##cohort_molnup
cohort_molnup<-high_risk_cohort %>% filter(drug== 0 )
cat("#total-dim(cohort_molnup)\n")
dim(cohort_molnup)

#outcomes
cat("#cohort_molnup$surv_event-total-outcome\n") 
freq_single(cohort_molnup$surv_event)
cat("#cohort_molnup$surv_event-total-outcome-round5\n") 
freq_single_nrst5(cohort_molnup$surv_event)

cat("#hosp_covid_date60d_6mon_count-cohort_molnup- #\n")
(sum(!is.na(cohort_molnup$hosp_covid60d6m_date)))
cat("#hosp_covid_date60d_6mon_count-cohort_molnup-round5 #\n")
r2nrst5(sum(!is.na(cohort_molnup$hosp_covid60d6m_date)))

cat("#hosp_allcause_date60d_6mon_count-cohort_molnup#\n")
(sum(!is.na(cohort_molnup$hosp_allcause60d6m_date)))
cat("#hosp_allcause_date60d_6mon_count-cohort_molnup-round5#\n")
r2nrst5(sum(!is.na(cohort_molnup$hosp_allcause60d6m_date)))

cat("#ons_dead_count_anytime-cohort_molnup-#\n")
sum(!is.na(cohort_molnup$ons_dead_date))
cat("#ons_dead_count_anytime-cohort_molnup-round5#\n")
r2nrst5(sum(!is.na(cohort_molnup$ons_dead_date)))

cat("#death_cause_covid-cohort_molnup\n")
freq_single(cohort_molnup$death_cause_covid)
cat("#death_cause_covid-cohort_molnup-round5\n")
freq_single_nrst5(cohort_molnup$death_cause_covid)

cat("all-cause death 60d-6m:-cohort_molnup\n")
freq_single(cohort_molnup$allcause_death_60d_6m)
cat("all-cause death 60d-6m:-cohort_molnup-round5\n")
freq_single_nrst5(cohort_molnup$allcause_death_60d_6m)

cat("covid_death_60d_6m:-cohort_molnup")
freq_single(cohort_molnup$covid_death_60d_6m)
cat("covid_death_60d_6m:-cohort_molnup-round5")
freq_single_nrst5(cohort_molnup$covid_death_60d_6m)

cat("allcause_death_under60d-cohort_molnup")
freq_single(cohort_molnup$allcause_death_under60d)
cat("allcause_death_under60d-cohort_molnup-round5")
freq_single_nrst5(cohort_molnup$allcause_death_under60d)

cat("allcause_death_under30d-cohort_molnup")
freq_single(cohort_molnup$allcause_death_under30d)
cat("allcause_death_under30d-cohort_molnup-round5")
freq_single_nrst5(cohort_molnup$allcause_death_under30d)

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
cat("#age-group-cohort_molnup-round5")
freq_single_nrst5(cohort_molnup$age_treated_group)

cat("#sex-cohort_molnup:")
freq_single(cohort_molnup$sex)
cat("#sex-cohort_molnup-round5")
freq_single_nrst5(cohort_molnup$sex)

cat("#imd-cohort_molnup:")
freq_single(cohort_molnup$imd)
cat("#imd-cohort_molnup-round5")
freq_single_nrst5(cohort_molnup$imd)

cat("#ethnicity-cohort_molnup:")
freq_single(cohort_molnup$ethnicity)
cat("#ethnicity-cohort_molnup-round5")
freq_single_nrst5(cohort_molnup$ethnicity)

cat("#region-cohort_molnup:")
freq_single(as.character(cohort_molnup$region))
cat("#region-cohort_molnup-round5")
freq_single_nrst5(as.character(cohort_molnup$region))

cat("#bmi-cohort_molnup")
#summary(cohort_molnup$bmi)
summary(as.numeric(cohort_molnup$bmi),na.rm=T)
mean(as.numeric(cohort_molnup$bmi),na.rm=T)
sd(as.numeric(cohort_molnup$bmi),na.rm=T)
IQR(as.numeric(cohort_molnup$bmi), na.rm=T)

cat("#total_covid_vacc_cat-cohort_molnup")
freq_single(cohort_molnup$total_covid_vacc_cat)
cat("#total_covid_vacc_cat-cohort_molnup-round5")
freq_single_nrst5(cohort_molnup$total_covid_vacc_cat)

##high-risk-cohort-therap-codelist#cohort_molnup
cat("#imid_therap_code\n") 
freq_single(cohort_molnup$imid_therap_code)
cat("#imid_therap_code-round5\n") 
freq_single_nrst5(cohort_molnup$imid_therap_code)

cat("#imid_ever_therap_code\n") 
freq_single(cohort_molnup$imid_ever_therap_code)
cat("#imid_ever_therap_code-round5\n") 
freq_single_nrst5(cohort_molnup$imid_ever_therap_code)

cat("#dialysis_therap_code\n") 
freq_single(cohort_molnup$dialysis_therap_code)
cat("#dialysis_therap_code-round5\n") 
freq_single_nrst5(cohort_molnup$dialysis_therap_code)

cat("#solid_organ_transplant_therap_code\n") 
freq_single(cohort_molnup$solid_organ_transplant_therap_code)
cat("#solid_organ_transplant_therap_code-round5\n") 
freq_single_nrst5(cohort_molnup$solid_organ_transplant_therap_code)

cat("#haema_disease_therap_code\n") 
freq_single(cohort_molnup$haema_disease_therap_code)
cat("#haema_disease_therap_code-round5\n") 
freq_single_nrst5(cohort_molnup$haema_disease_therap_code)

cat("#immunosupression_therap_code\n") 
freq_single(cohort_molnup$immunosupression_therap_code)
cat("#immunosupression_therap_code-round5\n") 
freq_single_nrst5(cohort_molnup$immunosupression_therap_code)

cat("#solid_cancer_therap_code\n") 
freq_single(cohort_molnup$solid_cancer_therap_code)
cat("#solid_cancer_therap_code-round5\n") 
freq_single_nrst5(cohort_molnup$solid_cancer_therap_code)

cat("#solid_cancer_ever_therap_code\n") 
freq_single(cohort_molnup$solid_cancer_ever_therap_code)
cat("#solid_cancer_ever_therap_code-round5\n") 
freq_single_nrst5(cohort_molnup$solid_cancer_ever_therap_code)

################
###cohort_sotro
cohort_sotro<-high_risk_cohort %>% filter(drug== 1 )
##outcomes
cat("#cohort_sotro$surv_event-total-outcome\n") 
freq_single(cohort_sotro$surv_event)
cat("#cohort_sotro$surv_event-total-outcome-round5\n") 
freq_single_nrst5(cohort_sotro$surv_event)

cat("#total-dim(cohort_sotro)\n")
dim(cohort_sotro)

cat("#str-START-cohort_sotro#\n")
str(cohort_sotro,list.len= ncol(cohort_sotro),give.attr = F)
cat("#str-END#\n")

cat("#hosp_covid_date60d_6mon_count-cohort_sotro #\n")
(sum(!is.na(cohort_sotro$hosp_covid60d6m_date)))
cat("#hosp_covid_date60d_6mon_count-cohort_sotro-round5 #\n")
r2nrst5(sum(!is.na(cohort_sotro$hosp_covid60d6m_date)))

cat("#hosp_allcause_date60d_6mon_count-cohort_sotro#\n")
(sum(!is.na(cohort_sotro$hosp_allcause60d6m_date)))
cat("#hosp_allcause_date60d_6mon_count-cohort_sotro-round5#\n")
r2nrst5(sum(!is.na(cohort_sotro$hosp_allcause60d6m_date)))

cat("#ons_dead_count_anytime-cohort_sotro-#\n")
sum(!is.na(cohort_sotro$ons_dead_date))
cat("#ons_dead_count_anytime-cohort_sotro-round5#\n")
r2nrst5(sum(!is.na(cohort_sotro$ons_dead_date)))

cat("#death_cause_covid-cohort_sotro\n")
freq_single(cohort_sotro$death_cause_covid)
cat("#death_cause_covid-cohort_sotro-round5\n")
freq_single_nrst5(cohort_sotro$death_cause_covid)

cat("all-cause death 60d-6m:-cohort_sotro\n")
freq_single(cohort_sotro$allcause_death_60d_6m)
cat("all-cause death 60d-6m:-cohort_sotro-round5\n")
freq_single_nrst5(cohort_sotro$allcause_death_60d_6m)

cat("covid_death_60d_6m:-cohort_sotro")
freq_single(cohort_sotro$covid_death_60d_6m)
cat("covid_death_60d_6m:-cohort_sotro-round5")
freq_single_nrst5(cohort_sotro$covid_death_60d_6m)

cat("allcause_death_under60d-cohort_sotro")
freq_single(cohort_sotro$allcause_death_under60d)
cat("allcause_death_under60d-cohort_sotro-round5")
freq_single_nrst5(cohort_sotro$allcause_death_under60d)

cat("allcause_death_under30d-cohort_sotro")
freq_single(cohort_sotro$allcause_death_under30d)
cat("allcause_death_under30d-cohort_sotro-round5")
freq_single_nrst5(cohort_sotro$allcause_death_under30d)

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
cat("#age-group-cohort_sotro-round5")
freq_single_nrst5(cohort_sotro$age_treated_group)

cat("#sex-cohort_sotro:")
freq_single(cohort_sotro$sex)
cat("#sex-cohort_sotro-round5:")
freq_single_nrst5(cohort_sotro$sex)

cat("#imd-cohort_sotro:")
freq_single(cohort_sotro$imd)
cat("#imd-cohort_sotro-round5:")
freq_single_nrst5(cohort_sotro$imd)

cat("#ethnicity-cohort_sotro:")
freq_single(cohort_sotro$ethnicity)
cat("#ethnicity-cohort_sotro-round5:")
freq_single_nrst5(cohort_sotro$ethnicity)

cat("#region-cohort_sotro:")
freq_single(as.character(cohort_sotro$region))
cat("#region-cohort_sotro-round5:")
freq_single_nrst5(as.character(cohort_sotro$region))

cat("#bmi-cohort_sotro")
summary(as.numeric(cohort_sotro$bmi))
mean(as.numeric(cohort_sotro$bmi),na.rm=T)
sd(as.numeric(cohort_sotro$bmi),na.rm=T)
IQR(as.numeric(cohort_sotro$bmi), na.rm=T)

cat("#total_covid_vacc_cat-cohort_sotro")
freq_single(cohort_sotro$total_covid_vacc_cat)
cat("#total_covid_vacc_cat-cohort_sotro-round5")
freq_single_nrst5(cohort_sotro$total_covid_vacc_cat)

##high-risk-cohort-therap-codelist#cohort_sotro
cat("#imid_therap_code\n") 
freq_single(cohort_sotro$imid_therap_code)
cat("#imid_therap_code-round5\n") 
freq_single_nrst5(cohort_sotro$imid_therap_code)

cat("#imid_ever_therap_code\n") 
freq_single(cohort_sotro$imid_ever_therap_code)
cat("#imid_ever_therap_code-round5\n") 
freq_single_nrst5(cohort_sotro$imid_ever_therap_code)

cat("#dialysis_therap_code\n") 
freq_single(cohort_sotro$dialysis_therap_code)
cat("#dialysis_therap_code-round5\n") 
freq_single_nrst5(cohort_sotro$dialysis_therap_code)

cat("#solid_organ_transplant_therap_code\n") 
freq_single(cohort_sotro$solid_organ_transplant_therap_code)
cat("#solid_organ_transplant_therap_code-round5\n") 
freq_single_nrst5(cohort_sotro$solid_organ_transplant_therap_code)

cat("#haema_disease_therap_code\n") 
freq_single(cohort_sotro$haema_disease_therap_code)
cat("#haema_disease_therap_code-round5\n") 
freq_single_nrst5(cohort_sotro$haema_disease_therap_code)

cat("#immunosupression_therap_code\n") 
freq_single(cohort_sotro$immunosupression_therap_code)
cat("#immunosupression_therap_code-round5\n") 
freq_single_nrst5(cohort_sotro$immunosupression_therap_code)

cat("#solid_cancer_therap_code\n") 
freq_single(cohort_sotro$solid_cancer_therap_code)
cat("#solid_cancer_therap_code-round5\n") 
freq_single_nrst5(cohort_sotro$solid_cancer_therap_code)

cat("#solid_cancer_ever_therap_code\n") 
freq_single(cohort_sotro$solid_cancer_ever_therap_code)
cat("#solid_cancer_ever_therap_code-round5\n") 
freq_single_nrst5(cohort_sotro$solid_cancer_ever_therap_code)


high_risk_cohort$surv_days <- as.numeric(high_risk_cohort$surv_days)
high_risk_cohort$surv_event <- as.numeric(high_risk_cohort$surv_event)

cat("#summary(cox_model)")
#surv_days,surv_event
cox_model <- coxph(Surv(surv_days, surv_event) ~ factor(drug)+ strata(factor(stp)), data = high_risk_cohort)
summary(cox_model)

#age_treated, sex01
cox_model1_age_sex <- coxph(Surv(surv_days, surv_event) ~ factor(drug) + age_treated + sex01+ strata(factor(stp)), data = high_risk_cohort)
summary(cox_model1_age_sex)

# Plot the survival curves
# ggsurvplot(survfit(cox_model), data = high_risk_cohort, pval = TRUE,
#            ggtheme = theme_minimal(), risk.table = TRUE,
#            conf.int = TRUE)

# Save dataset(s) ----
write.csv(df_vars, here::here("output", "tables", "data4analyse.csv"), row.names = FALSE)
write_rds(df_vars, here::here("output", "data", "data4analyse.rds"), compress = "gz")
write_rds(high_risk_cohort, here::here("output", "data", "high_risk_cohort.rds"), compress = "gz")
write_rds(high_risk_ever_cohort, here::here("output", "data", "high_risk_ever_cohort.rds"), compress = "gz")

# write.csv(df_vars, ("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/cohort_data4analyse.csv"), row.names = FALSE)

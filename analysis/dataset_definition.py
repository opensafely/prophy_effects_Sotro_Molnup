#<dadaset_definition.py> for <long_term_pax_sotro_molnup>
# Description: This script extracts data for project 91
#Author(s): Qing Wen
# Date last updated: 16/04/2024
###################################
from ehrql import(
    months,
    days,
    case,
    create_dataset,
    when,
    minimum_of,
)
from ehrql.tables.tpp import( 
    addresses,
    clinical_events,
    apcs,
    medications,
    patients,
    practice_registrations,
    ons_deaths,
    vaccinations,
)

from ehrql.tables.raw.tpp import(
    covid_therapeutics_raw,
)

#from ehrql.codes import *
from ehrql.codes import CTV3Code, ICD10Code

## codelists 
from codelists import(ethnicity_codelist, covid_icd10_codes, ethnicity,dialysis_codes,dialysis_icd10_codelist,
    dialysis_opcs4_codelist, kidney_transplant_codes, kidney_tx_icd10_codelist, kidney_tx_opcs4_codelist,
    solid_organ_transplant_nhsd_snomed_codes, solid_organ_transplant_nhsd_snomed_codes_new, dialysis_opcs4_codelist,
    haematopoietic_stem_cell_transplant_nhsd_snomed_codes, haematopoietic_stem_cell_transplant_nhsd_icd10_codes, 
    haematological_malignancies_nhsd_snomed_codes, haematological_malignancies_nhsd_icd10_codes, 
    sickle_cell_disease_nhsd_snomed_codes, sickle_cell_disease_nhsd_icd10_codes, immunosupression_nhsd_codes,
    immunosupression_nhsd_codes_new, immunosuppresant_drugs_dmd_codes, immunosuppresant_drugs_snomed_codes, 
    oral_steroid_drugs_dmd_codes, oral_steroid_drugs_snomed_codes, solid_organ_transplant_nhsd_opcs4_codes,
    replacement_of_organ_transplant_nhsd_opcs4_codes,thymus_gland_transplant_nhsd_opcs4_codes, conjunctiva_y_codes_transplant_nhsd_opcs4_codes, 
    conjunctiva_transplant_nhsd_opcs4_codes, stomach_transplant_nhsd_opcs4_codes, ileum_1_y_codes_transplant_nhsd_opcs4_codes, 
    ileum_2_y_codes_transplant_nhsd_opcs4_codes, ileum_1_transplant_nhsd_opcs4_codes, ileum_2_transplant_nhsd_opcs4_codes, 
    ileum_2_transplant_nhsd_opcs4_codes, haematopoietic_stem_cell_transplant_nhsd_opcs4_codes, 
    haematopoietic_stem_cell_transplant_nhsd_opcs4_codes, pregnancy_primis_codes,pregdel_primis_codes, non_haematological_cancer_opensafely_snomed_codes,
    non_haematological_cancer_opensafely_snomed_codes_new, lung_cancer_opensafely_snomed_codes, chemotherapy_radiotherapy_opensafely_snomed_codes,
    care_home_primis_snomed_codes)

index_startdate = "2021-12-16"  #index_date-studystart1_date
index_enddate = "2022-02-10"

## def_funs files
from def_funs import(is_fem_male, is_registered, 
    bmi_record, first_covid_therap_date, cause_of_death_matches, any_of) #, is_alive  #is_adult,

dataset = create_dataset()
dataset.configure_dummy_data(population_size = 3000)

##covid_therapeutics
had_covid_treat = (
    covid_therapeutics_raw
    .where(covid_therapeutics_raw.covid_indication.is_in(["non_hospitalised"])) 
    .sort_by(covid_therapeutics_raw.treatment_start_date) 
    .where(covid_therapeutics_raw.intervention.is_in(["Molnupiravir","Sotrovimab"]) & #["Molnupiravir","Sotrovimab","Paxlovid"]
    (covid_therapeutics_raw.treatment_start_date>=index_startdate) &
    (covid_therapeutics_raw.treatment_start_date<=index_enddate)
    ).sort_by(covid_therapeutics_raw.treatment_start_date).first_for_patient()
)

dataset.first_covid_treat_date = had_covid_treat.treatment_start_date
dataset.first_covid_treat_interve= had_covid_treat.intervention
dataset.first_covid_treat_status= had_covid_treat.current_status

treat_date = dataset.first_covid_treat_date
dataset.start_date = treat_date
dataset.date_treated = treat_date

is_adult = (patients.age_on(index_startdate) >= 18) & (
    patients.age_on(index_startdate) <= 110
)

is_alive = (
    patients.date_of_death.is_after(index_startdate)
    | patients.date_of_death.is_null()
)

dataset.define_population(
    is_fem_male
    & is_adult
    & is_alive
    & is_registered
#    & had_covid_treat
)

# demographic variables 1
dataset.dob = patients.date_of_birth ##dob
dataset.dod = patients.date_of_death  #dod: date of death
dataset.sex = patients.sex

# studystart2_date="2022-02-11" # studyend2_date="2023-03-31" 

dataset.age_sstart = patients.age_on("2021-12-16")
dataset.age_sstart_group = case(
        when(dataset.age_sstart < 30).then("18-29"),
        when(dataset.age_sstart < 40).then("30-39"),
        when(dataset.age_sstart < 50).then("40-49"),
        when(dataset.age_sstart < 60).then("50-59"),
        when(dataset.age_sstart < 70).then("60-69"),
        when(dataset.age_sstart < 80).then("70-79"),
        when(dataset.age_sstart < 90).then("80-89"),
        when(dataset.age_sstart >= 90).then("90+"),
        otherwise="unknown",
)

## demographic variables 2 on treatment day
dataset.age_treated = patients.age_on(treat_date)
# address = addresses.for_patient_on(treat_date)
# imd_rounded = addresses.for_patient_on(treat_date).imd_rounded

dataset.age_treated_group = case(
        when(dataset.age_treated < 30).then("18-29"),
        when(dataset.age_treated < 40).then("30-39"),
        when(dataset.age_treated < 50).then("40-49"),
        when(dataset.age_treated < 60).then("50-59"),
        when(dataset.age_treated < 70).then("60-69"),
        when(dataset.age_treated < 80).then("70-79"),
        when(dataset.age_treated < 90).then("80-89"),
        when(dataset.age_treated >= 90).then("90+"),
        otherwise="unknown",
)

ethnicity_snome = clinical_events.where(
        clinical_events.snomedct_code.is_in(ethnicity_codelist) #snomedct_code=clinical_events.ctv3_code.is_in(ethnicity_codelist)
).sort_by(clinical_events.date).last_for_patient().snomedct_code.to_category(ethnicity_codelist)  #more missing 1879)

dataset.ethnicity_snome = ethnicity_snome

dataset.ethnicity_snome_cat = case(
    when (ethnicity_snome == "1").then("White"),
    when (ethnicity_snome == "2").then("Mixed"),
    when (ethnicity_snome == "3").then("South Asian"),
    when (ethnicity_snome == "4").then("Black"),
    when (ethnicity_snome == "5").then("Other"),
    otherwise = "Unknown",
)
 
###ethnicity2 from comparative-booster-spring2023 <dataset_definition.py>code
dataset.ethnicity_ctv3 = clinical_events.where(
        clinical_events.ctv3_code.is_in(ethnicity)
).sort_by(clinical_events.date).last_for_patient().ctv3_code.to_category(ethnicity) #missing 367

spanning_addrs = addresses.where(addresses.start_date <= treat_date).except_where(
    addresses.end_date < treat_date
)

ordered_addrs = spanning_addrs.sort_by(
    case(when(addresses.has_postcode).then(1), otherwise=0),
    addresses.start_date,
    addresses.end_date,
    addresses.address_id,
).last_for_patient()

# dataset.has_postcode = ordered_addrs.has_postcode
# dataset.addrs_type = ordered_addrs.address_type
# dataset.care_home = ordered_addrs.care_home_is_potential_match 
# dataset.care_home_nurse = ordered_addrs.care_home_requires_nursing 
# dataset.care_home_nonurse = ordered_addrs.care_home_does_not_require_nursing 

#######
# Care home based on primis codes/TPP address match
carehome_primis = clinical_events.where(
        clinical_events.snomedct_code.is_in(care_home_primis_snomed_codes)
    ).where(
        clinical_events.date.is_on_or_before(treat_date)
    ).exists_for_patient() 

carehome_tpp = addresses.for_patient_on(treat_date).care_home_is_potential_match 

carehome = case(
    when(carehome_primis).then(True),
    when(carehome_tpp).then(True),
    otherwise=False
)

dataset.care_home_primis = carehome
#######
#dataset.care_home_0= sum_for_patient(dataset.care_home,dataset.care_home_nurse,dataset.care_home_nonurse )
# dataset.care_home_0= case(
#     when((ordered_addrs.care_home_is_potential_match=="TRUE")|(ordered_addrs.care_home_requires_nursing=="TRUE")|(ordered_addrs.care_home_does_not_require_nursing== "TRUE")).then(1),
#     otherwise = 0
# )
# #max_imd = 32844
imd0 = addresses.for_patient_on(treat_date).imd_rounded
dataset.imd = case(
    when((imd0 >=0) & (imd0 < int(32844 * 1 / 5))).then("1 (most deprived)"),
    when(imd0 < int(32844 * 2 / 5)).then("2"),
    when(imd0 < int(32844 * 3 / 5)).then("3"),
    when(imd0 < int(32844 * 4 / 5)).then("4"),
    when(imd0 < int(32844 * 5 / 5)).then("5 (least deprived)"),
    otherwise="unknown"
)

##Rurality##
#dataset.rural_urban = addresses.rural_urban_classification
# Index of Multiple Deprevation Rank (rounded down to nearest 100)
#dataset.imd = addresses.imd_rounded  ## code not working
##addresses.for_patient_on(treat_date).imd_rounded
dataset.imd1 = addresses.for_patient_on(treat_date).imd_rounded

#registrations
regd = practice_registrations.for_patient_on(treat_date)
dataset.region = regd.practice_nuts1_region_name
# STP
dataset.stp = regd.practice_stp

regd_end = practice_registrations.spanning(index_startdate, index_enddate
).sort_by(practice_registrations.end_date).first_for_patient()

dataset.dereg_date= regd_end.end_date

de_reg = (practice_registrations
    .where((practice_registrations.start_date.is_on_or_before(treat_date)) & (practice_registrations.start_date.is_not_null()) & 
    practice_registrations.end_date.is_after(treat_date))).sort_by(practice_registrations.end_date).first_for_patient()

dataset.dereg_date1 = de_reg.end_date

# Rurality
dataset.rural_urban = addresses.for_patient_on(treat_date).rural_urban_classification

dataset.bmi = bmi_record.numeric_value
dataset.bmi_date = bmi_record.date

##vaccination 
# first vaccine from during trials and up to treatment date
covid_vacc = (
    vaccinations.where((vaccinations.target_disease.is_in(["SARS-2 CORONAVIRUS"])) &
    (vaccinations.date.is_before(treat_date)) & (vaccinations.date.is_after("2020-06-08")))
    .sort_by(vaccinations.date)
)
# this will be replaced with distinct_count_for_patient() once it is developed
dataset.total_covid_vacc = covid_vacc.count_for_patient()

dataset.covid_vacc01 = covid_vacc.first_for_patient().date
dataset.covid_vacc02 = covid_vacc.where(covid_vacc.date.is_after(dataset.covid_vacc01 + days(19))
    ).sort_by(covid_vacc.date).first_for_patient().date

dataset.covid_vacc03 = covid_vacc.where(covid_vacc.date.is_after(dataset.covid_vacc02 + days(56))
    ).sort_by(covid_vacc.date).first_for_patient().date

dataset.covid_vacc04 = covid_vacc.where(covid_vacc.date.is_after(dataset.covid_vacc03 + days(56))
    ).sort_by(covid_vacc.date).first_for_patient().date

dataset.covid_vacc_last = covid_vacc.last_for_patient().date
dataset.last_vaccination_date = covid_vacc.last_for_patient().date

##### ############
non_hospital = (  
    covid_therapeutics_raw
    #.where(covid_therapeutics_raw.diagnosis.is_in(["Covid-19"]))
    .where(covid_therapeutics_raw.covid_indication.is_in(["non_hospitalised"])) #&
    #(covid_therapeutics_raw.current_status.is_in(["Approved", "Treatment Complete"])) ) #"Approved", "Treatment Complete")
    .sort_by(covid_therapeutics_raw.treatment_start_date) #current_status
)

first_molnupiravir = first_covid_therap_date(pre_dataset = non_hospital, covid_drug = "Molnupiravir")
dataset.first_molnupiravir_date = first_molnupiravir.treatment_start_date
dataset.first_molnupiravir_status = first_molnupiravir.current_status  #use approved/completed
dataset.first_molnupiravir_interve= first_molnupiravir.intervention
dataset.first_molnupiravir_diag = first_molnupiravir.diagnosis

first_sotrovimab = first_covid_therap_date(pre_dataset = non_hospital,covid_drug = "Sotrovimab")
dataset.first_sotrovimab_date = first_sotrovimab.treatment_start_date
dataset.first_sotrovimab_status = first_sotrovimab.current_status
dataset.first_sotrovimab_interve= first_sotrovimab.intervention
dataset.first_sotrovimab_diag = first_sotrovimab.diagnosis

##main outcome variables -hospital admission per primary_diagnosis
dataset.date_of_first_admis_af_treat = (
    apcs.where(apcs.admission_date.is_after(treat_date))
    .sort_by(apcs.admission_date).first_for_patient().admission_date
)

##hospitalisation as per primary_diagnosis == OUTCOME
hosp_af60d_covid_pdiag_1stdate_df = (  #covid_icd10_codes==>codelists.covid_icd10_codes
    apcs.where(
        (apcs.primary_diagnosis.is_in(covid_icd10_codes)) &    #primary_diagnosis
        (apcs.admission_date.is_after(treat_date + days(60))) &
        (apcs.admission_date.is_on_or_before(treat_date + days(60) + months(6))) & #  apcs.snomedct_code.is_in(covid_icd10_codes)
        (apcs.patient_classification.is_in(["1"]))   ## ordinary admissions only - exclude day cases and regular attenders
    ).sort_by(apcs.admission_date).first_for_patient()
)

##critical_care                    
ccare_af60d_covid_pdiag = (hosp_af60d_covid_pdiag_1stdate_df
    .admission_method.is_in(["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"]) &
    (hosp_af60d_covid_pdiag_1stdate_df.days_in_critical_care>0))

##hospitalisation as per primary_diagnosis
dataset.hosp_covid_date = hosp_af60d_covid_pdiag_1stdate_df.admission_date #hosp_af60d_covid_pdiag_1stdate_df
dataset.hosp_covid_classfic = hosp_af60d_covid_pdiag_1stdate_df.patient_classification #hosp_af60d_covid_pdiag_classfic
dataset.hosp_covid_pdiag = hosp_af60d_covid_pdiag_1stdate_df.primary_diagnosis #hosp_af60d_covid_pdiag
dataset.had_ccare_covid = ccare_af60d_covid_pdiag #had_ccare_covid_af60d_6mon_pdiag_date


##critical_care-date
dataset.ccare_covid_date = (  
    apcs.where(
        (apcs.primary_diagnosis.is_in(covid_icd10_codes)) &    #primary_diagnosis
        (apcs.admission_date.is_after(treat_date + days(60))) &
        (apcs.admission_date.is_on_or_before(treat_date + days(60) + months(6))) & #  apcs.snomedct_code.is_in(covid_icd10_codes)
        (apcs.patient_classification.is_in(["1"])) &
        (apcs.admission_method.is_in(["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"])) &
        (apcs.days_in_critical_care>0)
        ).sort_by(apcs.admission_date).first_for_patient()).admission_date

hosp_allcause_af60d_6mon_df = ( 
    apcs.where(
    (apcs.admission_date.is_after(treat_date + days(60))) &
    (apcs.admission_date.is_on_or_before (treat_date + days(60) + months(6))) &
    (apcs.patient_classification.is_in(["1"]))
    ).sort_by(apcs.admission_date).first_for_patient()
) 

dataset.hosp_allcause_date = hosp_allcause_af60d_6mon_df.admission_date #hosp_allcause_af60d_6mon_pdiag_date
dataset.hosp_allcause_classfic = hosp_allcause_af60d_6mon_df.patient_classification
dataset.hosp_allcause_pdiag = hosp_allcause_af60d_6mon_df.primary_diagnosis

hosp_covid_date = dataset.hosp_covid_date #first hospitalise after 60days
hosp_allcause_date = dataset.hosp_allcause_date 

dataset.hospitalise_disc_covid = ( #first discharge after 60days
    apcs.where(
        apcs.discharge_date.is_on_or_before(hosp_covid_date)
    ).sort_by(apcs.discharge_date).first_for_patient()
).discharge_date

dataset.hospitalise_disc_allcause = (      #allcause-first discharge after 60days
    apcs.where(
        apcs.discharge_date.is_on_or_before(hosp_allcause_date)
    ).sort_by(apcs.discharge_date).first_for_patient()
).discharge_date


dataset.ons_dead_date = ons_deaths.date
dataset.underly_deathcause = ons_deaths.underlying_cause_of_death 
dataset.death_cause_covid = cause_of_death_matches(covid_icd10_codes) 

dead_date_covid_treat = (ons_deaths.date.is_after(treat_date + days(60)) &
    (ons_deaths.date.is_on_or_before(treat_date + days(60) + months(6)))
)

dataset.ons_dead_tr60d_6mon_covid_treat = (ons_deaths.date.is_after(treat_date + days(60)) & 
    ons_deaths.date.is_on_or_before(treat_date + days(60) + months(6)) 
)


dataset.ons_dead_tr60d_6mon_covid_treat2 = (ons_deaths.date.is_after(treat_date + days(60)) & 
    ons_deaths.date.is_on_or_before(treat_date + days(60) + months(6)) & dataset.death_cause_covid
)

## Death of any cause
dataset.ons_dead_trstart_60d_covid_treat = (ons_deaths.date.is_after(treat_date) & 
    ons_deaths.date.is_on_or_before(treat_date + days(60)))

dataset.ons_dead_trstart_30d_covid_treat = (ons_deaths.date.is_after(treat_date) & 
    ons_deaths.date.is_on_or_before(treat_date + days(30))) 

#######comorbidities #######
#######tpp-clinical_events, apcs.admission_date,tpp-medications
c_events = clinical_events  #tpp-clinical_events
c_events_bf_treat = c_events.where(c_events.date.is_on_or_before(treat_date))
apcs_diags_bf_treat = apcs.where(apcs.admission_date.is_on_or_before(treat_date))
pcmeds_bf_treat = medications.where(medications.date.is_on_or_before(treat_date)) #prescribed medications in primary care.
pcmeds_bf_3m = medications.where(medications.date.is_on_or_between(treat_date - months(3),treat_date)) # primary care.
pcmeds_bf_6m = medications.where(medications.date.is_on_or_between(treat_date - months(6),treat_date))
pcmeds_bf_12m = medications.where(medications.date.is_on_or_between(treat_date - months(12),treat_date))
c_events_bf6m = c_events.where(c_events.date.is_on_or_between(treat_date - months(6),treat_date))

c_events_bf12m = c_events.where(c_events.date.is_on_or_between(treat_date - months(12),treat_date))
apcs_diags_bf12m = apcs.where(apcs.admission_date.is_on_or_between(treat_date - months(12), treat_date))

c_events_bf24m = c_events.where(c_events.date.is_on_or_between(treat_date - months(24),treat_date))
apcs_diags_bf24m = apcs.where(apcs.admission_date.is_on_or_between(treat_date - months(24),treat_date))

#dataset.diabetes = has_prev_event_sme(diabetes_codes) 
def had_meds_lastdate(codelist1,codelist2,dt=pcmeds_bf_treat, where=True):
    return(
    dt.where(where)
    .where(dt.dmd_code.is_in(codelist1)|dt.dmd_code.is_in(codelist2))
    .sort_by(dt.date).last_for_patient().date
)

def had_meds_count(codelist1,codelist2,dt=pcmeds_bf_treat, where=True):
    return(
    dt.where(where)
    .where(dt.dmd_code.is_in(codelist1)|dt.dmd_code.is_in(codelist2))
    .count_for_patient()
)

## Immune-mediated inflammatory disorders (IMID)
dataset.immunosuppresant_drugs_nhsd = had_meds_lastdate (codelist1=immunosuppresant_drugs_dmd_codes, 
    codelist2= immunosuppresant_drugs_snomed_codes, dt=pcmeds_bf_6m 
)
dataset.oral_steroid_drugs_nhsd = had_meds_lastdate (codelist1=oral_steroid_drugs_dmd_codes, 
    codelist2= oral_steroid_drugs_snomed_codes, dt=pcmeds_bf_12m
)

dataset.oral_steroid_drug_nhsd_3m_count=had_meds_count (codelist1=oral_steroid_drugs_dmd_codes, 
    codelist2=oral_steroid_drugs_snomed_codes, dt=pcmeds_bf_3m
)

dataset.oral_steroid_drug_nhsd_12m_count=had_meds_count (codelist1=oral_steroid_drugs_dmd_codes, 
    codelist2=oral_steroid_drugs_snomed_codes, dt=pcmeds_bf_12m
)

dataset.immunosuppresant_drugs_nhsd_ever=had_meds_lastdate (codelist1=immunosuppresant_drugs_dmd_codes,
    codelist2=immunosuppresant_drugs_snomed_codes, dt=pcmeds_bf_treat
)

dataset.oral_steroid_drugs_nhsd_ever=had_meds_lastdate(codelist1=oral_steroid_drugs_dmd_codes, 
    codelist2=oral_steroid_drugs_snomed_codes, dt=pcmeds_bf_treat
)

#### ###
def had_clinc_event_ctv3snome_lastdate (codelist, dt=c_events_bf_treat, code_type='ctv3', where=True):
       # Determine which code type to filter on
    if code_type == 'snomedct':
        code_field = dt.snomedct_code
    elif code_type == 'ctv3':       #elif code_type == 'ctv3':
        code_field = dt.ctv3_code
    return (
        dt.where(where)
        .where(code_field.is_in(codelist)& (dt.date.is_on_or_before(treat_date)))
        .sort_by(dt.date)
        .last_for_patient().date
)

def had_apcs_diag_icd10_lastdate(codelist, dt=apcs_diags_bf_treat, code_type='icd10_prim', where=True): #Currently only use for primary diagnosis
       # Determine which code type to filter on
    if code_type == 'icd10_prim':
        code_field = dt.primary_diagnosis
    elif code_type == 'icd10_sec':
        code_field = dt.secondary_diagnosis
    # else:
    #     print("Use 'ctv3' or 'snomedct'.")
    return (
    dt.where(where)
    .where(code_field.is_in(codelist) & (dt.admission_date.is_on_or_before(treat_date)))
    .sort_by(dt.admission_date)
    .last_for_patient().admission_date
)
###### #########
#solid_organ_transplant_nhsd_snomed_codes, solid_organ_transplant_nhsd_snomed_codes_new
#dataset.diabetes = had_clinc_event_ctv3snome_lastdate(diabetes_codes,code_type='snomedct') #tpp-clinical_events  #snomedct
#on_or_before = "start_date",
dataset.dialysis = had_clinc_event_ctv3snome_lastdate(dialysis_codes) #tpp-clinical_events  #CTV3ID
dataset.dialysis_icd10 = had_apcs_diag_icd10_lastdate(dialysis_icd10_codelist) #tpp-apcs

######################apcs_admis_alldiag_match######################

def apcs_proc_match(codelist):
    code_strings = set()
    for code in codelist:
            code_string = ICD10Code(code)._to_primitive_type()
            code_strings.add(code_string)
    conditions = [
        apcs.all_procedures.contains(code_str)
        for code_str in code_strings
    ]
    return apcs.where(any_of(conditions)
)

def apcs_proc_bf_treat_lastdate(codelist=None, where=True):
    return (
    (apcs_proc_match(codelist) if codelist else apcs)
    .where(apcs.admission_date.is_on_or_before(treat_date))
      # .where(apcs.patient_classification == "1")  #Ordinary admission 
    .where(where)
    .sort_by(apcs.admission_date)
    .last_for_patient()
    .admission_date
)

def apcs_proc_bf_treat_df(codelist=None, where=True): #return frame
    return (
    (apcs_proc_match(codelist) if codelist else apcs)
    .where(apcs.admission_date.is_on_or_before(treat_date))
    .where(where)
    .sort_by(apcs.admission_date)
)

def apcs_proc_bf_treat_af01Feb20_lastdate(codelist=None, where=True):
    return (
    (apcs_proc_match(codelist) if codelist else apcs)
    .where(apcs.admission_date.is_on_or_before(treat_date) & apcs.admission_date.is_on_or_after("2020-02-01"))
    .where(where).sort_by(apcs.admission_date).last_for_patient().admission_date
)

def apcs_proc_bf_treat_af01Feb20_df(codelist=None, where=True): ##get dataframe-apcs.admission_date
    return (
    (apcs_proc_match(codelist) if codelist else apcs)
    .where(apcs.admission_date.is_on_or_before(treat_date) & apcs.admission_date.is_on_or_after("2020-02-01"))
    .where(where).sort_by(apcs.admission_date)
)

#on_or_before="start_date"
dataset.dialysis_procedure = apcs_proc_bf_treat_lastdate(codelist= dialysis_opcs4_codelist) ##use fun:apcs_proc_bf_treat_lastdate
##kidney transplant
dataset.kidney_transplant = had_clinc_event_ctv3snome_lastdate(codelist=kidney_transplant_codes) #tpp-clinical_events #CTV3ID
dataset.kidney_transplant_icd10 = had_apcs_diag_icd10_lastdate (codelist=kidney_tx_icd10_codelist)

#kidney_transplant_procedure 
dataset.kidney_transplant_procedure = apcs_proc_bf_treat_lastdate(codelist= kidney_tx_opcs4_codelist)

##Solid organ transplant
dataset.solid_organ_transplant_nhsd_snomed = had_clinc_event_ctv3snome_lastdate(
    codelist = solid_organ_transplant_nhsd_snomed_codes, code_type='snomedct') #tpp-clinical_events  #snomedct
dataset.solid_organ_nhsd_snomed_new = had_clinc_event_ctv3snome_lastdate(
    codelist = solid_organ_transplant_nhsd_snomed_codes_new, code_type='snomedct')#tpp-clinical_events  #snomedct

dataset.solid_organ_transplant_nhsd_opcs4 =apcs_proc_bf_treat_af01Feb20_lastdate(codelist= solid_organ_transplant_nhsd_opcs4_codes)

dataset.transplant_all_y_codes_opcs4 =apcs_proc_bf_treat_af01Feb20_lastdate(codelist=replacement_of_organ_transplant_nhsd_opcs4_codes)

transplant_all_y_codes_opcs4_df =apcs_proc_bf_treat_af01Feb20_df(codelist=replacement_of_organ_transplant_nhsd_opcs4_codes)

dataset.transplant_all_y_codes_opcs4_count =apcs_proc_bf_treat_af01Feb20_df(codelist=replacement_of_organ_transplant_nhsd_opcs4_codes).count_for_patient()
#transplant_thymus_opcs4_df = apcs_proc_bf_treat_af01Feb20_df(codelist=thymus_gland_transplant_nhsd_opcs4_codes)

##between = ["transplant_all_y_codes_opcs4","transplant_all_y_codes_opcs4"],#is_on_or_before(treat_date)#"earliest": "2020-02-01
transplant_thymus_opcs4 = apcs_proc_bf_treat_af01Feb20_lastdate(codelist=thymus_gland_transplant_nhsd_opcs4_codes)

#dataset.transplant_thymus_opcs4_3 = transplant_thymus_opcs4(transplant_thymus_opcs4.is_on_or_between(transplant_all_y_codes_opcs4,transplant_all_y_codes_opcs4).last_for_patient()).admission_date
dataset.transplant_thymus_opcs4 = apcs_proc_bf_treat_af01Feb20_lastdate(codelist=thymus_gland_transplant_nhsd_opcs4_codes)
dataset.transplant_thymus_opcs4_count = apcs_proc_bf_treat_af01Feb20_df(codelist=thymus_gland_transplant_nhsd_opcs4_codes).count_for_patient()

dataset.transplant_thymus_opcs4_a =dataset.transplant_thymus_opcs4.is_on_or_between(dataset.transplant_all_y_codes_opcs4,dataset.transplant_all_y_codes_opcs4)
#dataset.transplant_thymus_opcs4_4 = dataset.transplant_thymus_opcs4.is_in(transplant_all_y_codes_opcs4_df)#apcs_proc_bf_treat_af01Feb20_df


dataset.transplant_conjunctiva_y_code_opcs4 = apcs_proc_bf_treat_af01Feb20_lastdate(codelist=conjunctiva_y_codes_transplant_nhsd_opcs4_codes)
dataset.transplant_conjunctiva_y_code_opcs4_count = apcs_proc_bf_treat_af01Feb20_df(codelist=conjunctiva_y_codes_transplant_nhsd_opcs4_codes).count_for_patient()
transplant_conjunctiva_y_code_opcs4_df = apcs_proc_bf_treat_af01Feb20_df(codelist=conjunctiva_y_codes_transplant_nhsd_opcs4_codes)

#between = ["transplant_conjunctiva_y_code_opcs4","transplant_conjunctiva_y_code_opcs4"],
dataset.transplant_conjunctiva_opcs4 =apcs_proc_bf_treat_af01Feb20_lastdate(codelist= conjunctiva_transplant_nhsd_opcs4_codes)
dataset.transplant_conjunctiva_opcs4_count =apcs_proc_bf_treat_af01Feb20_df(codelist= conjunctiva_transplant_nhsd_opcs4_codes).count_for_patient()
dataset.transplant_conjunctiva_opcs4_a =dataset.transplant_conjunctiva_opcs4.is_on_or_between(
    dataset.transplant_conjunctiva_y_code_opcs4,dataset.transplant_conjunctiva_y_code_opcs4)

# between = ["transplant_all_y_codes_opcs4","transplant_all_y_codes_opcs4"],
dataset.transplant_stomach_opcs4 = apcs_proc_bf_treat_af01Feb20_lastdate(codelist=stomach_transplant_nhsd_opcs4_codes)
dataset.transplant_stomach_opcs4_count =apcs_proc_bf_treat_af01Feb20_df(codelist=stomach_transplant_nhsd_opcs4_codes).count_for_patient()
dataset.transplant_stomach_opcs4_a = dataset.transplant_stomach_opcs4.is_on_or_between(dataset.transplant_all_y_codes_opcs4,dataset.transplant_all_y_codes_opcs4) 


dataset.transplant_ileum_1_Y_codes_opcs4 = apcs_proc_bf_treat_af01Feb20_lastdate(codelist=ileum_1_y_codes_transplant_nhsd_opcs4_codes)
dataset.transplant_ileum_1_Y_codes_opcs4_count = apcs_proc_bf_treat_af01Feb20_df(codelist=ileum_1_y_codes_transplant_nhsd_opcs4_codes).count_for_patient()
transplant_ileum_1_Y_codes_opcs4_df = apcs_proc_bf_treat_af01Feb20_df(codelist=ileum_1_y_codes_transplant_nhsd_opcs4_codes)

#between = ["transplant_ileum_1_Y_codes_opcs4","transplant_ileum_1_Y_codes_opcs4"],
dataset.transplant_ileum_1_opcs4 = apcs_proc_bf_treat_af01Feb20_lastdate(codelist=ileum_1_transplant_nhsd_opcs4_codes) #not defined by transplant_ileum_1_Y_codes_opcs4
dataset.transplant_ileum_1_opcs4_count = apcs_proc_bf_treat_af01Feb20_df(codelist=ileum_1_transplant_nhsd_opcs4_codes).count_for_patient() #
dataset.transplant_ileum_1_opcs4_a = dataset.transplant_ileum_1_opcs4.is_on_or_between(dataset.transplant_ileum_1_Y_codes_opcs4,dataset.transplant_ileum_1_Y_codes_opcs4) 

dataset.transplant_ileum_2_Y_codes_opcs4 = apcs_proc_bf_treat_af01Feb20_lastdate(codelist= ileum_2_y_codes_transplant_nhsd_opcs4_codes)
dataset.transplant_ileum_2_Y_codes_opcs4_count= apcs_proc_bf_treat_af01Feb20_df(codelist=ileum_2_y_codes_transplant_nhsd_opcs4_codes).count_for_patient()
transplant_ileum_2_Y_codes_opcs4_df= apcs_proc_bf_treat_af01Feb20_df(codelist=ileum_2_y_codes_transplant_nhsd_opcs4_codes)

#between = ["transplant_ileum_2_Y_codes_opcs4","transplant_ileum_2_Y_codes_opcs4"],
#"date": {"earliest": "2020-02-01"}
dataset.transplant_ileum_2_opcs4 = apcs_proc_bf_treat_af01Feb20_lastdate(codelist=ileum_2_transplant_nhsd_opcs4_codes)
dataset.transplant_ileum_2_opcs4_count = apcs_proc_bf_treat_af01Feb20_df(codelist=ileum_2_transplant_nhsd_opcs4_codes).count_for_patient()
dataset.transplant_ileum_2_opcs4_a = dataset.transplant_ileum_2_opcs4.is_on_or_between(dataset.transplant_ileum_2_Y_codes_opcs4,dataset.transplant_ileum_2_Y_codes_opcs4) 

def proc_match(codelist, dt):
    code_strings = set()
    for code in codelist:
        # try:
            code_string = ICD10Code(code)._to_primitive_type()
            code_strings.add(code_string)
    conditions = [
        dt.all_procedures.contains(code_str)
        for code_str in code_strings
    ]
    return dt.where(any_of(conditions))

def proc_bf_treat_af01Feb20_lastdate(dt, codelist=None, where=True):
    matched_data = proc_match(codelist, dt) if codelist else dt
    return (
        matched_data
        .where(dt.admission_date.is_on_or_before(treat_date) & dt.admission_date.is_on_or_after("2020-02-01"))
        .where(where).sort_by(dt.admission_date).last_for_patient().admission_date
    )

dataset.transplant_thymus_opcs4_2 = proc_bf_treat_af01Feb20_lastdate(
    dt=transplant_all_y_codes_opcs4_df, codelist=thymus_gland_transplant_nhsd_opcs4_codes
)

dataset.transplant_conjunctiva_opcs4_2 = proc_bf_treat_af01Feb20_lastdate(
    dt=transplant_conjunctiva_y_code_opcs4_df, codelist=conjunctiva_transplant_nhsd_opcs4_codes
)
dataset.transplant_stomach_opcs4_2 = proc_bf_treat_af01Feb20_lastdate(
    dt=transplant_all_y_codes_opcs4_df, codelist=stomach_transplant_nhsd_opcs4_codes
)
dataset.transplant_ileum_1_opcs4_2 = proc_bf_treat_af01Feb20_lastdate(
    dt=transplant_ileum_1_Y_codes_opcs4_df, codelist=ileum_1_transplant_nhsd_opcs4_codes
)
dataset.transplant_ileum_2_opcs4_2 = proc_bf_treat_af01Feb20_lastdate(
    dt=transplant_ileum_2_Y_codes_opcs4_df, codelist=ileum_2_transplant_nhsd_opcs4_codes
)

dataset.solid_organ_transplant_nhsd = minimum_of (
    dataset.solid_organ_transplant_nhsd_snomed, dataset.solid_organ_transplant_nhsd_opcs4,
    dataset.transplant_thymus_opcs4_2 , dataset.transplant_conjunctiva_opcs4_2, dataset.transplant_stomach_opcs4_2,
    dataset.transplant_ileum_1_opcs4_2,dataset.transplant_ileum_2_opcs4_2
)

dataset.solid_organ_transplant_nhsd_new = minimum_of (
    dataset.solid_organ_nhsd_snomed_new, dataset.solid_organ_transplant_nhsd_opcs4,
    dataset.transplant_thymus_opcs4_2 , dataset.transplant_conjunctiva_opcs4_2, dataset.transplant_stomach_opcs4_2,
    dataset.transplant_ileum_1_opcs4_2, dataset.transplant_ileum_2_opcs4_2
) 

## Haematological diseases-between = ["start_date - 24 months", "start_date"],
#c_events_bf12m,apcs_diags_bf12m, #c_events_bf24m,apcs_diags_bf24m

##between = ["start_date - 12 months", "start_date"],
dataset.haematopoietic_stem_cell_snomed = had_clinc_event_ctv3snome_lastdate (dt = c_events_bf12m, 
    codelist = haematopoietic_stem_cell_transplant_nhsd_snomed_codes, code_type = 'snomedct'
) #tpp-clinical_events  #snomedct

#between = ["start_date - 12 months", "start_date"],
dataset.haematopoietic_stem_cell_icd10  = had_apcs_diag_icd10_lastdate (dt = apcs_diags_bf12m, 
    codelist = haematopoietic_stem_cell_transplant_nhsd_icd10_codes
) ##tpp-apcs

def apcs_proc_12m_bf_treat_af01Feb20_lastdate (codelist=None, where=True):  #is_on_or_between(start, end) 
    return (
    (apcs_proc_match(codelist) if codelist else apcs)
    .where(apcs.admission_date.is_on_or_between(treat_date- months(12),treat_date) & \
    apcs.admission_date.is_on_or_after("2020-02-01"))
    .where(where).sort_by(apcs.admission_date).last_for_patient().admission_date
)

# between = ["start_date - 12 months", "start_date"], #"date": {"earliest": "2020-02-01"},
dataset.haematopoietic_stem_cell_opcs4 = apcs_proc_12m_bf_treat_af01Feb20_lastdate(
    codelist=haematopoietic_stem_cell_transplant_nhsd_opcs4_codes) 

# between = ["start_date - 24 months", "start_date"],
dataset.haematological_malignancies_snomed = had_clinc_event_ctv3snome_lastdate(dt = c_events_bf24m, 
    codelist = haematological_malignancies_nhsd_snomed_codes, code_type = 'snomedct') #tpp-clinical_events  #snomedct

# between = ["start_date - 24 months", "start_date"],
dataset.haematological_malignancies_icd10 = had_apcs_diag_icd10_lastdate(dt = apcs_diags_bf24m, 
    codelist = haematological_malignancies_nhsd_icd10_codes) ##tpp-apcs

#on_or_before = "start_date",
dataset.sickle_cell_disease_nhsd_snomed = had_clinc_event_ctv3snome_lastdate(
    codelist = sickle_cell_disease_nhsd_snomed_codes, code_type = 'snomedct') #tpp-clinical_events  #snomedct
#on_or_before = "start_date",
dataset.sickle_cell_disease_nhsd_icd10  = had_apcs_diag_icd10_lastdate(codelist = sickle_cell_disease_nhsd_icd10_codes) 

## Haematological diseases-ever(on_or_before = "start_date")
dataset.haematopoietic_stem_cell_snomed_ever = had_clinc_event_ctv3snome_lastdate(
    codelist = haematopoietic_stem_cell_transplant_nhsd_snomed_codes, code_type = 'snomedct') #tpp-clinical_events  #snomedct
#on_or_before = "start_date",
dataset.haematopoietic_stem_cell_icd10_ever = had_apcs_diag_icd10_lastdate(
    codelist = haematopoietic_stem_cell_transplant_nhsd_icd10_codes) ##tpp-apcs

##on_or_before = "start_date", "earliest": "2020-02-01"},
dataset.haematopoietic_stem_cell_opcs4_ever = apcs_proc_bf_treat_af01Feb20_lastdate(
    codelist=haematopoietic_stem_cell_transplant_nhsd_opcs4_codes)
                          
#on_or_before = "start_date"
dataset.haematological_malignancies_snomed_ever = had_clinc_event_ctv3snome_lastdate(
    codelist = haematological_malignancies_nhsd_snomed_codes, 
        code_type='snomedct') #tpp-clinical_events  #snomedct
#on_or_before = "start_date"
dataset.haematological_malignancies_icd10_ever = had_apcs_diag_icd10_lastdate(
    codelist = haematological_malignancies_nhsd_icd10_codes) ##tpp-apcs

##minimum_of
dataset.haematological_disease_nhsd_ever = minimum_of(
    dataset.haematopoietic_stem_cell_snomed_ever, 
    dataset.haematopoietic_stem_cell_icd10_ever, 
    dataset.haematopoietic_stem_cell_opcs4_ever, 
    dataset.haematological_malignancies_snomed_ever, 
    dataset.haematological_malignancies_icd10_ever,
    dataset.sickle_cell_disease_nhsd_snomed, 
    dataset.sickle_cell_disease_nhsd_icd10)


#on_or_before = "start_date",
##Primary immune deficiencies
dataset.immunosupression_nhsd = had_clinc_event_ctv3snome_lastdate(codelist=immunosupression_nhsd_codes, code_type='snomedct') #tpp-clinical_events  #snomedct
#on_or_before = "start_date",
dataset.immunosupression_nhsd_new  = had_clinc_event_ctv3snome_lastdate(codelist=immunosupression_nhsd_codes_new, code_type='snomedct') #tpp-clinical_events  #snomedct

#immunosuppresant_drugs_dmd_codes, immunosuppresant_drugs_snomed_codes

### Solid cancer
dataset.cancer_opensafely_snomed= c_events_bf6m.where(
        (c_events_bf6m.snomedct_code.is_in(non_haematological_cancer_opensafely_snomed_codes)) | \
        (c_events_bf6m.snomedct_code.is_in(lung_cancer_opensafely_snomed_codes)) | \
        (c_events_bf6m.snomedct_code.is_in(chemotherapy_radiotherapy_opensafely_snomed_codes))) \
        .sort_by(c_events_bf6m.date).last_for_patient().date

dataset.cancer_opensafely_snomed_new= c_events_bf6m.where(
        (c_events_bf6m.snomedct_code.is_in(non_haematological_cancer_opensafely_snomed_codes_new)) | \
        (c_events_bf6m.snomedct_code.is_in(lung_cancer_opensafely_snomed_codes)) | \
        (c_events_bf6m.snomedct_code.is_in(chemotherapy_radiotherapy_opensafely_snomed_codes))) \
        .sort_by(c_events_bf6m.date).last_for_patient().date

dataset.cancer_opensafely_snomed_ever= c_events_bf_treat.where(
        (c_events_bf_treat.snomedct_code.is_in(non_haematological_cancer_opensafely_snomed_codes_new)) | \
        (c_events_bf_treat.snomedct_code.is_in(lung_cancer_opensafely_snomed_codes)) | \
        (c_events_bf_treat.snomedct_code.is_in(chemotherapy_radiotherapy_opensafely_snomed_codes))) \
        .sort_by(c_events_bf_treat.date).last_for_patient().date


# # List of diseases
# highrisk_list = [
#     "Downs syndrome", "HIV", "AIDS", "IMID", "haematologic malignancy",
#     "Patients with a haematological diseases", "immune deficiencies", "liver disease",
#     "primary immune deficiencies", "rare neurological conditions",
#     "rare neurological diseases", "renal disease", "sickle cell disease",
#     "solid cancer", "solid organ recipients", "stem cell transplant recipient"
# ]

dataset.high_risk_covid_thera_MOL_count = non_hospital.MOL1_high_risk_cohort.count_distinct_for_patient()  #exists_for_patient()  
dataset.high_risk_covid_thera_MOL_count_dist = non_hospital.where(non_hospital.treatment_start_date.is_on_or_between(treat_date,treat_date)
    ).MOL1_high_risk_cohort.count_distinct_for_patient() 
#dataset.high_risk_cohort_covid_therapeutics_MOL_exist = non_hospital.MOL1_high_risk_cohort.exists_for_patient()  
high_risk_covid_thera_MOL_first = non_hospital.where(non_hospital.treatment_start_date.is_on_or_between(treat_date,treat_date)).first_for_patient().MOL1_high_risk_cohort

dataset.high_risk_covid_thera_SOT02_count_dist = non_hospital.where(non_hospital.treatment_start_date.is_on_or_between(treat_date,treat_date)
    ).SOT02_risk_cohorts.count_distinct_for_patient() 

dataset.high_risk_covid_thera_CASIM05_count_dist = non_hospital.where(non_hospital.treatment_start_date.is_on_or_between(treat_date,treat_date)
    ).CASIM05_risk_cohort.count_distinct_for_patient() 


#####Pregnancy#####
### pregnancy record in last 36 weeks
dataset.preg_36wks_date = (
    c_events_bf_treat.where((c_events_bf_treat.snomedct_code.is_in(pregnancy_primis_codes)) & \
    (c_events_bf_treat.date.is_on_or_before(treat_date)) & \
        (c_events_bf_treat.date.is_on_or_between(treat_date - days(252), treat_date - days(1)))) #clinical_events.snomedct_code.is_in(ethnicity_codelist)
        .sort_by(c_events_bf_treat.date)
        .last_for_patient().date
)

# dataset.preg_36wks_date  = preg_36wks_date
# #   # pregnancy OR delivery code since latest pregnancy record:
# #   # if one of these codes occurs later than the latest pregnancy code
# #   #  this indicates pregnancy has ended, if they are same date assume 
# #   #  pregnancy has most likely not ended yet

dataset.is_pregdel = (
    c_events_bf_treat.where((c_events_bf_treat.snomedct_code.is_in(pregdel_primis_codes)) & \
    (c_events_bf_treat.date.is_on_or_before(treat_date)) & \
        (c_events_bf_treat.date.is_on_or_between(dataset.preg_36wks_date + days(1), treat_date - days(1)))) #clinical_events.snomedct_code.is_in(ethnicity_codelist)
        .sort_by(c_events_bf_treat.date)
        .last_for_patient().exists_for_patient()    #.date
)

is_pregdel = (
    c_events_bf_treat.where((c_events_bf_treat.snomedct_code.is_in(pregdel_primis_codes)) & \
    (c_events_bf_treat.date.is_on_or_before(treat_date)) & \
        (c_events_bf_treat.date.is_on_or_between(dataset.preg_36wks_date + days(1), treat_date - days(1)))) #clinical_events.snomedct_code.is_in(ethnicity_codelist)
        .sort_by(c_events_bf_treat.date)
        .last_for_patient().exists_for_patient()    #.date
)
dataset.pregnancy = case(
    when((patients.age_on(dataset.preg_36wks_date) <=50 ) & (patients.sex.is_in(["female"])) & (~(dataset.is_pregdel))).then("preg"),
    otherwise="Not preg",
)

###### all_diagnoses-admission ######
#from ehrql.codes import CTV3Code, ICD10Code
def apcs_admis_alldiag_match(codelist):  #from-<comparative_booster_spring_2023>-----
    code_strings = set()
    for code in codelist:
        code_string = ICD10Code(code)._to_primitive_type()
        code_strings.add(code_string)
        conditions = [apcs.all_diagnoses.contains(code_str)  #all_diagnoses
        for code_str in code_strings]
    return apcs.where(any_of(conditions)
)

def apcs_admis_af_treat_alldiag_firstdate(codelist=None, where=True):
    return (
    (apcs_admis_alldiag_match(codelist) if codelist else apcs)
    .where(apcs.admission_date.is_on_or_after(treat_date))
    .where(apcs.patient_classification == "1")
    .where(where)
    .sort_by(apcs.admission_date)
    .first_for_patient()
    .admission_date
)

def apcs_admis_60daf_treat_alldiag_firstdate(codelist=None, where=True): #60days after treatment
    return (
    (apcs_admis_alldiag_match(codelist) if codelist else apcs)
    .where((apcs.admission_date.is_on_or_after(treat_date + days(60))) &
           (apcs.admission_date.is_on_or_before(treat_date + days(60) + months(6))))
    .where(apcs.patient_classification == "1")
    .where(where)
    .sort_by(apcs.admission_date)
    .first_for_patient()
    .admission_date
)

##hospitalisation as per all_diagnosis
# covid-related admission
dataset.covid_first_admi_af_treat_alldiag_firstdate_ = apcs_admis_af_treat_alldiag_firstdate(
    codelist= covid_icd10_codes,   
)

dataset.apcs_admis_60daf_treat_alldiag_firstdate=apcs_admis_60daf_treat_alldiag_firstdate(
    codelist= covid_icd10_codes,   
)

# covid-related admission to critical care
dataset.ccare_covid_first_af_treat_alldiag_date = apcs_admis_af_treat_alldiag_firstdate(codelist= covid_icd10_codes,
    where=(apcs.admission_method.is_in(
    ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"]
    )) & (apcs.days_in_critical_care>0),
)


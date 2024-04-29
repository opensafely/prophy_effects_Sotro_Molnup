#def_funs
from ehrql.tables.tpp import ( 
    addresses,
    clinical_events,
    apcs,
    medications,
    patients,
    practice_registrations,
    ons_deaths,
   )

from ehrql import (
    days,
    years,
    months,
    create_dataset,
    days,
)


#from ehrql.tables.raw.tpp import (
from ehrql.tables.raw.tpp import (
    covid_therapeutics_raw,
    isaric,
)


index_date = "2021-12-16"
index_date_end = "2022-02-10"
is_fem_male = patients.sex.is_in(["female", "male"])

index_date = "2021-12-16"
is_alive = (
    patients.date_of_death.is_after(index_date)
    | patients.date_of_death.is_null()
)

is_registered = practice_registrations.spanning (  
    index_date, index_date_end
).exists_for_patient()


def get_registration_status(index_date):
    return practice_registration_as_of(index_date).exists_for_patient() 


bmi_record = (
    clinical_events.where(
        clinical_events.snomedct_code.is_in(["60621009", "846931000000101"])
        # Ignore out-of-range values
        & (clinical_events.numeric_value > 4)
        & (clinical_events.numeric_value < 200)
        & (clinical_events.date >= patients.date_of_birth + years(16))
    )
    .sort_by(clinical_events.date)
    .last_for_patient()
)

def first_covid_therap_date(pre_dataset,covid_drug,start_date="2021-12-16",end_date="2022-02-10"):
    covid_treat=(
        pre_dataset
        .where(pre_dataset.intervention.is_in([covid_drug]) )
        .sort_by(pre_dataset.treatment_start_date)
    )
#Approved, Treatment Complete, Treatment Not Started, Treatment Stopped
    first_covid_therap = covid_treat.where(
    (covid_treat.treatment_start_date>=start_date) & 
    (covid_treat.treatment_start_date<=end_date) &
    (covid_treat.current_status.is_in(["Approved", "Treatment Complete"]))
    ).first_for_patient()

    return(
        first_covid_therap
        )

import operator
from functools import reduce

def any_of(conditions):
    return reduce(operator.or_, conditions)

# query if causes of death match a given codelist
def cause_of_death_matches(codelist):
    conditions = [
        getattr(ons_deaths, column_name).is_in(codelist)
        for column_name in (["underlying_cause_of_death"]+[f"cause_of_death_{i:02d}" for i in range(1, 16)])
    ]

    return any_of(conditions) 

def has_prev_event(codelist, where=True):
    return (
        prev_events.where(where)
        .where(prev_events.ctv3_code.is_in(codelist))
        .sort_by(prev_events.date)
        .last_for_patient().date
    )


def has_prev_event_numeric(codelist, where=True):
    prev_events_exists = prev_events.where(where) \
        .where(prev_events.ctv3_code.is_in(codelist)) \
        .exists_for_patient()
    return (
        case(
            when(prev_events_exists).then(1),
            when(~prev_events_exists).then(0)
            )
    )

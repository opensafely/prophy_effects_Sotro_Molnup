
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
library('ggpubr')

## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)

#high_risk_surv_data<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort.csv") %>%
high_risk_surv_data <- read_csv(here::here("output", "data", "high_risk_cohort.csv")) %>%
    select(patient_id, age_treated, imd, imd_num, drug, bmi, bmi_cat_num, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, 
    surv_event_num, surv_event_underly, calendar_day,calendar_wk, surv_days, surv_from_treat_days, surv_event, high_risk_num, diabetes, hypertension, chronic_cardiac_disease,
    chronic_respiratory_disease, autism, learning_disability, serious_mental_illness, dementia) %>% #age_treated_spline, calendar_day_spline, 
    mutate(age_treated_spline = ns(age_treated, df = 4),
    calendar_day_spline = ns(calendar_day, df = 4))%>% 
    mutate(across(c(imd_num, drug, bmi_cat_num, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, high_risk_num, diabetes,hypertension,chronic_cardiac_disease,
    chronic_respiratory_disease, autism, learning_disability, serious_mental_illness, dementia), as.factor))

str(high_risk_surv_data, list.len = ncol(high_risk_surv_data), give.attr= F)

cat("#freq_single(high_risk_surv_data$ethnicity_num)")
freq_single(high_risk_surv_data$ethnicity_num)
##variables <- c("age_treat_gp_rc", "sex", "sex_num","surv_event", "ethnicity", "region", "total_covid_vacc_cat", "first_covid_treat_interve")
##variables2 <- c("imd", "first_covid_treat_interve","high_risk_group")
#####################################################################################
#surv_days,surv_from_treat_days,surv_event
cat("#surv_days-high_risk_surv_data:")
mean(as.numeric(high_risk_surv_data$surv_days),na.rm=T)
cat("#surv_days-sd-high_risk_surv_data:")
sd(as.numeric(high_risk_surv_data$surv_days),na.rm=T)
cat("#surv_days-IQR-high_risk_surv_data:")
IQR(as.numeric(high_risk_surv_data$surv_days), na.rm=T)

cat("##surv_days-sum(is.na):")
sum(is.na(high_risk_surv_data$surv_days))
cat("##surv_days-sum(!is.na):")
sum(!is.na(high_risk_surv_data$surv_days))

cat("#surv_days-sum(is.na):")
sum(is.na(high_risk_surv_data$surv_from_treat_days))
cat("#surv_days-sum(!is.na):")
sum(!is.na(high_risk_surv_data$surv_from_treat_days))

cat("#sum(is.na(surv_event)):")
sum(is.na(high_risk_surv_data$surv_event))
cat("#sum(!is.na(surv_event)):")
sum(!is.na(high_risk_surv_data$surv_event))

cat("#sum(is.na(drug)):")
sum(is.na(high_risk_surv_data$drug))
cat("#sum(!is.na(drug)):")
sum(!is.na(high_risk_surv_data$drug))

cat("#sum(is.na(stp)):")
sum(is.na(high_risk_surv_data$stp))
cat("#sum(!is.na(stp)):")
sum(!is.na(high_risk_surv_data$stp))

#surv_days,surv_from_treat_days,surv_event
cat("#surv_from_treat_days-high_risk_surv_data:")
mean(as.numeric(high_risk_surv_data$surv_from_treat_days),na.rm=T)
cat("#surv_days-sd-high_risk_surv_data:")
sd(as.numeric(high_risk_surv_data$surv_from_treat_days),na.rm=T)
cat("#surv_days-IQR-high_risk_surv_data:")
IQR(as.numeric(high_risk_surv_data$surv_from_treat_days), na.rm=T)
####################################################################
####################################################################
# Define covariates and the stratification variable


# Create formulas for stratified Cox models
# forma_univ <- sapply(covariates, function(x) as.formula(paste('Surv(surv_days, surv_event_num) ~', x, '+ strata(', stp, ')')))

# # Fit stratified Cox models
# univ_models <- lapply(forma_univ, function(x) { coxph(x, data = high_risk_surv_data) })

# # Extract and format results
# univ_results <- lapply(univ_models, function(x) { 
#   x <- summary(x)
#   p.value <- signif(x$wald["pvalue"], digits=2)
#   wald.test <- signif(x$wald["test"], digits=2)
#   beta <- signif(x$coef[1], digits=2)
#   HR <- signif(x$coef[2], digits=2)
#   HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
#   HR.confint.upper <- signif(x$conf.int[,"upper .95"], 2)
#   HR <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
#   res <- c(beta, HR, wald.test, p.value)
#   names(res) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")
#   return(res)
# })

############################################################################
###########################################################################

cat("#summary(cox_model0)")
cox_model0 <- coxph(Surv(surv_days, surv_event_num) ~ (drug), data = high_risk_surv_data)
summary(cox_model0)

cat("#summary(cox_model_strata(stp))")
cox_model <- coxph(Surv(surv_days, surv_event_num) ~ (drug)+ strata(stp), data = high_risk_surv_data)
summary(cox_model)

table(high_risk_surv_data$drug, high_risk_surv_data$surv_event_num)
table(high_risk_surv_data$stp, high_risk_surv_data$surv_event_num)

# #age_treated, sex_num
cat("#summary(cox_model1_age_sex )")
cox_model1_age_sex <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num+ strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex)

covariates <- c("age_treated", "sex_num")
# Create formulas for stratified Cox models
forma_univ <- sapply(covariates, function(x) as.formula(paste('Surv(surv_days, surv_event_num) ~', x, '+ strata(stp)')))

# Fit stratified Cox models
univ_models <- lapply(forma_univ, function(x) { coxph(x, data = high_risk_surv_data) })

# Extract and format results
univ_results <- lapply(univ_models, function(x) { 
  x <- summary(x)
  p.value <- signif(x$wald["pvalue"], digits=2)
  wald.test <- signif(x$wald["test"], digits=2)
  beta <- signif(x$coef[1], digits=2)
  HR <- signif(x$coef[2], digits=2)
  HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
  HR.confint.upper <- signif(x$conf.int[,"upper .95"], 2)
  HR <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
  res <- c(beta, HR, wald.test, p.value)
  names(res) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")
  return(res)
})
# Combine results into a data frame
res_age_sex  <- t(as.data.frame(univ_results, check.names = FALSE))
res_age_sex_df <- as.data.frame(res_age_sex)
# Print the results
cat("#print(res_age_sex_df)")
print(res_age_sex_df)

cat("#summary(cox_model1_age_sex_highrisk )")
cox_model1_age_sex_highrisk <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num + high_risk_num + strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk)

####

covariates <- c("age_treated", "sex_num", "high_risk_num")
# Create formulas for stratified Cox models
forma_univ <- sapply(covariates, function(x) as.formula(paste('Surv(surv_days, surv_event_num) ~', x, '+ strata(stp)')))

# Fit stratified Cox models
univ_models <- lapply(forma_univ, function(x) { coxph(x, data = high_risk_surv_data) })

# # Extract and format results
# univ_results <- lapply(univ_models, function(x) { 
#   x <- summary(x)
#   p.value <- signif(x$wald["pvalue"], digits=2)
#   wald.test <- signif(x$wald["test"], digits=2)
#   beta <- signif(x$coef[1], digits=2)
#   HR <- signif(x$coef[2], digits=2)
#   HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
#   HR.confint.upper <- signif(x$conf.int[,"upper .95"], 2)
#   HR <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
#   res <- c(beta, HR, wald.test, p.value)
#   names(res) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")
#   return(res)
# })
# # Combine results into a data frame
# res_age_sex_highrisk <- t(as.data.frame(univ_results, check.names = FALSE))
# res_age_sex_highrisk_df <- as.data.frame(res_age_sex_highrisk)
# # Print the results
# cat("#print(res_age_sex_highrisk_df)")
# print(res_age_sex_highrisk_df)
###################
###################
#################this code -work-START#########################
# Create a function to handle the summary extraction safely
safe_summary <- function(model) {
  x <- summary(model)
   # Check if the model returned coefficients
  if (is.null(x$coef)) {
    return(rep(NA, 4))  # Return a vector of NAs if no coefficients are present
  }  
  p.value <- signif(x$wald["pvalue"], digits=2)
  wald.test <- signif(x$wald["test"], digits=2)
  beta <- signif(x$coef[1], digits=2)
  HR <- signif(x$coef[2], digits=2)
  HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
  HR.confint.upper <- signif(x$conf.int[,"upper .95"], 2)
  HR <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
  res <- c(beta, HR, wald.test, p.value)
  names(res) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")
  return(res)
}

# Apply the safe_summary function to each model
univ_results <- lapply(univ_models, safe_summary)

# Ensure all results have the same length (if necessary, pad with NAs)
max_length <- max(sapply(univ_results, length))
univ_results <- lapply(univ_results, function(res) {
  length(res) <- max_length  # Pad with NAs to ensure consistent length
  return(res)
})

# Convert to a data frame
res_age_sex_highrisk <- t(as.data.frame(univ_results, check.names = FALSE))
res_age_sex_highrisk <- as.data.frame(res_age_sex_highrisk)
names(res_age_sex_highrisk) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")

# Print the result
print(res_age_sex_highrisk)

##################
##################

cat("#summary(cox_model1_age_sex_highrisk_vacc )")
cox_model1_age_sex_highrisk_vacc <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num + high_risk_num + covid_vacc_num + strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk_vacc)

covariates <- c("age_treated", "sex_num", "high_risk_num", "covid_vacc_num")
# Create formulas for stratified Cox models
forma_univ <- sapply(covariates, function(x) as.formula(paste('Surv(surv_days, surv_event_num) ~', x, '+ strata(stp)')))
# Fit stratified Cox models
univ_models <- lapply(forma_univ, function(x) { coxph(x, data = high_risk_surv_data) })

# Apply the safe_summary function to each model
univ_results <- lapply(univ_models, safe_summary)

# Ensure all results have the same length (if necessary, pad with NAs)
max_length <- max(sapply(univ_results, length))
univ_results <- lapply(univ_results, function(res) {
  length(res) <- max_length  # Pad with NAs to ensure consistent length
  return(res)
})

# Convert to a data frame
res_age_sex_highrisk_vacc <- t(as.data.frame(univ_results, check.names = FALSE))
res_age_sex_highrisk_vacc <- as.data.frame(res_age_sex_highrisk_vacc)
names(res_age_sex_highrisk_vacc) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")

# Print the result
print(res_age_sex_highrisk_vacc)

##############################################
#covid_vacc_num
cat("#freq_single(high_risk_surv_data$covid_vacc_num)")
freq_single(high_risk_surv_data$covid_vacc_num)
cat("#sum(is.na(covid_vacc_num)):")
sum(is.na(high_risk_surv_data$covid_vacc_num))
cat("#sum(!is.na(covid_vacc_num)):")
sum(!is.na(high_risk_surv_data$covid_vacc_num))

#covid_vacc_num1
cat("#freq_single(high_risk_surv_data$covid_vacc_num1)")
freq_single(high_risk_surv_data$covid_vacc_num1)
cat("#sum(is.na(covid_vacc_num1)):")
sum(is.na(high_risk_surv_data$covid_vacc_num1))
cat("#sum(!is.na(covid_vacc_num1)):")
sum(!is.na(high_risk_surv_data$covid_vacc_num1))

#imd_num
cat("#freq_single(high_risk_surv_data$imd_num)")
freq_single(high_risk_surv_data$imd_num)
cat("#sum(is.na(imd_num)):")
sum(is.na(high_risk_surv_data$imd_num))
cat("#sum(!is.na(imd_num)):")
sum(!is.na(high_risk_surv_data$imd_num))

#region_num
cat("#freq_single(high_risk_surv_data$region_num)")
freq_single(high_risk_surv_data$region_num)
cat("#sum(is.na(region_num)):")
sum(is.na(high_risk_surv_data$region_num))
cat("#sum(!is.na(region_num)):")
sum(!is.na(high_risk_surv_data$region_num))

#ethnicity_num
cat("#freq_single(high_risk_surv_data$ethnicity_num)")
freq_single(high_risk_surv_data$ethnicity_num)
cat("#sum(is.na(ethnicity_num)):")
sum(is.na(high_risk_surv_data$ethnicity_num))
cat("#sum(!is.na(ethnicity_num)):")
sum(!is.na(high_risk_surv_data$ethnicity_num))

#bmi
cat("#sum(is.na(bmi)):")
sum(is.na(high_risk_surv_data$bmi))
cat("#sum(!is.na(bmi)):")
sum(!is.na(high_risk_surv_data$bmi))
#bmi, bmi_cat_num
cat("#freq_single(high_risk_surv_data$bmi_cat_num)")
freq_single(high_risk_surv_data$bmi_cat_num)

#diabetes
cat("#freq_single(high_risk_surv_data$diabetes)")
freq_single(high_risk_surv_data$diabetes)
cat("#sum(is.na(diabetes)):")
sum(is.na(high_risk_surv_data$diabetes))
cat("#sum(!is.na(diabetes)):")
sum(!is.na(high_risk_surv_data$diabetes))

# hypertension
cat("#freq_single(high_risk_surv_data$hypertension)")
freq_single(high_risk_surv_data$hypertension)
cat("#sum(is.na(hypertension)):")
sum(is.na(high_risk_surv_data$hypertension))
cat("#sum(!is.na(hypertension)):")
sum(!is.na(high_risk_surv_data$hypertension))

 #+ chronic_cardiac_disease 
cat("#freq_single(high_risk_surv_data$chronic_cardiac_disease)")
freq_single(high_risk_surv_data$chronic_cardiac_disease)
cat("#sum(is.na(chronic_cardiac_disease)):")
sum(is.na(high_risk_surv_data$chronic_cardiac_disease))
cat("#sum(!is.na(chronic_cardiac_disease)):")
sum(!is.na(high_risk_surv_data$chronic_cardiac_disease))

 # chronic_respiratory_disease 
 cat("#freq_single(high_risk_surv_data$chronic_respiratory_disease)")
freq_single(high_risk_surv_data$chronic_respiratory_disease)
cat("#sum(is.na(chronic_respiratory_disease)):")
sum(is.na(high_risk_surv_data$chronic_respiratory_disease))
cat("#sum(!is.na(chronic_respiratory_disease)):")
sum(!is.na(high_risk_surv_data$chronic_respiratory_disease))

 # autism 
 cat("#freq_single(high_risk_surv_data$autism)")
freq_single(high_risk_surv_data$autism)
cat("#sum(is.na(autism)):")
sum(is.na(high_risk_surv_data$autism))
cat("#sum(!is.na(autism)):")
sum(!is.na(high_risk_surv_data$autism))

# learning_disability 
cat("#freq_single(high_risk_surv_data$learning_disability)")
freq_single(high_risk_surv_data$learning_disability)
cat("#sum(is.na(learning_disability)):")
sum(is.na(high_risk_surv_data$learning_disability))
cat("#sum(!is.na(learning_disability)):")
sum(!is.na(high_risk_surv_data$learning_disability))

# serious_mental_illness 
cat("#freq_single(high_risk_surv_data$serious_mental_illness)")
freq_single(high_risk_surv_data$serious_mental_illness)
cat("#sum(is.na(serious_mental_illness)):")
sum(is.na(high_risk_surv_data$serious_mental_illness))
cat("#sum(!is.na(serious_mental_illness)):")
sum(!is.na(high_risk_surv_data$serious_mental_illness))

# dementia 
cat("#freq_single(high_risk_surv_data$dementia)")
freq_single(high_risk_surv_data$dementia)
cat("#sum(is.na(dementia)):")
sum(is.na(high_risk_surv_data$dementia))
cat("#sum(!is.na(dementia)):")
sum(!is.na(high_risk_surv_data$dementia))


cat("#summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth )")
cox_model1_age_sex_highrisk_vacc_imd_reg_eth <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num + high_risk_num + covid_vacc_num + imd_num + region_num + ethnicity_num + strata(stp), data = high_risk_surv_data)
summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth)

covariates <- c("age_treated", "sex_num", "high_risk_num", "covid_vacc_num", "imd_num", "region_num", "ethnicity_num" )
# Create formulas for stratified Cox models
forma_univ <- sapply(covariates, function(x) as.formula(paste('Surv(surv_days, surv_event_num) ~', x, '+ strata(stp)')))

# Fit stratified Cox models
univ_models <- lapply(forma_univ, function(x) { coxph(x, data = high_risk_surv_data) })

########################

# Apply the safe_summary function to each model
univ_results <- lapply(univ_models, safe_summary)

# Ensure all results have the same length (if necessary, pad with NAs)
max_length <- max(sapply(univ_results, length))
univ_results <- lapply(univ_results, function(res) {
  length(res) <- max_length  # Pad with NAs to ensure consistent length
  return(res)
})

# Convert to a data frame
res_age_sex_highrisk_vacc_imd_reg_eth <- t(as.data.frame(univ_results, check.names = FALSE))
res_age_sex_highrisk_vacc_imd_reg_eth <- as.data.frame(res_age_sex_highrisk_vacc_imd_reg_eth)
names(res_age_sex_highrisk_vacc_imd_reg_eth) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")

# Print the result
print(res_age_sex_highrisk_vacc_imd_reg_eth)
################################
# cat("#summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb )")
# cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb <- coxph(Surv(surv_days, surv_event_num) ~ drug + age_treated + sex_num + high_risk_num + covid_vacc_num + imd_num + region_num + ethnicity_num + bmi_cat_num + diabetes + hypertension + chronic_cardiac_disease + chronic_respiratory_disease + strata(stp), data = high_risk_surv_data)
# summary(cox_model1_age_sex_highrisk_vacc_imd_reg_eth_comorb)

# Plot the survival curves
# ggsurvplot(survfit(cox_model), data = high_risk_surv_data, pval = TRUE,
#            ggtheme = theme_minimal(), risk.table = TRUE,
#            conf.int = TRUE)

#####################################################################################
# Save dataset(s) ----
write.csv(high_risk_surv_data, here::here("output", "data", "high_risk_surv_data.csv"))
####################################################################################
#####################################################################################


## Import libraries
library('tidyverse')
library('here')
library('lubridate')
library('arrow')
library('dplyr')
library('readr')
library('fs')
library('splines')
library('gtsummary')
library('ggpubr')
library('broom')
library('purrr')
library('tidyr')
library('ggplot2')

## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "graps"), showWarnings = FALSE, recurse = TRUE)

checkNor <- function(data, grp, var, output_file) {
 
  output_file <- here::here("output", "graps", paste0("norm_plot_",var,".png"))

  histogram_plot <- ggplot(data, aes_string(x = var, fill = grp)) + 
    geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
    facet_wrap(as.formula(paste("~", grp))) + 
    theme_minimal() + 
    labs(title = "Distribution by Group")
  
  qq_plot_A <- ggplot(data[data[[grp]] == "Molnupiravir",], aes_string(sample = var)) +
    stat_qq() +
    stat_qq_line() +
    theme_minimal() +
    labs(title = "Q-Q Plot for Group Molnupiravir")
  
  qq_plot_B <- ggplot(data[data[[grp]] == "Sotrovimab",], aes_string(sample = var)) +
    stat_qq() +
    stat_qq_line() +
    theme_minimal() +
    labs(title = "Q-Q Plot for Group Sotrovimab")
  
  # Save plots to a file
  ggsave(output_file, 
         ggarrange(histogram_plot, qq_plot_A, qq_plot_B, ncol = 1, nrow = 3, 
                   labels = c("A", "B", "C")), 
         width = 10, height = 15)
}


#high_risk_basecomp_data<- read_csv("C:/Users/qw/Documents/Github/prophy_effects_Sotro_Molnup/output/data/high_risk_cohort.csv") %>%
high_risk_basecomp_data <- read_csv(here::here("output", "data", "high_risk_cohort.csv")) %>%
    select(patient_id, age_treated, imd, imd_num, drug, bmi, bmi_cat_num, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, 
    calendar_day, calendar_wk, vacc_last_treat_days, high_risk_num, diabetes, hypertension, chronic_cardiac_disease,
    chronic_respiratory_disease, autism, serious_mental_illness, first_covid_treat_interve) %>% #age_treated_spline, calendar_day_spline, 
    mutate(across(c(imd, imd_num, drug, bmi_cat_num, region_num, sex_num, ethnicity_num,stp, covid_vacc_num, high_risk_num, diabetes, hypertension, chronic_cardiac_disease,
    chronic_respiratory_disease, autism, serious_mental_illness, first_covid_treat_interve), as.factor)) %>% 
    mutate(across(c(bmi,calendar_wk), as.numeric))

str(high_risk_basecomp_data, list.len = ncol(high_risk_basecomp_data), give.attr= F)
high_risk_basecomp_data_miss <-show_mis(high_risk_basecomp_data)

#data, grp, var, output_file
checkNor(high_risk_basecomp_data, grp = "first_covid_treat_interve", var = "age_treated")
checkNor(high_risk_basecomp_data, grp = "first_covid_treat_interve",, var = "calendar_day")
checkNor(high_risk_basecomp_data, grp = "first_covid_treat_interve",, var = "vacc_last_treat_days")


sum_output <- high_risk_basecomp_data %>% select(age_treated,bmi,calendar_wk,vacc_last_treat_days)%>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(across(value, list(mean_sd = ~ sum_var(.)$mean_sd, median_iqr = ~ sum_var(.)$median_iqr), .names = "{col}_{fn}"))

options(scipen = 999)

cont_vars1 <- c("age_treated","calendar_wk")
ttest_output <- ttests(data=high_risk_basecomp_data, cont_vars1, ("drug"))
ranksum_output <- map_df(cont_vars1, ~ ranksum_test(high_risk_basecomp_data, .x, "drug"))

cat_vars3 <- c("sex_num", "imd_num", "bmi_cat_num", "region_num","ethnicity_num","covid_vacc_num","high_risk_num","diabetes",
 "hypertension","chronic_cardiac_disease", "autism","serious_mental_illness", "stp")
chisq_output <- map_df(cat_vars3, ~ chisq(high_risk_basecomp_data, .x, "drug"))

# Save dataset(s) ----
write.csv(sum_output, here::here("output", "tables", "sum_continu_vars_output.csv"))
write.csv(ttest_output, here::here("output", "tables", "ttest_output.csv"))
write.csv(ranksum_output, here::here("output", "tables", "ranksum_output.csv"))
write.csv(chisq_output, here::here("output", "tables", "chisq_output.csv"))

write.csv(high_risk_basecomp_data_miss, here::here("output", "tables", "high_risk_basecomp_data_miss.csv"))
write.csv(high_risk_basecomp_data, here::here("output", "data", "high_risk_basecomp_data.csv"))




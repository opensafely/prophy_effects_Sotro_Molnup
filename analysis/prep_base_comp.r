
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
#library("nortest")

## import functions
source(here("analysis", "lib", "r_funs.R"))

## Create directories 
dir_create(here::here("output", "tables"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "graps"), showWarnings = FALSE, recurse = TRUE)

checkNor <- function(x, label="") {

	shp_test <- shapiro.test(x)
	cat("Test raw data x")
	print(shp_test)

	z <- (x - mean(x, na.rm = T))/sd(x, na.rm = T )
	shp_test <- shapiro.test(z)
	cat("Test standardised data z")
	print(shp_test)
	cat("Shapiro-Wilk normality test p value =",shp_test$p.value,"\n")

 	graphics.off()
	fn <- paste("QQ-plot-",label,".png",sep="")
	
	png(fn, width = 20, height = 20, units = 'cm', res = 300)

	names(shp_test)
	pval <- shp_test$p.value
	pval

	##create qq plot 
	qqnorm(z,main=paste("Normal Q-Q plot: ", label, sep=""))
	abline(a=0, b=1, col=2)
    
	legend_txt <- paste("Shapiro-Wilk normality test p=",format(pval,digits=2),sep="")
	legend_txt
	legend(x="topleft", y = NULL, legend=legend_txt, inset = 0.1, title = NULL,bty = "n")

	ggsave(here::here("output", "graps", paste0("norm_plot_",label,".png")))
 	#dev.off()
	
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

gp_molnup<-high_risk_basecomp_data %>% filter(drug == 0 )
gp_sotro<-high_risk_basecomp_data %>% filter(drug == 1 )

checkNor(gp_molnup$age_treated, label="gp_molnup_age")
checkNor(gp_sotro$age_treated, label="gp_sotro_age")
checkNor(gp_molnup$calendar_wk, label="gp_molnup_cal_wk")
checkNor(gp_sotro$calendar_wk, label="gp_sotro_cal_wk")


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
write.csv(high_risk_basecomp_data, here::here("output", "data", "high_risk_basecomp_data.csv"))




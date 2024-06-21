
## Functions written by Qing Wen ## 
## Date last updated: 22/05/2024 ## 
freq_single<-function(idx_colname){ta_feq<-table(idx_colname)
		total<-sum(as.numeric(ta_feq))
		ft2<-cbind(t(ta_feq),total)
		tb_prop0<-round((ft2/total*100),digits=2)
		fre_tb0<-rbind(ft2,tb_prop0)
		fre_tb<-t(fre_tb0)
		return(fre_tb)
}

r2nrst5 <- function(x) {
  round(x / 5) * 5
}

freq_single_nrst5<-function(idx_colname){ta_feq<-r2nrst5(table(idx_colname))
		total<-r2nrst5(sum(as.numeric(ta_feq)))
		ft2<-cbind(t(ta_feq),total)
		tb_prop0<-round((ft2/total*100),digits=2)
		fre_tb0<-rbind(ft2,tb_prop0)
		fre_tb<-t(fre_tb0)
		return(fre_tb)
}

gen_sum_perct <- function(data, var, by_var = NULL) {
  if (!is.null(by_var)) {
    data <- data %>% select(all_of(var), all_of(by_var))
  } else {
    data <- data %>% select(all_of(var))
  }
  
  data %>%
    mutate(across(where(is.character), as.factor)) %>%
    tbl_summary(
      by = by_var, 
      missing = "no",
      type = all_continuous() ~ "continuous2",
      statistic = list(
        all_continuous() ~ c("{mean} ({sd})", "{median} ({IQR})"),
        all_categorical() ~ "{n} ({p}%)"
      )
    )
}

proc_rm_b8 <- function(data,var=vars) {
  result <- data %>%
    select(all_of(vars)) %>%
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
      # Calculate frequencies within each group
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

roundmid_any10 <- function(x, to = 10) {
  rounded <- ceiling(x / to) * to - (floor(to / 2) * (x != 0))
  return(rounded)
}
gen_sum_num <- function(data, var, by_var = NULL) {
  if (!is.null(by_var)) {
     data <- data %>% select(all_of(var), all_of(by_var))
   } else {
     data <- data %>% select(all_of(var))
  }
   
   data %>%
     mutate(across(where(is.character), as.factor)) %>%
     tbl_summary(
       by = by_var, 
       missing = "no",
       statistic = list(
         all_categorical() ~ "{n}"
       )
     )
 }

sum_var <- function(var) {
  mean_sd <- paste0(round(mean(var, na.rm = TRUE), 2), " (", round(sd(var, na.rm = TRUE), 2), ")")
  median_iqr <- paste0(round(median(var, na.rm = TRUE), 2), " (", 
                       round(quantile(var, 0.25, na.rm = TRUE), 2), "-", 
                       round(quantile(var, 0.75, na.rm = TRUE), 2), ")")
  tibble(mean_sd = mean_sd, median_iqr = median_iqr)
}

ttests <- function(data, vars, group_var) {
  data %>%
    select(all_of(c(vars, group_var))) %>%
    pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    nest() %>%
    mutate(t_test = map(data, ~ t.test(value ~ !!sym(group_var), data = .x)),
           tidy_t_test = map(t_test, broom::tidy)) %>%
    select(variable, tidy_t_test) %>%
    unnest(tidy_t_test)
}

chi_sq <- function(data, var, group_var) {
  formula <- as.formula(paste(var, "~", group_var))
  test_result <- chisq.test(table(data[[var]], data[[group_var]]))
  tidy_result <- tidy(test_result)
  tidy_result <- tidy_result %>% mutate(variable = var)
  return(tidy_result)
}

ranksum_test <- function(data, var, group_var) {
  formula <- as.formula(paste(var, "~", group_var))
  test_result <- wilcox.test(as.formula(paste(var, "~", group_var)), data = data)
  tidy_result <- tidy(test_result)
  tidy_result <- tidy_result %>% mutate(variable = var)
  return(tidy_result)
}

org_cox_mod_rd2 <- function(c_model) {
  tidy(c_model, exponentiate = TRUE, conf.int = TRUE) %>%
    rename(hazard_ratio = estimate) %>%
    select(term, p.value, hazard_ratio, conf.low, conf.high, everything()) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

org_cox_mod <- function(c_model) {
  tidy(c_model, exponentiate = TRUE, conf.int = TRUE) %>%
    rename(hazard_ratio = estimate) %>%
    select(term, p.value, hazard_ratio, conf.low, conf.high, everything()) 
}
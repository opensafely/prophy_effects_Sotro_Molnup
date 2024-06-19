
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

roundmid_any10 <- function(x, to = 10) {
  # This function rounds to the nearest multiple of 'to' with a custom midpoint
  ceiling(x / to) * to - (floor(to / 2) * (x != 0))
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
       missing = "always",
       statistic = list(
         all_categorical() ~ "{n}"
       )
     )
 }


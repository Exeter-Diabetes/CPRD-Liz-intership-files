# 01.cohort_definition.R

# Convert int64 safely
is.integer64 <- function(x) class(x) == "integer64"

# ---- Main cohort definition function ----
define_cohort <- function(cohort_dataset, index_date = "treatment_response") {
  library(tidyverse)
  library(rlang)
  
  # ---- Initial cleaning ----
  cohort <- cohort_dataset %>%
    filter(gender != 3) %>%
    mutate(
      patid = as.character(patid),
      across(where(is.integer64), as.integer)
    )
  
  # ---- Index-specific inclusions ----
  if (index_date == "at_diag") {
    cohort <- cohort %>% filter(
      diabetes_type == "type 2",
      dm_diag_date > regstartdate
    )
    index_col <- sym("dm_diag_date")
    age_col <- sym("dm_diag_age")
    prefix <- "pre_index_date_"
  } else if (index_date == "treatment_response") {
    cohort <- cohort %>% filter(
      drug_class == "MFN",
      drugline_all == 1,
      drug_instance == 1
    )
    index_col <- sym("dstartdate")
    age_col <- sym("dstartdate_age")
    prefix <- "predrug_"
  } else {
    stop("index_date must be either 'at_diag' or 'treatment_response'")
  }
  
  # ---- Shared inclusion ----
  cohort <- cohort %>%
    filter(
      !!age_col > 18,
      with_hes == 1,
      !!index_col >= as.Date("2013-01-01")
    )
  
  # ---- Ethnicity recode ----
  cohort <- cohort %>%
    mutate(
      ethnicity_5cat = recode(as.character(ethnicity_5cat),
                              "0" = "White", "1" = "South Asian", "2" = "Black",
                              "3" = "Other", "4" = "Mixed"),
      ethnicity_5cat = factor(ethnicity_5cat,
                              levels = c("White", "South Asian", "Black", "Other", "Mixed"))
    )
  
  # ---- Kidney failure death ----
  cohort <- cohort %>%
    mutate(kf_death_date_primary_cause = if_else(
      !is.na(death_date) & kf_death_primary_cause == 1,
      death_date, as.Date(NA)
    ))
  
  # ---- Frailty category ----
  frailty_var <- paste0(prefix, "frail_elderly_cat")
  cohort <- cohort %>%
    mutate(
      !!sym(frailty_var) := case_when(
        !!age_col <= 70 ~ "Age ≤ 70",
        !!age_col > 70 & predrug_efi_cat %in% c("fit", "mild") ~ "Non-frail > 70",
        !!age_col > 70 & predrug_efi_cat %in% c("moderate", "severe") ~ "Frail > 70"
      ) %>% factor(levels = c("Age ≤ 70", "Non-frail > 70", "Frail > 70"))
    )
  
  # ---- IMD quintiles ----
  cohort <- cohort %>%
    mutate(
      imd_quintile = case_when(
        imd_decile %in% 1:2   ~ "1-2",
        imd_decile %in% 3:4   ~ "3-4",
        imd_decile %in% 5:6   ~ "5-6",
        imd_decile %in% 7:8   ~ "7-8",
        imd_decile %in% 9:10  ~ "9-10",
        TRUE                  ~ NA_character_
      ) %>% factor(levels = c("1-2", "3-4", "5-6", "7-8", "9-10"), ordered = TRUE)
    )
  
  # ---- Composite CVD ----
  cvd_vars <- paste0(prefix, c("myocardialinfarction", "stroke", "angina", "ihd", 
                               "pad", "revasc", "tia", "primary_incident_mi"))
  cvd_output <- paste0(prefix, "cvd")
  
  cohort <- cohort %>%
    mutate(
      !!sym(cvd_output) := if_else(
        rowSums(select(., all_of(cvd_vars)), na.rm = TRUE) > 0, 1, 0
      )
    )
  
  # ---- Composite CKD ----
  ckd_output <- paste0(prefix, "ckd")
  cohort <- cohort %>%
    mutate(
      !!sym(ckd_output) := if_else(
        preckdstage %in% c("stage_3a", "stage_3b", "stage_4"),
        1, 0
      )
    )
  
  
  # Print cohort size
  msg <- if (index_date == "at_diag") {
    "Number of patients at diagnosis within study period: "
  } else {
    "Number of patients initiating metformin within study period: "
  }
  
  print(paste0(msg, nrow(cohort)))
  
  return(cohort)
}
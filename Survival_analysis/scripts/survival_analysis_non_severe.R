#### Set up ####
library(tidyverse)
library(aurum)
library(devtools)
rm(list=ls())

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024",cprdConf = "~/.aurum.yaml")

analysis = cprd$analysis("at_diag")

# Load cohort definition function (the file is in my default working directory, change if not)
source("01.cohort_definition.R")

at_diagnosis <- at_diagnosis %>% analysis$cached("final_20250723")

at_diag <- at_diagnosis %>%
  collect()

# Define cohort
cohort_diag <- define_cohort(at_diag, index_date = "at_diag")



#### Load in the earliest non-severe retinopathy table - Katie's code from email ####
analysis = cprd$analysis("lm")

earliest_retinopathy <- earliest_retinopathy %>% analysis$cached("earliest_non_severe")

earliest_retinopathy <- earliest_retinopathy %>% collect()

# Need to make patid a character variable so it can be joined with cohort_diag
earliest_retinopathy$patid <- as.character(earliest_retinopathy$patid)
cohort_diag <- cohort_diag %>% left_join(earliest_retinopathy, by="patid")

#remove patient if retinopathy before diabetes diagnosis - worth making a note of how many people this is!
cohort_diag <- cohort_diag %>% filter(is.na(earliest_non_severe) | earliest_non_severe>=dm_diag_date)
# From 417,666 to 416,038 so 1628 patients with retinopathy before diabetes diagnosis

# Create a censor_date column which has the date of event or censoring
# Create a censor_var column which is a binary variable indicating whether there was an event or not (0=censored, 1=event)
cohort_diag <- cohort_diag %>%
  mutate(censor_date=pmin(gp_end_date,
                          hes_end_date, #censoring at end of HES records to be consistent with Martha's analysis - although we aren't using HES date
                          death_date,
                          earliest_non_severe,
                          na.rm=TRUE),
         censor_var=ifelse(!is.na(earliest_non_severe ) & censor_date ==earliest_non_severe, 1 , 0))

# Need to create a column with the time, T for each patient
# i.e., the difference between diabetes diagnosis date and event date/censoring date
cohort_diag <- cohort_diag %>%
  mutate(time_to_censor = (censor_date - dm_diag_date)/365.25) %>%
  filter(time_to_censor>=0)


##################################################################################
# Edit from meeting on 02/09/25 - Gender and IMD should be treated as categorical variables rather than continuous
# With gender, can just change coding to be character rather than integer
# IMD needs to be changed to quintile rather than decile - Martha's code

cohort_diag <- cohort_diag %>%
  mutate(
    imd_quintile = case_when(
      imd_decile %in% 1:2   ~ "1",
      imd_decile %in% 3:4   ~ "2",
      imd_decile %in% 5:6   ~ "3",
      imd_decile %in% 7:8   ~ "4",
      imd_decile %in% 9:10  ~ "5",
      TRUE                  ~ NA_character_
    ),
    imd_quintile = factor(imd_quintile,
                          levels = c("1","2","3","4","5"),
                          ordered = FALSE)                    # Changed ordered to false because true gave weird Coxph model results
  )
    
cohort_diag$gender <- as.character(cohort_diag$gender)

##################################################################################


#### Conduct survival analysis ####
library(survival)
library(survminer) # for customizable graphs of survival function
library(broom) # for tidy output; possibly not needed
library(ggplot2)

# Make a Kaplan-Meier plot
KM <- survfit(Surv(time_to_censor, censor_var) ~ 1, data=cohort_diag)
plot(KM, ylab="Survival Probability", xlab="Years")


#### Cox proportional hazards model ####
full_cox <- coxph(Surv(time_to_censor, censor_var) ~ 
                    dm_diag_age + 
                    gender + 
                    ethnicity_5cat +
                    imd_quintile +    # Edited after meeting on 02/09/25
                    prehba1c +
                    prebmi +
                    preegfr +
                    prehdl +
                    prealt +
                    presbp, 
                  data = cohort_diag)

summary(full_cox)

full_cox_tab <- tidy(full_cox, exponentiate=T, conf.int=T)

# Create a plot of hazard ratios and confidence intervals
ggplot(full_cox_tab, 
       aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high)) +
  geom_pointrange() +   # Plots center point (x) and range (xmin, xmax)
  geom_vline(xintercept=1, color="red") +
  labs(y="Covariate", x="Hazard Ratio", title = "Hazard ratios and 95% CIs for non-severe retinopathy") +
  theme_classic()



#### Stepwise Regression ####
library(caret)
library(leaps)
library(MASS)

step_model <- stepAIC(full_cox, direction = "both")
# Returns this error: Error in dropterm.default(fit, scope$drop, scale = scale, trace = max(0,  : number of rows in use has changed: remove missing values?



#### Attempt to remedy this ####
# Create a complete cases data frame
completecases_var <- c("time_to_censor", "censor_var", "dm_diag_age", "gender", "ethnicity_5cat", 
                       "imd_quintile", "prehba1c", "prebmi", "preegfr", "prehdl", "prealt", "presbp")

completecases_df <- cohort_diag[complete.cases(cohort_diag[, completecases_var]),]
# the dimensions of this dataframe match the n of the coxph model (167,184) which is good


# Create the cox model again with complete cases data frame
full_cox_compcases <- coxph(Surv(time_to_censor, censor_var) ~ 
                    dm_diag_age + 
                    gender + 
                    ethnicity_5cat +
                    imd_quintile +
                    prehba1c +
                    prebmi +
                    preegfr +
                    prehdl +
                    prealt +
                    presbp, 
                  data = completecases_df)


step_model <- stepAIC(full_cox_compcases, direction = "both")

summary(step_model)



#### Assessing the proportional hazards assumption ####
# chi-squared test based on Schoenfeld residuals to assess whether any covariates are not proportional hazards 
cox.zph(full_cox)

plot(cox.zph(full_cox))

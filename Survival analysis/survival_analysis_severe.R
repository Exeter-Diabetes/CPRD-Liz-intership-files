#### Setup ####
library(tidyverse)
library(aurum)
library(devtools)
rm(list=ls())

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024",cprdConf = "~/.aurum.yaml")

analysis = cprd$analysis("at_diag")

# Load cohort definition function
source("01.cohort_definition.R")

at_diagnosis <- at_diagnosis %>% analysis$cached("final_20250723")

at_diag <- at_diagnosis %>%
  collect()

# Define cohort
cohort_diag <- define_cohort(at_diag, index_date = "at_diag")


#### Load in the earliest severe retinopathy table and join with cohort ####

analysis = cprd$analysis("lm")

earliest_severe <- earliest_severe %>% analysis$cached("earliest_severe")

earliest_severe <- earliest_severe %>% collect()

# Need to make patid a character variable so it can be joined with cohort_diag
earliest_severe$patid <- as.character(earliest_severe$patid)

cohort_diag <- cohort_diag %>% left_join(earliest_severe, by="patid")

#remove patient if retinopathy before diabetes diagnosis - worth making a note of how many people this is!
cohort_diag <- cohort_diag %>% filter(is.na(earliest_severe) | earliest_severe>=dm_diag_date)
# cohort_diag has 417,666 observations initially and 417,628 after so 38 patients had severe retinopathy code before diabetes


#### Get the data ready for survival analysis ####

# Create a censor_date column which has the date of event or censoring
# Create a censor_var column which is a binary variable indicating whether there was an event or not (0=censored, 1=event)
cohort_diag <- cohort_diag %>%
  mutate(censor_date=pmin(gp_end_date,
                          hes_end_date, 
                          death_date,
                          earliest_severe,
                          na.rm=TRUE),
         censor_var=ifelse(!is.na(earliest_severe ) & censor_date ==earliest_severe, 1 , 0))

# Need to create a column with the time, T for each patient
# i.e., the difference between diabetes diagnosis date and event date/censoring date
cohort_diag <- cohort_diag %>%
  mutate(time_to_censor = (censor_date - dm_diag_date)/365.25) %>%
  filter(time_to_censor>=0)


#### Conduct survival analysis ####
library(survival)
library(survminer) # for customizable graphs of survival function
library(broom) # for tidy output; possibly not needed
library(ggplot2)

# Make a Kaplan-Meier plot
KM <- survfit(Surv(time_to_censor, censor_var) ~ 1, data=cohort_diag)
plot(KM, ylab="survival probability", xlab="years")


#### Cox proportional hazards model ####
full_cox <- coxph(Surv(time_to_censor, censor_var) ~ 
                    dm_diag_age + 
                    gender + 
                    ethnicity_5cat +
                    imd_decile +
                    prehba1c +
                    prebmi +
                    preegfr +
                    prehdl +
                    prealt +
                    presbp, 
                  data = cohort_diag)

summary(full_cox)

full_cox_tab <- tidy(full_cox, exponentiate=T, conf.int=T)

ggplot(full_cox_tab, 
       aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high)) +
  geom_pointrange() +   # Plots center point (x) and range (xmin, xmax)
  geom_vline(xintercept=1, color="red") +
  labs(y="Covariate", x="Hazard Ratio", title = "Hazard ratios and 95% CIs for severe retinopathy") +
  theme_classic()


#### Stepwise Regression ####

library(caret)
library(leaps)
library(MASS)

# Create a complete cases data frame
completecases_var <- c("time_to_censor", "censor_var", "dm_diag_age", "gender", "ethnicity_5cat", 
                       "imd_decile", "prehba1c", "prebmi", "preegfr", "prehdl", "prealt", "presbp")

completecases_df <- cohort_diag[complete.cases(cohort_diag[, completecases_var]),]

# Create the cox model again with complete cases data frame
full_cox_compcases <- coxph(Surv(time_to_censor, censor_var) ~ 
                              dm_diag_age + 
                              gender + 
                              ethnicity_5cat +
                              imd_decile +
                              prehba1c +
                              prebmi +
                              preegfr +
                              prehdl +
                              prealt +
                              presbp, 
                            data = completecases_df)

summary(full_cox_compcases)

# Conduct Stepwise analysis with the complete cases data frame cox model
step_model <- stepAIC(full_cox_compcases, direction = "both")

summary(step_model)

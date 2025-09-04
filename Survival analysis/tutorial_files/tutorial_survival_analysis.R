library(tidyverse)
library(survival)
library(survminer) # for customizable graphs of survival function
library(broom) # for tidy output possibly not needed
library(ggplot2) # for graphing (actually loaded by survminer)
rm(list=ls())

data(cancer, package = "survival")

# Use the Surv() function to specify the survival outcome variables
# For data with a single time variable indicating time to event or censoring the specification is Surv(time, event)
# Where time is the survival/censoring time variable and event is the binary status variable
Surv(aml$time, aml$status)

# Sometimes you need to enter two time variables
# Surv(time, time2, event)
data(heart, package = "survival")

Surv(jasa1$start, jasa1$stop, jasa1$event)


# ---- Kaplan-Meier ----
# We can obtain the KM estimate of S(t) using survfit()
# The first argument is a model formula with a Surv() outcome speicifcation on the left side of ~
# To estimate the survival function for the entire data set, we specify 1 after ~
KM <- survfit(Surv(time, status) ~ 1, data=aml)

print(KM)
# n=23, events=18, median=27, confidence intervals=18 and 45

# To get a plot of the Kaplan-Meier curve
plot(KM, ylab = "survival probability", xlab = "months")

# tidy() function from the broom package produces a table of the KM estimate of the S(t) i.e., the survival function
KM_table <- tidy(KM)


# ---- Plotting ----
# Use the survminer package which uses ggplot2, use the ggsurvplot() function to produce a plot
# Automatically adds + symbols to denote censored observations
ggsurvplot(KM, conf.int=T)

# Can also add a risk table easily
ggsurvplot(KM, conf.int=T, risk.table=T)


# ---- Cox proportional hazards model ----
# Using the lung data set from the survival package
# fit cox model and save results
lung_cox <- coxph(Surv(time, status) ~ age + sex + wt.loss, data=lung)

# summary of results
summary(lung_cox)

# sample size=214, number of events=152, 14 observations dropped due to missingness
# coef: log hazard ratio coefficient, generally positive means increasing the log-hazard and lowering survival
# exp(coef): hazard ratios (exponentiated coefficients) 
## Age: 1.0203, for each additional year at baseline, the hazard increases by 2.03% or by a factor of 1.0203
## Sex: .5939 females (coded as 2) have 60% the hazard of males, or a 40% decrease
## Weight loss: 1.008, for each additional pound of weight loss, the hazard increases by 0.8%
# se(coef): standard error of log hazard ratios
# Pr(>|z|): p-value for test of log hazard ratio = 0, or equivalently, hazard ratio = 1
# lower .95, upper .95: confidence intervals for hazard ratio
# concordance: proportion of pairs that are concordant, a goodness-of-fit measure

# Again can use broom package to tidy the coxph() results and store them in a tibble
lung_cox_table <- tidy(lung_cox, exponentiate=T, conf.int=T)

# Predicting survival after coxph() with survfit()
# If no covariate values are supplied to survfit, then a survival function will be estimated for a subject with mean values on all model covariates
# predict survival function for subject with means on all covariates
surv_at_means <- survfit(lung_cox) 

# Table of survival function
surv_at_means_tab <- tidy(surv_at_means)

# Plot of predicted survival for subject at means of covariates
plot(surv_at_means, xlab="days", ylab="survival probability")


# ---- Exercise 1 (with veteran data set) ----
# Create a graph and table of the KM estimated survival function for the entire data set. What is the median survival time?
ex1_q1 <- survfit(Surv(time, status) ~ 1, data=veteran)
ex1_q1_tab <- tidy(ex1_q1)
ex1_q1_graph <- plot(ex1_q1, ylab="survival probability", xlab="survival time")


# ---- Exercise 2 (with veteran data set) ----
ex2_q1 <- coxph(Surv(time, status) ~ trt + karno, data=veteran)
summary(ex2_q1)
# exp(coef) for karno was 0.9666 so for every 1 score increase in karnofsky score the hazard risk decreases by ~3.4%
# exp(coef) for trt  was 1.1940 but the p-value wasn't significant 
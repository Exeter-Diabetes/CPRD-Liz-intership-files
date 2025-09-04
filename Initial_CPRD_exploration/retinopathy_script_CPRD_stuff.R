#### Set up ####
library(tidyverse)
library(aurum)
library(devtools)
rm(list=ls())

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024",cprdConf = "~/.aurum.yaml")

analysis = cprd$analysis("lm")


#### Import retinopathy codelist and severe/non-severe codelists ####
all_retinopathy <- read_delim("C:/Users/Liz/OneDrive - University of Exeter/BRC_internship/Retinopathy/exeter_medcodelist_retinopathy.txt", col_types=cols(.default=col_character()))

severe <- read_delim("C:/Users/Liz/OneDrive - University of Exeter/BRC_internship/Retinopathy/severe_retinopathy_codelist.txt", col_types=cols(.default=col_character()))

non_severe <- read_delim("C:/Users/Liz/OneDrive - University of Exeter/BRC_internship/Retinopathy/non_severe_retinopathy_codelist.txt", col_types=cols(.default=col_character()))


#### Find all CPRD records with medcodes that relate to retinopathy ####
raw_retinopathy <- cprd$tables$observation %>%
  inner_join(all_retinopathy, by=c("medcodeid"="MedCodeId"), copy=TRUE) %>%
  analysis$cached("raw_retinopathy", indexes=c("patid", "obsdate"))

# Check how many retinopathy related entries exist in the observation data
raw_retinopathy %>% count()
# 4,095,518

# Check how many unique patient IDs have a retinopathy code
raw_retinopathy %>% distinct(patid) %>% count()
# 829,075

# Find the earliest code for each patient with a retinopathy code and produce a data frame
earliest_retinopathy <- raw_retinopathy %>%
  group_by(patid) %>%
  summarise(earliest_retinopathy=min(obsdate, na.rm=TRUE)) %>%
  ungroup() %>%
  analysis$cached("earliest_retinopathy", unique_indexes="patid")


#### Same steps but for the severe codelist ####
raw_severe <- raw_retinopathy %>%
  inner_join(severe, by=c("medcodeid"="MedCodeId"), copy=TRUE) %>%
  analysis$cached("raw_severe", indexes=c("patid", "obsdate"))

raw_severe %>% count()
# 211,555

raw_severe %>% distinct(patid) %>% count()
# 67,929

earliest_severe <- raw_severe %>%
  group_by(patid) %>%
  summarise(earliest_severe=min(obsdate, na.rm=TRUE)) %>%
  ungroup() %>%
  analysis$cached("earliest_severe", unique_indexes="patid")


#### Same steps but for the non-severe codelist ####
raw_non_severe <- raw_retinopathy %>%
  inner_join(non_severe, by=c("medcodeid"="MedCodeId"), copy=TRUE) %>%
  analysis$cached("raw_non_severe", indexes=c("patid", "obsdate"))

raw_non_severe %>% count()
# 3,883,963

raw_non_severe %>% distinct(patid) %>% count()
# 823,977

earliest_non_severe <- raw_non_severe %>%
  group_by(patid) %>%
  summarise(earliest_non_severe=min(obsdate, na.rm=TRUE)) %>%
  ungroup() %>%
  analysis$cached("earliest_non_severe", unique_indexes="patid")

################################################################################

#### Accessing the diabetes cohort ####
# These three lines of code are from Katie's email
analysis = cprd$analysis("all")

diabetes_cohort <- diabetes_cohort %>% analysis$cached("diabetes_cohort")
# this will connect to the pre-existing all_diabetes_cohort table – analysis$cached can’t overwrite tables

analysis = cprd$analysis("lm")

diabetes_cohort %>% distinct(patid) %>% count()
# 2,081,045

# Find out which members of the diabetes cohort also have a retinopathy code
diabetes_retinopathy_match <- diabetes_cohort %>%
  inner_join(raw_retinopathy, by="patid") %>%
  analysis$cached("diabetes_retinopathy_match")

diabetes_retinopathy_match %>% count()
# 3,987,367

diabetes_retinopathy_match %>% distinct(patid) %>% count()
# 800,770


# Find out which members of the diabetes cohort have a severe retinopathy code
diabetes_severe_match <- diabetes_cohort %>%
  inner_join(raw_severe, by="patid") %>%
  analysis$cached("diabetes_severeDR_match")

diabetes_severe_match %>% distinct(patid) %>% count()
# 66,073


# Find out which members of the diabetes cohort have a non-severe retinopathy code
diabetes_non_severe_match <- diabetes_cohort %>%
  inner_join(raw_non_severe, by="patid") %>%
  analysis$cached("diabetes_nonsevereDR_match")

diabetes_non_severe_match %>% distinct(patid) %>% count()
# 796,050

################################################################################

# To load the actual data onto R studio (because I didn't know how to do this before)
data <- diabetes_severe_match %>% collect() 

################################################################################

#### Load cached tables (because I wasn't sure how to do this before) ####
raw_retinopathy <- raw_retinopathy %>% analysis$cached("raw_retinopathy")
earliest_retinopathy <- earliest_retinopathy %>% analysis$cached("earliest_retinopathy")
raw_severe <- raw_severe %>% analysis$cached("raw_severe")
earliest_severe <- earliest_severe %>% analysis$cached("earliest_severe")
raw_non_severe <- raw_non_severe %>% analysis$cached("raw_non_severe")
earliest_non_severe <- earliest_non_severe %>% analysis$cached("earliest_non_severe")


#### How many people have only one severe code (pp=per person) ####
severe_codes_pp <- raw_severe %>%
  group_by(patid) %>%
  summarise(codes_per_person=n()) %>%
  ungroup() %>%
  analysis$cached("severe_codes_pp", unique_indexes="patid")

severe_codes_pp <- collect(severe_codes_pp)

count_ones <- severe_codes_pp %>% count(codes_per_person == 1)
# 31,612 people with only one severe code


#### Same for non-severe codes ####
non_severe_codes_pp <- raw_non_severe %>%
  group_by(patid) %>%
  summarise(ns_codes_pp=n()) %>%
  ungroup() %>%
  analysis$cached("non_severe_codes_pp", unique_indexes="patid")

non_severe_codes_pp <- collect(non_severe_codes_pp)

sum(non_severe_codes_pp$ns_codes_pp == 1)
# 271012 people with only one non-severe code

################################################################################

# Access the ready made MySQL table with all the operation codes for laser photocoagulation
analysis = cprd$analysis("all_patid")

opcs4_laser <- opcs4_laser %>% analysis$cached("raw_ukpds_photocoagulation_opcs4")


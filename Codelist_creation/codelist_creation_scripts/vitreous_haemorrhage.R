# Setup
library(tidyverse)
rm(list=ls())

# Import Aurum medcode look up table
aurum_medcodes <- read_delim("C:/Users/Liz/OneDrive - University of Exeter/BRC_internship/202406_EMISMedicalDictionary.txt", col_types=cols(.default=col_character()))

# Filter for vitreous haemorrhage
VH_codes <- aurum_medcodes %>%
  filter(grepl("vitreous haemorrhage|vitreous hemorrhage", Term, ignore.case = TRUE))

#### Connect to CPRD data ####
library(aurum)
library(devtools)

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024",cprdConf = "~/.aurum.yaml")

analysis = cprd$analysis("lm")

raw_VH <- cprd$tables$observation %>%
  inner_join(VH_codes, by=c("medcodeid"="MedCodeId"), copy=TRUE) %>%
  analysis$cached("raw_VH", indexes=c("patid", "obsdate"))

raw_VH %>% count()
# 48578

raw_VH %>% distinct(patid) %>% count()
# 25319

library(tidyverse)
library(aurum)
rm(list=ls())

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024",cprdConf = "~/.aurum.yaml")

analysis = cprd$analysis("lm")


codelist <- data.frame(medcodeid=c("12345","46474373"))


raw_gastroparesis_codes <- cprd$tables$observation %>%
  inner_join(codelist, by="medcodeid", copy=TRUE) %>%
  analysis$cached("raw_gastroparesis_codes", indexes=c("patid", "obsdate"))

raw_gastroparesis_codes %>% count()

raw_gastroparesis_codes %>% distinct(patid) %>% count()

earliest_code <- raw_gastroparesis_codes %>%
  group_by(patid) %>%
  summarise(earliest_code=min(obsdate, na.rm=TRUE)) %>%
  ungroup() %>%
  analysis$cached("earliest_code", unique_indexes="patid")




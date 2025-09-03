# Setup
library(tidyverse)
rm(list=ls())

# Import Aurum medcode lookup table
# Need to make sure medcodes are imported as characters otherwise can lose precision in long medcodes
aurum_medcodes <- read_delim("C:/Users/Liz/OneDrive - University of Exeter/BRC_internship/202406_EMISMedicalDictionary.txt", col_types=cols(.default=col_character()))

# Filter by key words
gastroparesis_terms <- aurum_medcodes %>%
  filter(grepl("gastroparesis|gastric stasis|gastric atonia|gastric dysmotility|delayed gastric emptying", Term, ignore.case = TRUE))

# Remove rows that relate to post-procedural delayed gastric emptying (rows 12 and 18)
total <- gastroparesis_terms %>% slice(-12,-18)

# Add hastags onto the end of all the medcodes and snowmed codes so that when converted into a CSV they retain their accuracy
with_hashes <- total %>% mutate(MedCodeId = paste0(MedCodeId, "#")) %>%
  mutate(SnomedCTConceptId = paste0(SnomedCTConceptId, "#"))

# Keep only the columns necessary for the Medcode list on GitHub
final_codelist <- subset(with_hashes, select = c(MedCodeId, OriginalReadCode, Term, Observations, SnomedCTConceptId))

# Export the final codelist as a csv file
write_csv(final_codelist, "gastroparesis_codelist.csv")
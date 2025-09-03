# Setup
library(tidyverse)
rm(list=ls())

# Import Aurum medcode look up table
# Need to make sure medcodes are imported as characters otherwise can lose precision in long medcodes
aurum_medcodes <- read_delim("C:/Users/Liz/OneDrive - University of Exeter/BRC_internship/202406_EMISMedicalDictionary.txt", col_types=cols(.default=col_character()))

# Import foot ulcer or infection or ischaemia codes from OpenCodelists: https://www.opencodelists.org/codelist/nhsd-primary-care-domain-refsets/footulcerinfectisch_cod/20250627/
oc_foot <- read_csv("C:/Users/Liz/OneDrive - University of Exeter/BRC_internship/Foot infection/foot_ulcer_infection_ischaemia_SNOMED.csv", col_types=cols(.default=col_character()))

# Import skin and soft tissue infections from OpenCodelists: https://www.opencodelists.org/codelist/ukhsa/skin-and-soft-tissue-infections/2dfc111b/
oc_infection <- read_csv("C:/Users/Liz/OneDrive - University of Exeter/BRC_internship/Foot infection/skin_softtissue_infections_SNOMED.csv", col_types=cols(.default=col_character()))

# Join the foot OpenCodelist with the Aurum Medcode dictionary using an inner join to get all the rows common to the column specified
oc_foot_match <- aurum_medcodes %>% inner_join(oc_foot, by=c("SnomedCTConceptId"="code"))
oc_foot_match %>% distinct(SnomedCTConceptId) %>% count()
# 76

# Filter the general infection OpenCodelist for foot specific terms
oc_infection_foot_only <- oc_infection %>%
  filter(grepl("foot|heel|toe", term, ignore.case = TRUE))

# Remove rows that exclude feet or aren't specific enough, rows: 19, 34, 57, 84, 33, 46, 42, 17, 50, 40, 73
oc_infection_foot_only <- oc_infection_foot_only[-c(19,34,57,84,33,46,42,17,50,40,73),]

# Join the filtered infection OpenCodelist with the Aurum Medcode dictionary
oc_infection_match <- aurum_medcodes %>% inner_join(oc_infection_foot_only, by=c("SnomedCTConceptId"="code"))

# Use the anti join function to see which medcodes are in oc_foot_match but not in oc_infection_match
oc_foot_unique <- anti_join(oc_foot_match, oc_infection_match, by="MedCodeId")
# reduced the obs. by 12 from 114 to 102

# Now need to combine oc_foot_unique with oc_infection_match to create a medcode list with no duplicates
oc_total <- rbind(oc_foot_unique, oc_infection_match)
# 154 obs. (102 and 52)

# Decided that "paronychia of toe" should be added from the medical dictionary
# Paronychia of toe entries can be found on rows 56521, 243922, 243923, 244902, 244903
paronychias <- slice(aurum_medcodes, 56521, 243922, 243923, 244902, 244903)

# Now need to combine all the other foot infection medcodes with the paronychias
#Need to use bind_rows not rbind because the number of columns do not match
oc_paronychias <- bind_rows(oc_total, paronychias)

# might be some terms with neuropathic in them that have been missed - noticed them when looking at neuropathy codelist
# filter aurum_medcodes by "neuropathic"
neuropathic <- aurum_medcodes %>%
  filter(grepl("neuropathic", Term, ignore.case = TRUE))

# Now filter neuropathic terms by foot terms
neuropathic_feet <- neuropathic %>%
  filter(grepl("foot|feet|heel|toe", Term, ignore.case=TRUE))

# Anti join to see which ones in in neuropathic feet list are not in final codelist
neuropathic_foot_unique <- anti_join(neuropathic_feet, oc_paronychias, by="MedCodeId")
# Two unique medcodes

# Create final codelist
total <- bind_rows(oc_paronychias, neuropathic_foot_unique)

################################################################################

# Add hastags onto the end of all the medcodes and snomed codes so that when converted into a CSV they retain their accuracy
with_hashes <- total %>% mutate(MedCodeId = paste0(MedCodeId, "#")) %>%
  mutate(SnomedCTConceptId = paste0(SnomedCTConceptId, "#"))

################################################################################

# Keep only the columns necessary for the Medcode list on GitHub
final_codelist <- subset(with_hashes, select = c(MedCodeId, OriginalReadCode, Term, Observations, SnomedCTConceptId))

# Export the final codelist as a csv file
write_csv(final_codelist, "foot_infections_codelist.csv")

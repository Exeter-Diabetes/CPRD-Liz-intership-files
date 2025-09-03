# Setup
library(tidyverse)
rm(list=ls())

# Import the already created retinopathy codelist (found on github)
retinopathy_codelist <- read_delim("C:/Users/Liz/OneDrive - University of Exeter/BRC_internship/Retinopathy/Codelists - .txt/exeter_medcodelist_retinopathy.txt", col_types=cols(.default=col_character()))

# Obtain a severe retinopathy code list (terms need to pertain to laser therapy, advanced retinopathy, or proliferative retinopathy)
# https://pmc.ncbi.nlm.nih.gov/articles/PMC5337737/ 
# need to specify that it cannot be preproliferative or nonproliferative as a search with just proliferative will include those terms
severe_retinopathy <- retinopathy_codelist %>%
  filter(grepl("proliferative|advanced|laser|prolif.|sight threatening", Term, ignore.case = T)) %>%
  filter(!grepl("non proliferative|nonproliferative|non-proliferative|preproliferative|pre proliferative", Term, ignore.case = T)) 

# Decided to add severe non-proliferative due to a paper suggesting that sight threatening DR includes NPDR, PDR, and DME
# https://link.springer.com/article/10.1186/s12916-021-01966-x
severe <- retinopathy_codelist %>%
  filter(grepl("severe", Term, ignore.case = T))

# Remove the duplicate (has both severe and proliferative in the term)
severe <- severe[-c(3), ] %>%
  rbind(severe_retinopathy)

# Create a non-severe codelist by finding all the codes present in the retinopathy codelist but not the severe retinopathy codelist
non_severe <- retinopathy_codelist %>%
  anti_join(severe, by = "Term")

# Create text files for both code lists
write_tsv(severe, "severe_retinopathy_codelist.txt")
write_tsv(non_severe, "non_severe_retinopathy_codelist.txt")



#### Make the codelists ready for upload to github ####

# Add hashtags to end of medcodes to retain their accuracy when coverted to csv format
severe_with_hashes <- severe %>% 
  mutate(MedCodeId = paste0(MedCodeId, "#"))

non_severe_with_hashes <- non_severe %>% 
  mutate(MedCodeId = paste0(MedCodeId, "#"))


# Remove the unnecessary columns
final_severe <- subset(severe_with_hashes, select = c(MedCodeId, Term, Observations_jun24))

final_non_severe <- subset(non_severe_with_hashes, select = c(MedCodeId, Term, Observations_jun24))


# Export to csv files
write_csv(final_severe, "severe_retinopathy_codelist.csv")

write_csv(final_non_severe, "non_severe_retinopathy_codelist.csv")

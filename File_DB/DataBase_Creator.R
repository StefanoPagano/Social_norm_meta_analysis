library(tidyverse)
library(readxl)
rm(list = ls())

setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/File_DB/")
source("Treatment_db.R") # create treatment-level database
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/File_DB/")
source("Individual_within.R") # create subject-level database for within-subject design paper 
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/File_DB/")
source("Individual_between.R") # create subject-level database for between-subject design paper

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/File_DB/Output/"

# Subjects choices file
Choice_DB <- rbind(Cha026_choice_1, Cha026_choice_2, Her061_choice, Kim003_choice_dg, Kim003_choice_ug, Bas115_choice) %>%
  subset.data.frame(select = -c(p))

write.csv(Choice_DB, file = paste(csv_path_output, "Subjects_choices.csv", sep = ""), row.names = F)


# Subjects beliefs file
Belief_DB <- rbind(Bas115_norms_all, Cha026_beliefs_1, Cha026_beliefs_2, Her061_beliefs, Kim003_beliefs_dg, Kim003_beliefs_ug) %>%
  subset.data.frame(select = -c(choice, endowment, gender, age, A))

write.csv(Belief_DB, file = paste(csv_path_output, "Subjects_beliefs.csv", sep = ""), row.names = F)


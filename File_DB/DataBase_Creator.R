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
Choice_DB <- rbind(Individual_Cha026_DB_1, Individual_Cha026_DB_2, Individual_Her061_DB, Individual_Kim003_dg_DB, Individual_Kim003_ug_DB, Individual_Within_DB) %>%
  subset.data.frame(select = -c(p))

write.csv(Choice_DB, file = paste(csv_path_output, "Subjects_choices.csv", sep = ""), row.names = F)


# Subjects beliefs file
Belief_DB <- rbind(Bas115_output, Cha026_output_1, Cha026_output_2, Her061_output, Kim003_dg_output, Kim003_ug_output) %>%
  subset.data.frame(select = -c(choice, endowment, gender, age, A))

write.csv(Belief_DB, file = paste(csv_path_output, "Subjects_beliefs.csv", sep = ""), row.names = F)


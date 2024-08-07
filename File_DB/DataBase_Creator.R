library(tidyverse)
library(readxl)
rm(list = ls())

setwd("~/GitHub/Social_norm_meta_analysis/File_DB")
source("Treatment_db.R") # create treatment-level database
setwd("~/GitHub/Social_norm_meta_analysis/File_DB")
source("Individual_within.R") # create subject-level database for within-subject design paper 
setwd("~/GitHub/Social_norm_meta_analysis/File_DB")
source("Individual_between.R") # create subject-level database for between-subject design paper

csv_path_output <- "~/GitHub/Social_norm_meta_analysis/File_DB/Output/"

# Subjects choices file
Choice_DB <- rbind(Cha026_choice_1, 
                   Cha026_choice_2, 
                   Cha026_choice_3, 
                   Her061_choice, 
                   Kim003_choice_dg, 
                   Kim003_choice_ug, 
                   Bas115_choice, 
                   Laz164_choices, 
                   Lis165_base_choices, 
                   Lis165_take1_choices, 
                   Kru001_std_choices, 
                   Kru001_bully_choices, 
                   and089_pub_choices, 
                   and089_pvt_choices,
                   Eck169_choices) %>% 
  subset.data.frame(select = -c(p))

write.csv(Choice_DB, file = paste(csv_path_output, "Subjects_choices.csv", sep = ""), row.names = F)


# Subjects beliefs file
Belief_DB <- rbind(Bas115_beliefs, 
                   Cha026_beliefs_1, 
                   Cha026_beliefs_2, 
                   Cha026_beliefs_3, 
                   Her061_beliefs, 
                   Kim003_beliefs_dg, 
                   Kim003_beliefs_ug, 
                   Laz164_beliefs, 
                   Lis165_base_beliefs, 
                   Lis165_take1_beliefs, 
                   Kru001_std_beliefs, 
                   Kru001_bully_beliefs, 
                   and089_pub_beliefs, 
                   and089_pvt_beliefs,
                   Eck169_beliefs) 

write.csv(Belief_DB, file = paste(csv_path_output, "Subjects_beliefs.csv", sep = ""), row.names = F)

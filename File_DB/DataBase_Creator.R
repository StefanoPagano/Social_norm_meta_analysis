library(tidyverse)
library(readxl)
rm(list = ls())

setwd("~/GitHub/Social_norm_meta_analysis/File_DB")
source("Treatment_db.R") # create treatment-level database
setwd("~/GitHub/Social_norm_meta_analysis/File_DB")
source("Individual_within.R") # create subject-level database for within-subject design paper 
setwd("~/GitHub/Social_norm_meta_analysis/File_DB")
source("Individual_between.R") # create subject-level database for between-subject design paper

# Drop ---- 
rm(list = ls()[!(ls() %in% c("Kim003_choice_ug",
                             "Kim003_choice_dg", 
                             "Kim003_beliefs_ug",
                             "Kim003_beliefs_dg",
                             "Cha026_choice_1",
                             "Cha026_beliefs_1",
                             "Cha026_choice_2",
                             "Cha026_beliefs_2",
                             "Cha026_choice_3",
                             "Cha026_beliefs_3",
                             "Her061_choice",
                             "Her061_beliefs",
                             "Bas115_beliefs",
                             "Bas115_choice",
                             "Laz164_choices",
                             "Laz164_beliefs",
                             "Lis165_base_choices",
                             "Lis165_base_beliefs",
                             "Lis165_take1_choices",
                             "Lis165_take1_beliefs",
                             "Kru001_std_beliefs",
                             "Kru001_std_choices",
                             "Kru001_bully_choices",
                             "Kru001_bully_beliefs",
                             "and089_pvt_choices",
                             "and089_pvt_beliefs",
                             "and089_pub_choices",
                             "and089_pub_beliefs",
                             "Eck169_choices",
                             "Eck169_beliefs",
                             "Gac013_choice_dg",
                             "Gac013_choice_tog",
                             "Gac013_norm_dg",
                             "Gac013_norm_tog",
                             "Del037_beliefs",
                             "Del037_choices_BASE",
                             "Tho028_choices",
                             "Tho028_beliefs",
                             "Fro073_choices",
                             "Fro073_norms"))])

csv_path_output <- "~/GitHub/Social_norm_meta_analysis/File_DB/Output/"

# Subjects choices file
Choice_DB <- rbind(Cha026_choice_1, 
                   Cha026_choice_2, 
                   Cha026_choice_3, 
                   Del037_choices_BASE,
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
                   Eck169_choices,
                   Gac013_choice_dg,
                   Gac013_choice_tog,
                   Tho028_choices,
                   Fro073_choices) %>% 
  subset.data.frame(select = -c(p))

write.csv(Choice_DB, file = paste(csv_path_output, paste("Subjects_choices_", today(), ".csv", sep = ""), sep = ""), row.names = F)


# Subjects beliefs file
Belief_DB <- rbind(Bas115_beliefs, 
                   Cha026_beliefs_1, 
                   Cha026_beliefs_2, 
                   Cha026_beliefs_3, 
                   Del037_beliefs,
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
                   Eck169_beliefs,
                   Gac013_norm_dg,
                   Gac013_norm_tog,
                   Tho028_beliefs,
                   Fro073_norms) 

write.csv(Belief_DB, file = paste(csv_path_output, paste("Subjects_beliefs_", today(), ".csv", sep = ""), sep = ""), row.names = F)

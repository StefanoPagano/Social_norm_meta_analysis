# ONLY DG

library(tidyverse)
library(readxl)
rm(list = ls())

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/File_DB/"

#### Paper: 2020Bas115 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2020Bas115")

# choice file
Bas115=read.csv("Basic_Verrina_2021.csv", sep=",")

ns_column = c(1,21:32)
Bas115_norms <- Bas115 %>% subset.data.frame(select = ns_column)

# recoding
Bas115_norms <- Bas115_norms %>%
  mutate(DG_SN_1 = recode(dg_sn_1, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_2 = recode(dg_sn_2, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_3 = recode(dg_sn_3, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_4 = recode(dg_sn_4, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_5 = recode(dg_sn_5, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_6 = recode(dg_sn_6, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_7 = recode(dg_sn_7, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_8 = recode(dg_sn_8, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_9 = recode(dg_sn_9, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_10 = recode(dg_sn_10, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_11 = recode(dg_sn_11, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1)) %>%
  subset.data.frame(select = -c(dg_sn_1, dg_sn_2, dg_sn_3, dg_sn_4, dg_sn_5, dg_sn_6, dg_sn_7, dg_sn_8, dg_sn_9, dg_sn_10, dg_sn_11))


#### Paper: 2018Her061 ----


#### Paper: 2017Del037 ----


#### Paper: 2017Tho028 ----


#### Paper: 2019Cha026 ----


#### Paper: 2016Kim003 ----





# write csv file
write.csv(Belief_DB, file = paste(csv_path_output, "Beliefs.csv", sep = ""), row.names = F)
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

# Subject ID (treatment social 123, private 127 subjects, tot 250)
coldg = c("code","give","social")

# treatment social
# n_progr_1 <- c(1:123)
Bas115_sub_1 <- Bas115 %>%
  subset.data.frame(select = coldg, subset = social == 1) %>%
  mutate(endowment = 10) %>%
  mutate(coop = give/endowment) %>%
  mutate(subject_id = paste("2020Bas115", "1a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "1a", sep = "_"))

colnames(Bas115_sub_1) <- c("code", "choice", "social", "endowment", "cooperation", "subject_id", "treatment_id")

Bas115_sub_1 <- Bas115_sub_1 %>%
  subset.data.frame(select = -c(code, social)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation)

Choice_DB <- Bas115_sub_1

# treatment social
# n_progr_2 <- c(1:127)
Bas115_sub_2 <- Bas115 %>%
  subset.data.frame(select = coldg, subset = social == 0) %>%
  mutate(endowment = 10) %>%
  mutate(coop = give/endowment) %>%
  mutate(subject_id = paste("2020Bas115", "2a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "2a", sep = "_"))

colnames(Bas115_sub_2) <- c("code", "choice", "social", "endowment", "cooperation", "subject_id", "treatment_id")

Bas115_sub_2 <- Bas115_sub_2 %>%
  subset.data.frame(select = -c(code, social)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation)

Choice_DB <- Bas115_sub_2 %>%
  rbind.data.frame(Choice_DB)


#### Paper: 2018Her061 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2018Her061")

# choice file
Her061=read_excel("meta.xlsx", sheet = "behavior")

# Subject ID (treatment monetary 38)
coldg = c("Subject","monetary","action")

# treatment monetary
n_progr_1 <- c(1:38)
Her061_sub_1 <- Her061 %>%
  subset.data.frame(select = coldg, subset = monetary == 1) %>%
  mutate(endowment = 10) %>%
  mutate(coop = action/endowment) %>%
  mutate(subject_id = paste("2018Her061", "9", Subject, sep = "_")) %>%
  mutate(treatment_id = paste("2018Her061", "9", sep = "_"))

colnames(Her061_sub_1) <- c("Subject", "monetary", "choice", "endowment", "cooperation", "subject_id", "treatment_id")

Her061_sub_1 <- Her061_sub_1 %>%
  subset.data.frame(select = -c(Subject, monetary)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation)

Choice_DB <- Her061_sub_1 %>%
  rbind.data.frame(Choice_DB)

#### Paper: 2017Del037 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Del037")

# choice file
Del037=read.csv("DATA_full.csv", sep=",")

# Subject ID (treatment base 60, in 64, out 68 subjects, tot 192)
coldg = c("Date", "Subject","Treatment","earn_TOT","DICT_oth")

# treatment base
# n_progr_1 <- c(1:60)
Del037_sub_1 <- Del037 %>%
  subset.data.frame(select = coldg, subset = Treatment == "BASE") %>%
  mutate(endowment = earn_TOT) %>%
  mutate(coop = DICT_oth/endowment) %>%
  mutate(subject_id = paste("2017Del037", "1", Subject, Date, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "1", sep = "_"))

colnames(Del037_sub_1) <- c("Date", "Subject", "Treatment", "earn_TOT", "choice", "endowment", "cooperation", "subject_id", "treatment_id")

Del037_sub_1 <- Del037_sub_1 %>%
  subset.data.frame(select = -c(Date, Subject, Treatment, earn_TOT)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation) 

Choice_DB <- Del037_sub_1 %>%
  rbind.data.frame(Choice_DB)

# treatment IN
# n_progr_1 <- c(1:64)
Del037_sub_2 <- Del037 %>%
  subset.data.frame(select = coldg, subset = Treatment == "IN") %>%
  mutate(endowment = earn_TOT) %>%
  mutate(coop = DICT_oth/endowment) %>%
  mutate(subject_id = paste("2017Del037", "2", Subject, Date, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "2", sep = "_"))

colnames(Del037_sub_2) <- c("Date", "Subject", "Treatment", "earn_TOT", "choice", "endowment", "cooperation", "subject_id", "treatment_id")

Del037_sub_2 <- Del037_sub_2 %>%
  subset.data.frame(select = -c(Date, Subject, Treatment, earn_TOT)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation)

Choice_DB <- Del037_sub_2 %>%
  rbind.data.frame(Choice_DB)

# treatment OUT
# n_progr_1 <- c(1:68)
Del037_sub_3 <- Del037 %>%
  subset.data.frame(select = coldg, subset = Treatment == "OUT") %>%
  mutate(endowment = earn_TOT) %>%
  mutate(coop = DICT_oth/endowment) %>%
  mutate(subject_id = paste("2017Del037", "3", Subject, Date, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "3", sep = "_"))

colnames(Del037_sub_3) <- c("Date", "Subject", "Treatment", "earn_TOT", "choice", "endowment", "cooperation", "subject_id", "treatment_id")

Del037_sub_3 <- Del037_sub_3 %>%
  subset.data.frame(select = -c(Date, Subject, Treatment, earn_TOT)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation) 

Choice_DB <- Del037_sub_3 %>%
  rbind.data.frame(Choice_DB)

#### Paper: 2017Tho028 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Tho028")

# choice file
Tho028=read_excel("libcons_alldata.xlsx", sheet = "alldata")

# Subject ID (treatment base 106, asym 266, tot 372)
coldg = c("subj", "treat", "role", "decider", "sent")

# treatment base
# n_progr_1 <- c(1:106)
Tho028_sub_1 <- Tho028 %>%
  subset.data.frame(select = coldg, subset = treat == 1) %>%
  subset.data.frame(subset = role == 1) %>%
  subset.data.frame(subset = decider == 1) %>%
  mutate(endowment = 20) %>%
  mutate(coop = sent/endowment) %>%
  mutate(subject_id = paste("2017Tho028", "1", subj, sep = "_")) %>%
  mutate(treatment_id = paste("2017Tho028", "1", sep = "_"))

colnames(Tho028_sub_1) <- c("subj", "treat", "role", "decider", "choice", "endowment", "cooperation", "subject_id", "treatment_id")

Tho028_sub_1 <- Tho028_sub_1 %>%
  subset.data.frame(select = -c(subj, treat, role, decider)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation) 

Choice_DB <- Tho028_sub_1 %>%
  rbind.data.frame(Choice_DB)

# treatment asym
# n_progr_1 <- c(1:166)
Tho028_sub_2 <- Tho028 %>%
  subset.data.frame(select = coldg, subset = treat == 2) %>%
  subset.data.frame(subset = role == 1) %>%
  subset.data.frame(subset = decider == 1) %>%
  mutate(endowment = 20) %>%
  mutate(coop = sent/endowment) %>%
  mutate(subject_id = paste("2017Tho028", "2", subj, sep = "_")) %>%
  mutate(treatment_id = paste("2017Tho028", "2", sep = "_"))

colnames(Tho028_sub_2) <- c("subj", "treat", "role", "decider", "choice", "endowment", "cooperation", "subject_id", "treatment_id")

Tho028_sub_2 <- Tho028_sub_2 %>%
  subset.data.frame(select = -c(subj, treat, role, decider)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation)

Choice_DB <- Tho028_sub_2 %>%
  rbind.data.frame(Choice_DB)

#### Paper: 2019Cha026 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2019Cha026")

# choice file
Cha026=read_excel("data.xls", sheet = "Sheet1", 
              col_types = c("numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric"))

# Subject ID (treatment neutrally 106, tax 266, tot 372)
coldg = c("subject","elicit_norms","frame_tax","endowment", "keep")

# treatment neutrally
# n_progr_1 <- c(1:227)
Cha026_sub_1 <- Cha026 %>%
  subset.data.frame(select = coldg, subset = frame_tax == 0) %>%
  subset.data.frame(subset = elicit_norms == 0) %>%
  subset.data.frame(subset = endowment == 10) %>%
  mutate(sent = endowment - keep) %>%
  mutate(coop = sent/endowment) %>%
  mutate(subject_id = paste("2019Cha026", "1", subject, sep = "_")) %>%
  mutate(treatment_id = paste("2019Cha026", "1", sep = "_"))

colnames(Cha026_sub_1) <- c("subject","elicit_norms","frame_tax","endowment", "keep", "choice", "cooperation", "subject_id", "treatment_id")

Cha026_sub_1 <- Cha026_sub_1 %>%
  subset.data.frame(select = -c(subject, elicit_norms, frame_tax, keep)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation) 

Choice_DB <- Cha026_sub_1 %>%
  rbind.data.frame(Choice_DB)

# treatment frame tax
# n_progr_1 <- c(1:400)
Cha026_sub_2 <- Cha026 %>%
  subset.data.frame(select = coldg, subset = frame_tax == 1) %>%
  subset.data.frame(subset = elicit_norms == 0) %>%
  subset.data.frame(subset = endowment == 10) %>%
  mutate(sent = endowment - keep) %>%
  mutate(coop = sent/endowment) %>%
  mutate(subject_id = paste("2019Cha026", "2", subject, sep = "_")) %>%
  mutate(treatment_id = paste("2019Cha026", "2", sep = "_"))

colnames(Cha026_sub_2) <- c("subject","elicit_norms","frame_tax","endowment", "keep", "choice", "cooperation", "subject_id", "treatment_id")

Cha026_sub_2 <- Cha026_sub_2 %>%
  subset.data.frame(select = -c(subject, elicit_norms, frame_tax, keep)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation) 

Choice_DB <- Cha026_sub_2 %>%
  rbind.data.frame(Choice_DB)

#### Paper: 2016Kim003 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2016Kim003")

# choice file
Kim003=read.csv("DG_Data.csv", sep="\t")

# Subject ID (treatment neutrally 106, tax 266, tot 372)
coldg = c("subj_id", "role", "sent")

# treatment neutrally
# n_progr_1 <- c(1:227)
Kim003_sub_1 <- Kim003 %>%
  subset.data.frame(select = coldg, subset = role == 1) %>%
  mutate(endowment = 16) %>%
  mutate(coop = sent/endowment) %>%
  mutate(subject_id = paste("2016Kim003", "7", subj_id, sep = "_")) %>%
  mutate(treatment_id = paste("2016Kim003", "7", sep = "_"))

colnames(Kim003_sub_1) <- c("subj_id", "role", "choice", "endowment", "cooperation", "subject_id", "treatment_id")

Kim003_sub_1 <- Kim003_sub_1 %>%
  subset.data.frame(select = -c(subj_id, role)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation) 

Choice_DB <- Kim003_sub_1 %>%
  rbind.data.frame(Choice_DB)

# write csv file
write.csv(Choice_DB, file = paste(csv_path_output, "Choices.csv", sep = ""), row.names = F)


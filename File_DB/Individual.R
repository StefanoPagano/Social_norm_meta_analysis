library(tidyverse)
library(readxl)
rm(list = ls())

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/File_DB/"

#### CHOICES DB ----
# paper: 2020Bas115, 2018Her061, 2017Del037, 2017Tho028, 2019Cha026, 2016Kim003
# ONLY DG
#### WITHIN SUBJECTS DESIGN ----
#### Paper: 2020Bas115 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2020Bas115")

# choice file
Bas115=read.csv("Basic_Verrina_2021.csv", sep=",")

# norms file
ns_column = c(1,21:32)
Bas115_db_norms <- Bas115 %>% subset.data.frame(select = ns_column)

# recoding
Bas115_db_norms <- Bas115_db_norms %>%
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

# Subject ID (treatment social 123, private 127 subjects, tot 250)
coldg = c("code","give","social")


# treatment social
# n_progr_1 <- c(1:123)
Bas115_sub_1a <- Bas115 %>%
  subset.data.frame(select = coldg, subset = social == 1) %>%
  mutate(endowment = 10) %>%
  mutate(coop = give/endowment) %>%
  mutate(subject_id = paste("2020Bas115", "1a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "1a", sep = "_"), paper_id = "2020Bas115")
  

colnames(Bas115_sub_1a) <- c("code", "choice", "social", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Bas115_sub_1a <- Bas115_sub_1a %>%
  subset.data.frame(select = -c(code, social)) %>% 
  relocate(subject_id, treatment_id, choice, endowment, cooperation)

Choice_DB <- Bas115_sub_1a

Bas115_norms_1a <- Bas115_db_norms %>%
  subset.data.frame(subset = social == 1) %>%
  mutate(subject_id = paste("2020Bas115", "1a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "1a", sep = "_"), paper_id = "2020Bas115") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

# treatment private
# n_progr_2 <- c(1:127)
Bas115_sub_2a <- Bas115 %>%
  subset.data.frame(select = coldg, subset = social == 0) %>%
  mutate(endowment = 10) %>%
  mutate(coop = give/endowment) %>%
  mutate(subject_id = paste("2020Bas115", "2a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "2a", sep = "_"), paper_id = "2020Bas115")

colnames(Bas115_sub_2a) <- c("code", "choice", "social", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Bas115_sub_2a <- Bas115_sub_2a %>%
  subset.data.frame(select = -c(code, social)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation)

Choice_DB <- Bas115_sub_2a %>%
  rbind.data.frame(Choice_DB)

Bas115_norms_2a <- Bas115_db_norms %>%
  subset.data.frame(subset = social == 0) %>%
  mutate(subject_id = paste("2020Bas115", "2a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "2a", sep = "_"), paper_id = "2020Bas115") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

# PIVOT
Bas115_norms_all <- rbind.data.frame(Bas115_norms_1a, Bas115_norms_2a)

Bas115_norms_all <- Bas115_norms_all %>%
  pivot_longer(!c(subject_id, treatment_id, paper_id, code, social), names_to = "scenarios", values_to = "KW_score") %>%
  mutate(scenarios = as.numeric(recode(scenarios, `DG_SN_1` = 0, `DG_SN_2` = 1, `DG_SN_3` = 2, `DG_SN_4` = 3, `DG_SN_5` = 4, `DG_SN_6` = 5, `DG_SN_7` = 6, `DG_SN_8` = 7, `DG_SN_9` = 8, `DG_SN_10` = 9, `DG_SN_11` = 10))) %>%
  subset.data.frame(select = -c(code, social))

#### Paper: 2017Del037 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Del037")

# choice file
Del037=read.csv("DATA_full.csv", sep=",")

Del037_db_norms <- Del037 %>% 
  subset.data.frame(select = c(ID, Treatment, Dictator,
                               KWansw.1.,
                               KWansw.2.,
                               KWansw.3.,
                               KWansw.4.,
                               KWansw.5.,
                               KWansw.6.,
                               KWansw.7.)) %>%
  mutate(KW1 = recode(KWansw.1., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW2 = recode(KWansw.2., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW3 = recode(KWansw.3., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW4 = recode(KWansw.4., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW5 = recode(KWansw.5., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW6 = recode(KWansw.6., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW7 = recode(KWansw.7., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1)) %>%
  subset.data.frame(select = -c(KWansw.1., KWansw.2., KWansw.3., KWansw.4., KWansw.5., KWansw.6., KWansw.7.))

# Subject ID (treatment base 60, in 64, out 68 subjects, tot 192)
coldg = c("ID", "Treatment", "Dictator", "earn_TOT", "DICT_oth")

# treatment base
# n_progr_1 <- c(1:60)
Del037_sub_1 <- Del037 %>%
  subset.data.frame(select = coldg, subset = Treatment == "BASE") %>%
  subset.data.frame(subset = Dictator == 1) %>%
  mutate(endowment = earn_TOT) %>%
  mutate(coop = DICT_oth/endowment) %>%
  mutate(subject_id = paste("2017Del037", "1", ID, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "1", sep = "_"), paper_id = "2017Del037")

colnames(Del037_sub_1) <- c("ID", "Treatment", "Dictator", "earn_TOT", "choice", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Del037_sub_1 <- Del037_sub_1 %>%
  subset.data.frame(select = -c(ID, Dictator, Treatment, earn_TOT)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation) 

Choice_DB <- Del037_sub_1 %>%
  rbind.data.frame(Choice_DB)

Del037_norms_BASE <- Del037_db_norms %>%
  subset.data.frame(subset = Treatment == "BASE") %>%
  subset.data.frame(subset = Dictator == 1) %>%
  mutate(subject_id = paste("2017Del037", "1", ID, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "1", sep = "_"), paper_id = "2017Del037") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

# treatment IN
# n_progr_1 <- c(1:64)
Del037_sub_2 <- Del037 %>%
  subset.data.frame(select = coldg, subset = Treatment == "IN") %>%
  subset.data.frame(subset = Dictator == 1) %>%
  mutate(endowment = earn_TOT) %>%
  mutate(coop = DICT_oth/endowment) %>%
  mutate(subject_id = paste("2017Del037", "2", ID, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "2", sep = "_"), paper_id = "2017Del037")

colnames(Del037_sub_2) <- c("ID", "Treatment", "Dictator", "earn_TOT", "choice", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Del037_sub_2 <- Del037_sub_2 %>%
  subset.data.frame(select = -c(ID, Dictator, Treatment, earn_TOT)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation)

Choice_DB <- Del037_sub_2 %>%
  rbind.data.frame(Choice_DB)

Del037_norms_IN <- Del037_db_norms %>%
  subset.data.frame(subset = Treatment == "IN") %>%
  subset.data.frame(subset = Dictator == 1) %>%
  mutate(subject_id = paste("2017Del037", "2", ID, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "2", sep = "_"), paper_id = "2017Del037") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

# treatment OUT
# n_progr_1 <- c(1:68)
Del037_sub_3 <- Del037 %>%
  subset.data.frame(select = coldg, subset = Treatment == "OUT") %>%
  subset.data.frame(subset = Dictator == 1) %>%
  mutate(endowment = earn_TOT) %>%
  mutate(coop = DICT_oth/endowment) %>%
  mutate(subject_id = paste("2017Del037", "3", ID, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "3", sep = "_"), paper_id = "2017Del037")

colnames(Del037_sub_3) <- c("ID", "Treatment", "Dictator", "earn_TOT", "choice", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Del037_sub_3 <- Del037_sub_3 %>%
  subset.data.frame(select = -c(ID, Dictator, Treatment, earn_TOT)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation) 

Choice_DB <- Del037_sub_3 %>%
  rbind.data.frame(Choice_DB)

Del037_norms_OUT <- Del037_db_norms %>%
  subset.data.frame(subset = Treatment == "OUT") %>%
  subset.data.frame(subset = Dictator == 1) %>%
  mutate(subject_id = paste("2017Del037", "3", ID, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "3", sep = "_"), paper_id = "2017Del037") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

# PIVOT
Del037_norms_all <- rbind.data.frame(Del037_norms_BASE, Del037_norms_IN, Del037_norms_OUT)

#da confermare ordine score kw
Del037_norms_all <- Del037_norms_all %>%
  pivot_longer(!c(subject_id, treatment_id, paper_id, ID, Treatment, Dictator), names_to = "scenarios", values_to = "KW_score") %>%
  mutate(scenarios = as.numeric(recode(scenarios, `KW1` = 6, `KW2` = 5, `KW3` = 4, `KW4` = 3, `KW5` = 2, `KW6` = 1, `KW7` = 0))) %>%
  subset.data.frame(select = -c(ID, Treatment, Dictator))

#### Paper: 2017Tho028 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Tho028")

# choice file
Tho028=read_excel("libcons_alldata.xlsx", sheet = "alldata")

Tho028_db_norms <- Tho028 %>%
  subset.data.frame(select = c(4:16, 28, 33, 34)) %>%
  mutate(BASE0 = recode(base0, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         BASE1 = recode(base1, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         BASE2 = recode(base2, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         BASE3 = recode(base3, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         BASE4 = recode(base4, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         BASE5 = recode(base5, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ASYM0 = recode(asym1_0, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ASYM1 = recode(asym1_1, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ASYM2 = recode(asym1_2, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ASYM3 = recode(asym1_3, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ASYM4 = recode(asym1_4, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ASYM5 = recode(asym1_5, `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1)) %>%
  subset.data.frame(select = -c(base0, base1, base2, base3, base4, base5, asym1_0, asym1_1, asym1_2, asym1_3, asym1_4, asym1_5))

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
  mutate(treatment_id = paste("2017Tho028", "1", sep = "_"), paper_id = "2017Tho028")

colnames(Tho028_sub_1) <- c("subj", "treat", "role", "decider", "choice", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Tho028_sub_1 <- Tho028_sub_1 %>%
  subset.data.frame(select = -c(subj, treat, role, decider)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation) 

Choice_DB <- Tho028_sub_1 %>%
  rbind.data.frame(Choice_DB)

Tho028_norms_base <- Tho028_db_norms %>%
  subset.data.frame(select = c(1:4, 5:10)) %>%
  subset.data.frame(subset = treat == 1) %>%
  subset.data.frame(subset = role == 1) %>%
  subset.data.frame(subset = decider == 1) %>%
  mutate(subject_id = paste("2017Tho028", "1", subj, sep = "_")) %>%
  mutate(treatment_id = paste("2017Tho028", "1", sep = "_"), paper_id = "2017Tho028") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

colnames(Tho028_norms_base) <- c("subject_id", "treatment_id", "paper_id", "subj", "treat", "role", "decider", "KW0", "KW1", "KW2", "KW3", "KW4", "KW5")


# treatment asym
# n_progr_1 <- c(1:166)
Tho028_sub_2 <- Tho028 %>%
  subset.data.frame(select = coldg, subset = treat == 2) %>%
  subset.data.frame(subset = role == 1) %>%
  subset.data.frame(subset = decider == 1) %>%
  mutate(endowment = 20) %>%
  mutate(coop = sent/endowment) %>%
  mutate(subject_id = paste("2017Tho028", "2", subj, sep = "_")) %>%
  mutate(treatment_id = paste("2017Tho028", "2", sep = "_"), paper_id = "2017Tho028")

colnames(Tho028_sub_2) <- c("subj", "treat", "role", "decider", "choice", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Tho028_sub_2 <- Tho028_sub_2 %>%
  subset.data.frame(select = -c(subj, treat, role, decider)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation)

Choice_DB <- Tho028_sub_2 %>%
  rbind.data.frame(Choice_DB)

Tho028_norms_asym <- Tho028_db_norms %>%
  subset.data.frame(select = c(1:4, 11:16)) %>%
  subset.data.frame(subset = treat == 2) %>%
  subset.data.frame(subset = role == 1) %>%
  subset.data.frame(subset = decider == 1) %>%
  mutate(subject_id = paste("2017Tho028", "2", subj, sep = "_")) %>%
  mutate(treatment_id = paste("2017Tho028", "2", sep = "_"), paper_id = "2017Tho028") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

colnames(Tho028_norms_asym) <- c("subject_id", "treatment_id", "paper_id", "subj", "treat", "role", "decider", "KW0", "KW1", "KW2", "KW3", "KW4", "KW5")

# PIVOT
Tho028_norms_all <- rbind.data.frame(Tho028_norms_base, Tho028_norms_asym)

Tho028_norms_all <- Tho028_norms_all %>%
  pivot_longer(!c(subject_id, treatment_id, paper_id, subj, treat, role, decider), names_to = "scenarios", values_to = "KW_score") %>%
  mutate(scenarios = as.numeric(recode(scenarios, `KW0` = 0, `KW1` = 1, `KW2` = 2, `KW3` = 3, `KW4` = 4, `KW5` = 5))) %>%
  subset.data.frame(select = -c(subj, treat, role, decider))

#### BETWEEN SUBJECTS DESIGN ----
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


#### BELIEFS DB ----

# test
# for (ewt in Choice_DB$endowment) {if (is.na(d) == T) {print(is.na(d))} }
# for (ewt in Choice_DB$endowment) {if (is.nan(d) == T) {print(is.nan(d))} }
# fine test

Belief_DB <- data.frame(p = NA, subject_id = NA, treatment_id = NA, paper_id = NA, scenarios = NA, choice = NA, endowment = NA, A = NA)
dbbase <- Choice_DB %>%
  subset.data.frame(subset = paper_id %in% c("2020Bas115", "2017Tho028", "2017Del037")) %>%
  mutate(n = c(1:length(Choice_DB$subject_id)))

j = 1

for (x in dbbase$n) {
  ewt = dbbase$endowment[x]
  for (i in 0:ewt) {
    new_line_DB <- data.frame(p = j,
                              subject_id = dbbase$subject_id[x], 
                              treatment_id = dbbase$treatment_id[x],
                              paper_id = dbbase$paper_id[x],
                              scenarios = i, 
                              choice = dbbase$choice[x],
                              endowment = dbbase$endowment[x],
                              #KW_score = -999,
                              A = 0)

    Belief_DB <- new_line_DB %>% rbind.data.frame(Belief_DB) %>% arrange(p)
    j = j+1
    
  }

}

Belief_DB <- Belief_DB %>%
  subset.data.frame(subset = endowment > 0)

for (y in Belief_DB$p) {
  if (Belief_DB$choice[y] == Belief_DB$scenarios[y]) {Belief_DB$A[y] = 1}
  else {Belief_DB$A[y] = 0}
}

#Bas115_output <- Belief_DB %>%
#  subset.data.frame(subset = treatment_id == "2020Bas115_1a")

Bas115_output <- Belief_DB %>% merge.data.frame(Bas115_norms_all, by = c("subject_id", "scenarios", "treatment_id", "paper_id")) %>%
  arrange(subject_id, scenarios)

Tho028_output <- Belief_DB %>% merge.data.frame(Tho028_norms_all, by = c("subject_id", "scenarios", "treatment_id", "paper_id")) %>%
  arrange(subject_id, scenarios)

Del037_output <- Belief_DB %>% merge.data.frame(Del037_norms_all, by = c("subject_id", "scenarios", "treatment_id", "paper_id")) %>%
  arrange(subject_id, scenarios)

# write csv file
write.csv(Bas115_output, file = paste(csv_path_output, "Bas115_output.csv", sep = ""), row.names = F)
write.csv(Tho028_output, file = paste(csv_path_output, "Tho028_output.csv", sep = ""), row.names = F)
write.csv(Del037_output, file = paste(csv_path_output, "Del037_output.csv", sep = ""), row.names = F)


csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/File_DB/"

#### CHOICES DB ----
# ONLY DG

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
coldg = c("code","give","social","demo_gender","demo_age") #gender: 1-maschio, 0-femmina


# treatment social
# n_progr_1 <- c(1:123)
Bas115_sub_1a <- Bas115 %>%
  subset.data.frame(select = coldg, subset = social == 1) %>%
  mutate(endowment = 10) %>%
  mutate(coop = give/endowment) %>%
  mutate(subject_id = paste("2020Bas115", "1a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "1a", sep = "_"), paper_id = "2020Bas115")
  

colnames(Bas115_sub_1a) <- c("code", "choice", "social", "gender", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Bas115_sub_1a <- Bas115_sub_1a %>%
  subset.data.frame(select = -c(code, social)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age)

Choice_Within_DB <- Bas115_sub_1a

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

colnames(Bas115_sub_2a) <- c("code", "choice", "social", "gender", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Bas115_sub_2a <- Bas115_sub_2a %>%
  subset.data.frame(select = -c(code, social)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age)

Choice_Within_DB <- Bas115_sub_2a %>%
  rbind.data.frame(Choice_Within_DB)

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
  pivot_longer(!c(subject_id, treatment_id, paper_id, code, social), names_to = "scenarios", values_to = "KW_Normative") %>%
  mutate(scenarios = as.numeric(recode(scenarios, `DG_SN_1` = 0, `DG_SN_2` = 1, `DG_SN_3` = 2, `DG_SN_4` = 3, `DG_SN_5` = 4, `DG_SN_6` = 5, `DG_SN_7` = 6, `DG_SN_8` = 7, `DG_SN_9` = 8, `DG_SN_10` = 9, `DG_SN_11` = 10))) %>%
  subset.data.frame(select = -c(code, social))

#### Paper: 2017Del037 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Del037")

# choice file
Del037=read.csv("DATA_full.csv", sep=",") %>%
  mutate(gender = recode(ANAGansw.1., `2` = 1, `4` = 0),
         age = ANAGansw.2.)

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
coldg = c("ID", "Treatment", "Dictator", "earn_TOT", "DICT_oth", "gender", "age")

# treatment base
# n_progr_1 <- c(1:60)
Del037_sub_1 <- Del037 %>%
  subset.data.frame(select = coldg, subset = Treatment == "BASE") %>%
  subset.data.frame(subset = Dictator == 1) %>%
  mutate(endowment = earn_TOT) %>%
  mutate(coop = DICT_oth/endowment) %>%
  mutate(subject_id = paste("2017Del037", "1", ID, sep = "_")) %>%
  mutate(treatment_id = paste("2017Del037", "1", sep = "_"), paper_id = "2017Del037")

colnames(Del037_sub_1) <- c("ID", "Treatment", "Dictator", "earn_TOT", "choice", "gender", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Del037_sub_1 <- Del037_sub_1 %>%
  subset.data.frame(select = -c(ID, Dictator, Treatment, earn_TOT)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age) 

Choice_Within_DB <- Del037_sub_1 %>%
  rbind.data.frame(Choice_Within_DB)

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

colnames(Del037_sub_2) <- c("ID", "Treatment", "Dictator", "earn_TOT", "choice", "gender", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Del037_sub_2 <- Del037_sub_2 %>%
  subset.data.frame(select = -c(ID, Dictator, Treatment, earn_TOT)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age)

Choice_Within_DB <- Del037_sub_2 %>%
  rbind.data.frame(Choice_Within_DB)

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

colnames(Del037_sub_3) <- c("ID", "Treatment", "Dictator", "earn_TOT", "choice", "gender", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Del037_sub_3 <- Del037_sub_3 %>%
  subset.data.frame(select = -c(ID, Dictator, Treatment, earn_TOT)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age) 

Choice_Within_DB <- Del037_sub_3 %>%
  rbind.data.frame(Choice_Within_DB)

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
  pivot_longer(!c(subject_id, treatment_id, paper_id, ID, Treatment, Dictator), names_to = "scenarios", values_to = "KW_Normative") %>%
  mutate(scenarios = as.numeric(recode(scenarios, `KW1` = 6, `KW2` = 5, `KW3` = 4, `KW4` = 3, `KW5` = 2, `KW6` = 1, `KW7` = 0))) %>%
  subset.data.frame(select = -c(ID, Treatment, Dictator))



#### INDIVIDUAL DB (ONLY WITHIN DESIGN) ----

Individual_Within_DB <- data.frame(p = NA, subject_id = NA, treatment_id = NA, paper_id = NA, scenarios = NA, choice = NA, endowment = NA, A = NA, gender = NA, age = NA, Design = NA)
dbbase <- Choice_Within_DB %>%
  subset.data.frame(subset = paper_id %in% c("2020Bas115", "2017Del037")) %>%
  mutate(n = c(1:length(Choice_Within_DB$subject_id)))

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
                              A = 0,
                              gender = dbbase$gender[x],
                              age = dbbase$age[x],
                              Design = "Within")

    Individual_Within_DB <- new_line_DB %>% rbind.data.frame(Individual_Within_DB) %>% arrange(p)
    j = j+1
    
  }

}

Individual_Within_DB <- Individual_Within_DB %>%
  subset.data.frame(subset = endowment > 0)

for (y in Individual_Within_DB$p) {
  if (Individual_Within_DB$choice[y] == Individual_Within_DB$scenarios[y]) {Individual_Within_DB$A[y] = 1}
  else {Individual_Within_DB$A[y] = 0}
}

Bas115_output <- Individual_Within_DB %>% merge.data.frame(Bas115_norms_all, by = c("subject_id", "scenarios", "treatment_id", "paper_id")) %>%
  arrange(subject_id, scenarios) %>%
  relocate(p, subject_id, treatment_id, paper_id, scenarios, choice, A, endowment, gender, age, Design, KW_Normative) %>%
  subset.data.frame(select = -c(p)) %>%
  mutate(KW_Personal = NA,
         Bicchieri_Empirical = NA,
         Bicchieri_Normative = NA,
         Bicchieri_Personal = NA)

Del037_output <- Individual_Within_DB %>% merge.data.frame(Del037_norms_all, by = c("subject_id", "scenarios", "treatment_id", "paper_id")) %>%
  arrange(subject_id, scenarios) %>%
  relocate(p, subject_id, treatment_id, paper_id, scenarios, choice, A, endowment, gender, age, Design, KW_Normative) %>%
  subset.data.frame(select = -c(p)) %>%
  mutate(KW_Personal = NA,
         Bicchieri_Empirical = NA,
         Bicchieri_Normative = NA,
         Bicchieri_Personal = NA)
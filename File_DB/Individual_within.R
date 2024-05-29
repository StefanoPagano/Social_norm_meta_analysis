
csv_path_output <- "~/GitHub/Social_norm_meta_analysis/File_DB/"

df_merge_game_type = read_xlsx(path = "G:/Mon Drive/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% 
  subset.data.frame(select = c(treatment_id, Game_type, Separate_sample_beliefs))

#### CHOICES DB ----
# ONLY DG

#### Paper: 2020Bas115 ----

# set wd 
setwd("G:/Mon Drive/Meta-analysis beliefs/Dati paper/2020Bas115")

# choice file
Bas115=read.csv("Basic_Verrina_2021.csv", sep=",")

# norms file
ns_column = c(1,3,6,16,18,21:61)
Bas115_db_norms <- Bas115 %>% subset.data.frame(select = ns_column)

# recoding
Bas115_db_norms <- Bas115_db_norms %>%
  mutate(DG_SN_1 = dplyr::recode(dg_sn_1, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_2 = dplyr::recode(dg_sn_2, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_3 = dplyr::recode(dg_sn_3, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_4 = dplyr::recode(dg_sn_4, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_5 = dplyr::recode(dg_sn_5, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_6 = dplyr::recode(dg_sn_6, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_7 = dplyr::recode(dg_sn_7, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_8 = dplyr::recode(dg_sn_8, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_9 = dplyr::recode(dg_sn_9, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_10 = dplyr::recode(dg_sn_10, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DG_SN_11 = dplyr::recode(dg_sn_11, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_1 = dplyr::recode(ug_sn_1, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_2 = dplyr::recode(ug_sn_2, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_3 = dplyr::recode(ug_sn_3, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_4 = dplyr::recode(ug_sn_4, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_5 = dplyr::recode(ug_sn_5, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_6 = dplyr::recode(ug_sn_6, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_7 = dplyr::recode(ug_sn_7, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_8 = dplyr::recode(ug_sn_8, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_9 = dplyr::recode(ug_sn_9, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_10 = dplyr::recode(ug_sn_10, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_11 = dplyr::recode(ug_sn_11, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1)) %>%
  subset.data.frame(select = -c(dg_sn_1, dg_sn_2, dg_sn_3, dg_sn_4, dg_sn_5, dg_sn_6, dg_sn_7, dg_sn_8, dg_sn_9, dg_sn_10, dg_sn_11,
                                ug_sn_1, ug_sn_2, ug_sn_3, ug_sn_4, ug_sn_5, ug_sn_6, ug_sn_7, ug_sn_8, ug_sn_9, ug_sn_10, ug_sn_11))

# Subject ID (treatment social 123, private 127 subjects, tot 250)
coldg = c("code","give","social","role_DG", "demo_gender","demo_age") #gender: 1-maschio, 0-femmina

# DG ----
## treatment social
### choices
Bas115_sub_1a <- Bas115 %>%
  subset.data.frame(select = coldg, subset = social == 1) %>%
  #subset.data.frame(subset = role_DG == 1) %>%
  mutate(endowment = 10) %>%
  mutate(coop = give/endowment) %>%
  mutate(subject_id = paste("2020Bas115", "1a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "1a", sep = "_"), paper_id = "2020Bas115")
  

colnames(Bas115_sub_1a) <- c("code", "choice", "social", "role", "female", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Bas115_sub_1a <- Bas115_sub_1a %>%
  subset.data.frame(select = -c(code, social, role)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, female, age)

Choice_Within_DB <- Bas115_sub_1a

### beliefs
Bas115_norms_1a <- Bas115_db_norms[, c(1:4,6,25:35)] %>%
  subset.data.frame(subset = social == 1) %>%
  #subset.data.frame(subset = role_DG == 1) %>%
  mutate(subject_id = paste("2020Bas115", "1a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "1a", sep = "_"), paper_id = "2020Bas115") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

colnames(Bas115_norms_1a)[colnames(Bas115_norms_1a)=="role_DG"] = "role"
colnames(Bas115_norms_1a)[colnames(Bas115_norms_1a)=="demo_age"] = "age"
colnames(Bas115_norms_1a)[colnames(Bas115_norms_1a)=="demo_gender"] = "female"

## treatment private
### choices
Bas115_sub_2a <- Bas115 %>%
  subset.data.frame(select = coldg, subset = social == 0) %>%
  #subset.data.frame(subset = role_DG == 1) %>%
  mutate(endowment = 10) %>%
  mutate(coop = give/endowment) %>%
  mutate(subject_id = paste("2020Bas115", "2a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "2a", sep = "_"), paper_id = "2020Bas115")

colnames(Bas115_sub_2a) <- c("code", "choice", "social", "role", "female", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Bas115_sub_2a <- Bas115_sub_2a %>%
  subset.data.frame(select = -c(code, social, role)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, female, age)

Choice_Within_DB <- Bas115_sub_2a %>%
  rbind.data.frame(Choice_Within_DB)

### beliefs
Bas115_norms_2a <- Bas115_db_norms[,c(1:4,6,25:35)] %>%
  subset.data.frame(subset = social == 0) %>%
  #subset.data.frame(subset = role_DG == 1) %>%
  mutate(subject_id = paste("2020Bas115", "2a", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "2a", sep = "_"), paper_id = "2020Bas115") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

colnames(Bas115_norms_2a)[colnames(Bas115_norms_2a)=="role_DG"] = "role"
colnames(Bas115_norms_2a)[colnames(Bas115_norms_2a)=="demo_age"] = "age"
colnames(Bas115_norms_2a)[colnames(Bas115_norms_2a)=="demo_gender"] = "female"



### PIVOT for DG 
Bas115_norms_dg <- rbind.data.frame(Bas115_norms_1a, Bas115_norms_2a) %>%
  pivot_longer(!c(subject_id, treatment_id, paper_id, code, social, role, female, age), names_to = "scenarios", values_to = "KW_Normative") %>%
  mutate(scenarios = as.numeric(dplyr::recode(scenarios, `DG_SN_1` = 0, `DG_SN_2` = 1, `DG_SN_3` = 2, `DG_SN_4` = 3, `DG_SN_5` = 4, `DG_SN_6` = 5, `DG_SN_7` = 6, `DG_SN_8` = 7, `DG_SN_9` = 8, `DG_SN_10` = 9, `DG_SN_11` = 10))) %>%
  subset.data.frame(select = -c(code, social, role))

# UG ----

colug = c("code","give_UG","social", "role_UG","demo_gender","demo_age") #gender: 1-maschio, 0-femmina

## treatment social
### choices
Bas115_sub_1c <- Bas115 %>%
  subset.data.frame(select = colug, subset = social == 1) %>%
  subset.data.frame(subset = role_UG == 1) %>%
  mutate(endowment = 10) %>%
  mutate(coop = give_UG/endowment) %>%
  mutate(subject_id = paste("2020Bas115", "1c", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "1c", sep = "_"), paper_id = "2020Bas115")

colnames(Bas115_sub_1c) <- c("code", "choice", "social", "role", "female", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Bas115_sub_1c <- Bas115_sub_1c %>%
  subset.data.frame(select = -c(code, social, role)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, female, age)

Choice_Within_DB <- Bas115_sub_1c %>%
  rbind.data.frame(Choice_Within_DB)

### beliefs
Bas115_norms_1c <- Bas115_db_norms[, c(1:3,5:6,36:46)] %>%
  subset.data.frame(subset = social == 1) %>%
  subset.data.frame(subset = role_UG == 1) %>%
  mutate(subject_id = paste("2020Bas115", "1c", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "1c", sep = "_"), paper_id = "2020Bas115") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

colnames(Bas115_norms_1c)[colnames(Bas115_norms_1c)=="role_UG"] = "role"
colnames(Bas115_norms_1c)[colnames(Bas115_norms_1c)=="demo_age"] = "age"
colnames(Bas115_norms_1c)[colnames(Bas115_norms_1c)=="demo_gender"] = "female"

## treatment private
### choices
Bas115_sub_2c <- Bas115 %>%
  subset.data.frame(select = colug, subset = social == 0) %>%
  subset.data.frame(subset = role_UG == 1) %>%
  mutate(endowment = 10) %>%
  mutate(coop = give_UG/endowment) %>%
  mutate(subject_id = paste("2020Bas115", "2c", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "2c", sep = "_"), paper_id = "2020Bas115")

colnames(Bas115_sub_2c) <- c("code", "choice", "social", "role", "female", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Bas115_sub_2c <- Bas115_sub_2c %>%
  subset.data.frame(select = -c(code, social, role)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, female, age)

Choice_Within_DB <- Bas115_sub_2c %>%
  rbind.data.frame(Choice_Within_DB)

### beliefs
Bas115_norms_2c <- Bas115_db_norms[, c(1:3,5:6,36:46)] %>%
  subset.data.frame(subset = social == 0) %>%
  subset.data.frame(subset = role_UG == 1) %>%
  mutate(subject_id = paste("2020Bas115", "2c", code, sep = "_")) %>%
  mutate(treatment_id = paste("2020Bas115", "2c", sep = "_"), paper_id = "2020Bas115") %>% 
  relocate(paper_id) %>%
  relocate(treatment_id) %>%
  relocate(subject_id)

colnames(Bas115_norms_2c)[colnames(Bas115_norms_2c)=="role_UG"] = "role"
colnames(Bas115_norms_2c)[colnames(Bas115_norms_2c)=="demo_age"] = "age"
colnames(Bas115_norms_2c)[colnames(Bas115_norms_2c)=="demo_gender"] = "female"

### PIVOT for UG 
Bas115_norms_ug <- rbind.data.frame(Bas115_norms_1c, Bas115_norms_2c)%>%
  pivot_longer(!c(subject_id, treatment_id, paper_id, code, social, role, female, age), names_to = "scenarios", values_to = "KW_Normative") %>%
  mutate(scenarios = as.numeric(dplyr::recode(scenarios, `UG_SN_1` = 0, `UG_SN_2` = 1, `UG_SN_3` = 2, `UG_SN_4` = 3, `UG_SN_5` = 4, `UG_SN_6` = 5, `UG_SN_7` = 6, `UG_SN_8` = 7, `UG_SN_9` = 8, `UG_SN_10` = 9, `UG_SN_11` = 10))) %>%
  subset.data.frame(select = -c(code, social, role))

### Merge pivot
Bas115_beliefs <- rbind(Bas115_norms_dg, Bas115_norms_ug) %>%
  merge.data.frame(df_merge_game_type, by = c("treatment_id")) %>%
  mutate(KW_Personal = NA,
         Bicchieri_Empirical = NA,
         Bicchieri_Normative = NA,
         Bicchieri_Personal = NA,
         Design = ifelse(Separate_sample_beliefs == "Y", "Between", "Within")) %>%
  subset.data.frame(select = -c(Separate_sample_beliefs))


#### INDIVIDUAL DB (ONLY WITHIN DESIGN) ----

Bas115_choice <- data.frame(p = NA, subject_id = NA, treatment_id = NA, paper_id = NA, scenarios = NA, choice = NA, endowment = NA, A = NA, female = NA, age = NA)
dbbase <- Choice_Within_DB %>%
  subset.data.frame(subset = paper_id %in% c("2020Bas115")) %>%
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
                              female = dbbase$female[x],
                              age = dbbase$age[x])

    Bas115_choice <- new_line_DB %>% rbind.data.frame(Bas115_choice) %>% arrange(p)
    j = j+1
    
  }

}

Bas115_choice <- Bas115_choice %>%
  subset.data.frame(subset = endowment > 0)

for (y in Bas115_choice$p) {
  if (Bas115_choice$choice[y] == Bas115_choice$scenarios[y]) {Bas115_choice$A[y] = 1}
  else {Bas115_choice$A[y] = 0}
}

Bas115_choice <- Bas115_choice %>%
  merge.data.frame(df_merge_game_type, by = "treatment_id") %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, female, age) %>%
  mutate(Design = ifelse(Separate_sample_beliefs == "Y", "Between", "Within")) %>%
  subset.data.frame(select = -c(Separate_sample_beliefs))



# #### Paper: 2017Del037 ----
# 
# # set wd 
# setwd("G:/Mon Drive/Meta-analysis beliefs/Dati paper/2017Del037")
# 
# # choice file
# Del037=read.csv("DATA_full.csv", sep=",") %>%
#   mutate(gender = dplyr::recode(ANAGansw.1., `2` = 1, `4` = 0),
#          age = ANAGansw.2.)
# 
# Del037_db_norms <- Del037 %>% 
#   subset.data.frame(select = c(ID, Treatment, Dictator,
#                                KWansw.1.,
#                                KWansw.2.,
#                                KWansw.3.,
#                                KWansw.4.,
#                                KWansw.5.,
#                                KWansw.6.,
#                                KWansw.7.)) %>%
#   mutate(KW1 = dplyr::recode(KWansw.1., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
#          KW2 = dplyr::recode(KWansw.2., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
#          KW3 = dplyr::recode(KWansw.3., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
#          KW4 = dplyr::recode(KWansw.4., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
#          KW5 = dplyr::recode(KWansw.5., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
#          KW6 = dplyr::recode(KWansw.6., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
#          KW7 = dplyr::recode(KWansw.7., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1)) %>%
#   subset.data.frame(select = -c(KWansw.1., KWansw.2., KWansw.3., KWansw.4., KWansw.5., KWansw.6., KWansw.7.))
# 
# # Subject ID (treatment base 60, in 64, out 68 subjects, tot 192)
# coldg = c("ID", "Treatment", "Dictator", "earn_TOT", "DICT_oth", "gender", "age")
# 
# # treatment base
# # n_progr_1 <- c(1:60)
# Del037_sub_1 <- Del037 %>%
#   subset.data.frame(select = coldg, subset = Treatment == "BASE") %>%
#   subset.data.frame(subset = Dictator == 1) %>%
#   mutate(endowment = earn_TOT) %>%
#   mutate(coop = DICT_oth/endowment) %>%
#   mutate(subject_id = paste("2017Del037", "1", ID, sep = "_")) %>%
#   mutate(treatment_id = paste("2017Del037", "1", sep = "_"), paper_id = "2017Del037")
# 
# colnames(Del037_sub_1) <- c("ID", "Treatment", "Dictator", "earn_TOT", "choice", "gender", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")
# 
# Del037_sub_1 <- Del037_sub_1 %>%
#   subset.data.frame(select = -c(ID, Dictator, Treatment, earn_TOT)) %>% 
#   relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age) 
# 
# Choice_Within_DB <- Del037_sub_1 %>%
#   rbind.data.frame(Choice_Within_DB)
# 
# Del037_norms_BASE <- Del037_db_norms %>%
#   subset.data.frame(subset = Treatment == "BASE") %>%
#   subset.data.frame(subset = Dictator == 1) %>%
#   mutate(subject_id = paste("2017Del037", "1", ID, sep = "_")) %>%
#   mutate(treatment_id = paste("2017Del037", "1", sep = "_"), paper_id = "2017Del037") %>% 
#   relocate(paper_id) %>%
#   relocate(treatment_id) %>%
#   relocate(subject_id)
# 
# # treatment IN
# # n_progr_1 <- c(1:64)
# Del037_sub_2 <- Del037 %>%
#   subset.data.frame(select = coldg, subset = Treatment == "IN") %>%
#   subset.data.frame(subset = Dictator == 1) %>%
#   mutate(endowment = earn_TOT) %>%
#   mutate(coop = DICT_oth/endowment) %>%
#   mutate(subject_id = paste("2017Del037", "2", ID, sep = "_")) %>%
#   mutate(treatment_id = paste("2017Del037", "2", sep = "_"), paper_id = "2017Del037")
# 
# colnames(Del037_sub_2) <- c("ID", "Treatment", "Dictator", "earn_TOT", "choice", "gender", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")
# 
# Del037_sub_2 <- Del037_sub_2 %>%
#   subset.data.frame(select = -c(ID, Dictator, Treatment, earn_TOT)) %>% 
#   relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age)
# 
# Choice_Within_DB <- Del037_sub_2 %>%
#   rbind.data.frame(Choice_Within_DB)
# 
# Del037_norms_IN <- Del037_db_norms %>%
#   subset.data.frame(subset = Treatment == "IN") %>%
#   subset.data.frame(subset = Dictator == 1) %>%
#   mutate(subject_id = paste("2017Del037", "2", ID, sep = "_")) %>%
#   mutate(treatment_id = paste("2017Del037", "2", sep = "_"), paper_id = "2017Del037") %>% 
#   relocate(paper_id) %>%
#   relocate(treatment_id) %>%
#   relocate(subject_id)
# 
# # treatment OUT
# # n_progr_1 <- c(1:68)
# Del037_sub_3 <- Del037 %>%
#   subset.data.frame(select = coldg, subset = Treatment == "OUT") %>%
#   subset.data.frame(subset = Dictator == 1) %>%
#   mutate(endowment = earn_TOT) %>%
#   mutate(coop = DICT_oth/endowment) %>%
#   mutate(subject_id = paste("2017Del037", "3", ID, sep = "_")) %>%
#   mutate(treatment_id = paste("2017Del037", "3", sep = "_"), paper_id = "2017Del037")
# 
# colnames(Del037_sub_3) <- c("ID", "Treatment", "Dictator", "earn_TOT", "choice", "gender", "age", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")
# 
# Del037_sub_3 <- Del037_sub_3 %>%
#   subset.data.frame(select = -c(ID, Dictator, Treatment, earn_TOT)) %>% 
#   relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age) 
# 
# Choice_Within_DB <- Del037_sub_3 %>%
#   rbind.data.frame(Choice_Within_DB)
# 
# Del037_norms_OUT <- Del037_db_norms %>%
#   subset.data.frame(subset = Treatment == "OUT") %>%
#   subset.data.frame(subset = Dictator == 1) %>%
#   mutate(subject_id = paste("2017Del037", "3", ID, sep = "_")) %>%
#   mutate(treatment_id = paste("2017Del037", "3", sep = "_"), paper_id = "2017Del037") %>% 
#   relocate(paper_id) %>%
#   relocate(treatment_id) %>%
#   relocate(subject_id)
# 
# # PIVOT
# Del037_norms_all <- rbind.data.frame(Del037_norms_BASE, Del037_norms_IN, Del037_norms_OUT)
# 
# #da confermare ordine score kw
# Del037_norms_all <- Del037_norms_all %>%
#   pivot_longer(!c(subject_id, treatment_id, paper_id, ID, Treatment, Dictator), names_to = "scenarios", values_to = "KW_Normative") %>%
#   mutate(scenarios = as.numeric(dplyr::recode(scenarios, `KW1` = 6, `KW2` = 5, `KW3` = 4, `KW4` = 3, `KW5` = 2, `KW6` = 1, `KW7` = 0))) %>%
#   subset.data.frame(select = -c(ID, Treatment, Dictator))

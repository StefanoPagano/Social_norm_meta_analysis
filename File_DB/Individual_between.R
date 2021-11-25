
csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/File_DB/"

df_merge_game_type = read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% 
  subset.data.frame(select = c(treatment_id, Game_type))

#### BETWEEN SUBJECTS DESIGN ----
#### Paper: 2018Her061 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2018Her061")

# choice file
Her061=read_excel("meta.xlsx", sheet = "behavior")

# norms file
Her061_db_norms=read_excel("meta.xlsx", sheet = "sn")
Her061_db_norms <- Her061_db_norms %>% subset.data.frame(select = c(1:13), subset = EXP == 1)
# Subject ID (treatment monetary 38)
coldg = c("Subject","monetary","action")

# treatment monetary
n_progr_1 <- c(1:38)
Her061_sub_1 <- Her061 %>%
  subset.data.frame(select = coldg, subset = monetary == 1) %>%
  mutate(endowment = 10) %>%
  mutate(coop = action/endowment) %>%
  mutate(subject_id = paste("2018Her061", "9", Subject, sep = "_")) %>%
  mutate(treatment_id = paste("2018Her061", "9", sep = "_"), paper_id = "2018Her061")

colnames(Her061_sub_1) <- c("Subject", "monetary", "choice", "endowment", "cooperation", "subject_id", "treatment_id", "paper_id")

Her061_sub_1 <- Her061_sub_1 %>%
  subset.data.frame(select = -c(Subject, monetary)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation)

Choice_Her061_DB <- Her061_sub_1

Her061_KW_score <- Her061_db_norms %>%
  pivot_longer(!c(SUBJECTS, EXP), names_to = "scenarios", values_to = "KW_Score") %>%
  mutate(scenarios = as.numeric(recode(scenarios, `SA_0` = 0, `SA_1` = 1, `SA_2` = 2, `SA_3` = 3, `SA_4` = 4, `SA_5` = 5, `SA_6` = 6, `SA_7` = 7, `SA_8` = 8, `SA_9` = 9, `SA_10` = 10))) %>%
  subset.data.frame(select = -c(EXP)) %>%
  group_by(scenarios) %>%
  summarise(KW_Normative = mean(KW_Score))

Individual_Her061_DB <- data.frame(p = NA, subject_id = NA, treatment_id = NA, paper_id = NA, scenarios = NA, choice = NA, endowment = NA, A = NA, gender = NA, age = NA, Design = NA)
dbbase <- Choice_Her061_DB %>%
  mutate(n = c(1:length(Choice_Her061_DB$subject_id)))

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
                              gender = NA,
                              age = NA,
                              Design = "Between")
    
    Individual_Her061_DB <- new_line_DB %>% rbind.data.frame(Individual_Her061_DB) %>% arrange(p)
    j = j+1
    
  }
  
}

Individual_Her061_DB <- Individual_Her061_DB %>%
  subset.data.frame(subset = endowment > 0)
  

for (y in Individual_Her061_DB$p) {
  if (Individual_Her061_DB$choice[y] == Individual_Her061_DB$scenarios[y]) {Individual_Her061_DB$A[y] = 1}
  else {Individual_Her061_DB$A[y] = 0}
}

Individual_Her061_DB <- Individual_Her061_DB %>%
  merge.data.frame(df_merge_game_type, by = "treatment_id") %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design) 

Her061_output <- Individual_Her061_DB %>% 
  merge.data.frame(Her061_KW_score, by = c("scenarios")) %>%
  arrange(subject_id, scenarios) %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design, KW_Normative) %>%
  subset.data.frame(select = -c(p)) %>%
  mutate(KW_Personal = NA,
         Bicchieri_Empirical = NA,
         Bicchieri_Normative = NA,
         Bicchieri_Personal = NA)


#### Paper: 2019Cha026 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2019Cha026")

# choice file
Cha026=read_excel("data.xls", sheet = "Sheet1", 
                  col_types = c("numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric")) %>%
  mutate(gender = recode(female, `1` = 0, `0` = 1)) #1: male, 0: female

# Subject ID (treatment neutrally 106, tax 266, tot 372)
coldg = c("subject","elicit_norms","frame_tax","endowment", "keep", "gender", "age")

# norms file 
Cha026_db_norms_1 <- Cha026 %>%
  subset.data.frame(select = c(1,4,5,6,8,9,13,14)) %>%
  subset.data.frame(subset = elicit_norms == 1) %>%
  subset.data.frame(subset = frame_tax == 0) %>%
  subset.data.frame(subset = endowment == 10) 

Cha026_db_norms_2 <- Cha026 %>%
  subset.data.frame(select = c(1,4,5,6,8,9,13,14)) %>%
  subset.data.frame(subset = elicit_norms == 1) %>%
  subset.data.frame(subset = frame_tax == 1) %>%
  subset.data.frame(subset = endowment == 10)

# treatment neutrally
# n_progr_1 <- c(1:227)
Cha026_sub_1 <- Cha026 %>%
  subset.data.frame(select = coldg, subset = frame_tax == 0) %>%
  subset.data.frame(subset = elicit_norms == 0) %>%
  subset.data.frame(subset = endowment == 10) %>%
  mutate(sent = endowment - keep) %>%
  mutate(coop = sent/endowment) %>%
  mutate(subject_id = paste("2019Cha026", "1", subject, sep = "_")) %>%
  mutate(treatment_id = paste("2019Cha026", "1", sep = "_"), paper_id = "2019Cha026")

colnames(Cha026_sub_1) <- c("subject","elicit_norms","frame_tax","endowment", "keep", "gender", "age","choice", "cooperation", "subject_id", "treatment_id", "paper_id")

Cha026_sub_1 <- Cha026_sub_1 %>%
  subset.data.frame(select = -c(subject, elicit_norms, frame_tax, keep)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age) 

Choice_Cha026_DB_1 <- Cha026_sub_1

Cha026_KW_score_1 <- Cha026_db_norms_1 %>%
  group_by(action) %>%
  summarise(KW_Normative = mean(norm))

colnames(Cha026_KW_score_1) <- c("scenarios", "KW_Normative")

Individual_Cha026_DB_1 <- data.frame(p = NA, subject_id = NA, treatment_id = NA, paper_id = NA, scenarios = NA, choice = NA, endowment = NA, A = NA, gender = NA, age = NA, Design = NA)
dbbase <- Choice_Cha026_DB_1 %>%
  mutate(n = c(1:length(Choice_Cha026_DB_1$subject_id)))

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
                              Design = "Between")
    
    Individual_Cha026_DB_1 <- new_line_DB %>% rbind.data.frame(Individual_Cha026_DB_1) %>% arrange(p)
    j = j+1
    
  }
  
}

Individual_Cha026_DB_1 <- Individual_Cha026_DB_1 %>%
  subset.data.frame(subset = endowment > 0) 

for (y in Individual_Cha026_DB_1$p) {
  if (Individual_Cha026_DB_1$choice[y] == Individual_Cha026_DB_1$scenarios[y]) {Individual_Cha026_DB_1$A[y] = 1}
  else {Individual_Cha026_DB_1$A[y] = 0}
}

Individual_Cha026_DB_1 <- Individual_Cha026_DB_1 %>%
  merge.data.frame(df_merge_game_type, by = "treatment_id") %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design)

Cha026_output_1 <- Individual_Cha026_DB_1 %>% 
  merge.data.frame(Cha026_KW_score_1, by = c("scenarios")) %>%
  arrange(subject_id, scenarios) %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design, KW_Normative) %>%
  subset.data.frame(select = -c(p)) %>%
  mutate(KW_Personal = NA,
         Bicchieri_Empirical = NA,
         Bicchieri_Normative = NA,
         Bicchieri_Personal = NA)

# treatment frame tax
# n_progr_1 <- c(1:400)
Cha026_sub_2 <- Cha026 %>%
  subset.data.frame(select = coldg, subset = frame_tax == 1) %>%
  subset.data.frame(subset = elicit_norms == 0) %>%
  subset.data.frame(subset = endowment == 10) %>%
  mutate(sent = endowment - keep) %>%
  mutate(coop = sent/endowment) %>%
  mutate(subject_id = paste("2019Cha026", "2", subject, sep = "_")) %>%
  mutate(treatment_id = paste("2019Cha026", "2", sep = "_"), paper_id = "2019Cha026")

colnames(Cha026_sub_2) <- c("subject","elicit_norms","frame_tax","endowment", "keep", "gender", "age","choice", "cooperation", "subject_id", "treatment_id", "paper_id")

Cha026_sub_2 <- Cha026_sub_2 %>%
  subset.data.frame(select = -c(subject, elicit_norms, frame_tax, keep)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age) 

Choice_Cha026_DB_2 <- Cha026_sub_2 

Cha026_KW_score_2 <- Cha026_db_norms_2 %>%
  group_by(action) %>%
  summarise(KW_Normative = mean(norm))

colnames(Cha026_KW_score_2) <- c("scenarios", "KW_Normative")

Individual_Cha026_DB_2 <- data.frame(p = NA, subject_id = NA, treatment_id = NA, paper_id = NA, scenarios = NA, choice = NA, endowment = NA, A = NA, gender = NA, age = NA, Design = NA)
dbbase <- Choice_Cha026_DB_2 %>%
  mutate(n = c(1:length(Choice_Cha026_DB_2$subject_id)))

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
                              Design = "Between")
    
    Individual_Cha026_DB_2 <- new_line_DB %>% rbind.data.frame(Individual_Cha026_DB_2) %>% arrange(p)
    j = j+1
    
  }
  
}

Individual_Cha026_DB_2 <- Individual_Cha026_DB_2 %>%
  subset.data.frame(subset = endowment > 0)

for (y in Individual_Cha026_DB_2$p) {
  if (Individual_Cha026_DB_2$choice[y] == Individual_Cha026_DB_2$scenarios[y]) {Individual_Cha026_DB_2$A[y] = 1}
  else {Individual_Cha026_DB_2$A[y] = 0}
}

Individual_Cha026_DB_2 <- Individual_Cha026_DB_2 %>%
  merge.data.frame(df_merge_game_type, by = "treatment_id") %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design)

Cha026_output_2 <- Individual_Cha026_DB_2 %>%
  merge.data.frame(Cha026_KW_score_1, by = c("scenarios")) %>%
  arrange(subject_id, scenarios) %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design, KW_Normative) %>%
  subset.data.frame(select = -c(p)) %>%
  mutate(KW_Personal = NA,
         Bicchieri_Empirical = NA,
         Bicchieri_Normative = NA,
         Bicchieri_Personal = NA)

#### Paper: 2016Kim003 ----

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2016Kim003")

# choice file
Kim003_dg=read.csv("DG_Data.csv", sep="\t")
Kim003_ug=read.table("UG_Data.txt", header = T)

# norms file
Kim003_db_norms=read.csv("Norm_Elicitation_Data.csv", sep = ",")

Kim003_db_norms <- Kim003_db_norms %>% 
  mutate(ANSW01 = recode(answers.1., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW02 = recode(answers.2., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW03 = recode(answers.3., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW04 = recode(answers.4., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW05 = recode(answers.5., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW06 = recode(answers.6., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW07 = recode(answers.7., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW08 = recode(answers.8., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW09 = recode(answers.9., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW12 = recode(answers.12., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW13 = recode(answers.13., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW14 = recode(answers.14., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW15 = recode(answers.15., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW16 = recode(answers.16., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW17 = recode(answers.17., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW18 = recode(answers.18., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW19 = recode(answers.19., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW20 = recode(answers.20., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW21 = recode(answers.21., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW22 = recode(answers.22., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW23 = recode(answers.23., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW24 = recode(answers.24., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW25 = recode(answers.25., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW26 = recode(answers.26., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW27 = recode(answers.27., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW28 = recode(answers.28., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW29 = recode(answers.29., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW30 = recode(answers.30., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW31 = recode(answers.31., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW34 = recode(answers.34., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW35 = recode(answers.35., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW36 = recode(answers.36., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW37 = recode(answers.37., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW38 = recode(answers.38., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW39 = recode(answers.39., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW40 = recode(answers.40., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW41 = recode(answers.41., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         ANSW42 = recode(answers.42., `0` = 0,`1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1))


# DG
## 
coldg = c("subj_id", "role", "sent", "gender", "age")

Kim003_sub_1 <- Kim003_dg %>%
  subset.data.frame(select = coldg, subset = role == 1) %>%
  mutate(endowment = 16) %>%
  mutate(coop = sent/endowment) %>%
  mutate(agex= 2016-age) %>%
  mutate(subject_id = paste("2016Kim003", "7", subj_id, sep = "_")) %>%
  mutate(treatment_id = paste("2016Kim003", "7", sep = "_"), paper_id = "2016Kim003")

colnames(Kim003_sub_1) <- c("subj_id", "role", "choice", "gender", "year_sub", "endowment", "cooperation", "age", "subject_id", "treatment_id", "paper_id")

Kim003_sub_1 <- Kim003_sub_1 %>%
  subset.data.frame(select = -c(subj_id, role, year_sub)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age) 

Choice_Kim003_dg_DB <- Kim003_sub_1

## norms 
Kim003_db_norms_dg <- Kim003_db_norms %>% subset.data.frame(select = dg_columns <- c(1, 3, 4, 135:143))

Kim003_KW_score_dg <- Kim003_db_norms_dg %>%
  pivot_longer(!c(X140626_1150, session, Subject), names_to = "scenarios", values_to = "KW_score") %>%
  mutate(scenarios = as.numeric(recode(scenarios, `ANSW01` = 0, `ANSW02` = 2, `ANSW03` = 4, `ANSW04` = 6, `ANSW05` = 8, `ANSW06` = 10, `ANSW07` = 12, `ANSW08` = 14, `ANSW09` = 16))) %>%
  subset.data.frame(select = -c(X140626_1150, session, Subject)) %>%
  group_by(scenarios) %>%
  summarise(KW_Normative = mean(KW_score))

Kim003_KW_score_dg_add <- data.frame(scenarios = seq(1,15,2), KW_Normative = NA)
Kim003_KW_score_dg_add$KW_Normative[1] = mean(c(Kim003_KW_score_dg$KW_Normative[1],Kim003_KW_score_dg$KW_Normative[2]))
Kim003_KW_score_dg_add$KW_Normative[2] = mean(c(Kim003_KW_score_dg$KW_Normative[2],Kim003_KW_score_dg$KW_Normative[3]))
Kim003_KW_score_dg_add$KW_Normative[3] = mean(c(Kim003_KW_score_dg$KW_Normative[3],Kim003_KW_score_dg$KW_Normative[4]))
Kim003_KW_score_dg_add$KW_Normative[4] = mean(c(Kim003_KW_score_dg$KW_Normative[4],Kim003_KW_score_dg$KW_Normative[5]))
Kim003_KW_score_dg_add$KW_Normative[5] = mean(c(Kim003_KW_score_dg$KW_Normative[5],Kim003_KW_score_dg$KW_Normative[6]))
Kim003_KW_score_dg_add$KW_Normative[6] = mean(c(Kim003_KW_score_dg$KW_Normative[6],Kim003_KW_score_dg$KW_Normative[7]))
Kim003_KW_score_dg_add$KW_Normative[7] = mean(c(Kim003_KW_score_dg$KW_Normative[7],Kim003_KW_score_dg$KW_Normative[8]))
Kim003_KW_score_dg_add$KW_Normative[8] = mean(c(Kim003_KW_score_dg$KW_Normative[8],Kim003_KW_score_dg$KW_Normative[9]))

Kim003_KW_score_dg <- Kim003_KW_score_dg_add %>% rbind.data.frame(Kim003_KW_score_dg) %>% arrange(scenarios)

Individual_Kim003_dg_DB <- data.frame(p = NA, subject_id = NA, treatment_id = NA, paper_id = NA, scenarios = NA, choice = NA, endowment = NA, A = NA, gender = NA, age = NA, Design = NA)
dbbase <- Choice_Kim003_dg_DB %>%
  mutate(n = c(1:length(Choice_Kim003_dg_DB$subject_id)))

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
                              Design = "Between")
    
    Individual_Kim003_dg_DB <- new_line_DB %>% rbind.data.frame(Individual_Kim003_dg_DB) %>% arrange(p)
    j = j+1
  }
  
}

Individual_Kim003_dg_DB <- Individual_Kim003_dg_DB %>%
  subset.data.frame(subset = endowment > 0)

for (y in Individual_Kim003_dg_DB$p) {
  if (Individual_Kim003_dg_DB$choice[y] == Individual_Kim003_dg_DB$scenarios[y]) {Individual_Kim003_dg_DB$A[y] = 1}
  else {Individual_Kim003_dg_DB$A[y] = 0}
}

Individual_Kim003_dg_DB <- Individual_Kim003_dg_DB %>%
  merge.data.frame(df_merge_game_type, by = "treatment_id") %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design)

Kim003_dg_output <- Individual_Kim003_dg_DB %>%
  merge.data.frame(Kim003_KW_score_dg, by = c("scenarios")) %>%
  arrange(subject_id, scenarios) %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design, KW_Normative) %>%
  subset.data.frame(select = -c(p)) %>%
  mutate(KW_Personal = NA,
         Bicchieri_Empirical = NA,
         Bicchieri_Normative = NA,
         Bicchieri_Personal = NA)

# UG
colug = c("exp_id","exp_num","subj_id","subj", "role", "sent", "gender", "age")

Kim003_sub_2 <- Kim003_ug %>%
  subset.data.frame(select = colug, subset = role == 1) %>%
  mutate(endowment = 16) %>%
  mutate(coop = sent/endowment) %>%
  mutate(agex= 2016-age) %>%
  mutate(subject_id = paste("2016Kim003", "8", subj_id, sep = "_")) %>%
  mutate(treatment_id = paste("2016Kim003", "8", sep = "_"), paper_id = "2016Kim003")

colnames(Kim003_sub_2) <- c("exp_id", "exp_num", "subj_id", "subj", "role", "choice", "gender", "year_sub", "endowment", "cooperation", "age", "subject_id", "treatment_id", "paper_id")

Kim003_sub_2 <- Kim003_sub_2 %>%
  subset.data.frame(select = -c(exp_id, exp_num, subj_id, subj, role, year_sub)) %>% 
  relocate(subject_id, treatment_id, paper_id, choice, endowment, cooperation, gender, age) 

Choice_Kim003_DB <- Kim003_sub_2

Kim003_db_norms_ug <- Kim003_db_norms %>% subset.data.frame(select = dg_columns <- c(1, 3, 4, 155:163))

Kim003_KW_score_ug <- Kim003_db_norms_ug %>%
  pivot_longer(!c(X140626_1150, session, Subject), names_to = "scenarios", values_to = "KW_score") %>%
  mutate(scenarios = as.numeric(recode(scenarios, `ANSW23` = 0, `ANSW24` = 2, `ANSW25` = 4, `ANSW26` = 6, `ANSW27` = 8, `ANSW28` = 10, `ANSW29` = 12, `ANSW30` = 14, `ANSW31` = 16))) %>%
  subset.data.frame(select = -c(X140626_1150, session, Subject)) %>%
  group_by(scenarios) %>%
  summarise(KW_Normative = mean(KW_score))

Kim003_KW_score_ug_add <- data.frame(scenarios = seq(1,15,2), KW_Normative = NA)
Kim003_KW_score_ug_add$KW_Normative[1] = mean(c(Kim003_KW_score_ug$KW_Normative[1],Kim003_KW_score_ug$KW_Normative[2]))
Kim003_KW_score_ug_add$KW_Normative[2] = mean(c(Kim003_KW_score_ug$KW_Normative[2],Kim003_KW_score_ug$KW_Normative[3]))
Kim003_KW_score_ug_add$KW_Normative[3] = mean(c(Kim003_KW_score_ug$KW_Normative[3],Kim003_KW_score_ug$KW_Normative[4]))
Kim003_KW_score_ug_add$KW_Normative[4] = mean(c(Kim003_KW_score_ug$KW_Normative[4],Kim003_KW_score_ug$KW_Normative[5]))
Kim003_KW_score_ug_add$KW_Normative[5] = mean(c(Kim003_KW_score_ug$KW_Normative[5],Kim003_KW_score_ug$KW_Normative[6]))
Kim003_KW_score_ug_add$KW_Normative[6] = mean(c(Kim003_KW_score_ug$KW_Normative[6],Kim003_KW_score_ug$KW_Normative[7]))
Kim003_KW_score_ug_add$KW_Normative[7] = mean(c(Kim003_KW_score_ug$KW_Normative[7],Kim003_KW_score_ug$KW_Normative[8]))
Kim003_KW_score_ug_add$KW_Normative[8] = mean(c(Kim003_KW_score_ug$KW_Normative[8],Kim003_KW_score_ug$KW_Normative[9]))

Kim003_KW_score_ug <- Kim003_KW_score_ug_add %>% rbind.data.frame(Kim003_KW_score_ug) %>% arrange(scenarios)

Individual_Kim003_ug_DB <- data.frame(p = NA, subject_id = NA, treatment_id = NA, paper_id = NA, scenarios = NA, choice = NA, endowment = NA, A = NA, gender = NA, age = NA, Design = NA)
dbbase <- Choice_Kim003_DB %>%
  mutate(n = c(1:length(Choice_Kim003_DB$subject_id)))

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
                              Design = "Between")
    
    Individual_Kim003_ug_DB <- new_line_DB %>% rbind.data.frame(Individual_Kim003_ug_DB) %>% arrange(p)
    j = j+1
    
  }
  
}

Individual_Kim003_ug_DB <- Individual_Kim003_ug_DB %>%
  subset.data.frame(subset = endowment > 0) 

for (y in Individual_Kim003_ug_DB$p) {
  if (Individual_Kim003_ug_DB$choice[y] == Individual_Kim003_ug_DB$scenarios[y]) {Individual_Kim003_ug_DB$A[y] = 1}
  else {Individual_Kim003_ug_DB$A[y] = 0}
}

Individual_Kim003_ug_DB <- Individual_Kim003_ug_DB %>%
  merge.data.frame(df_merge_game_type, by = "treatment_id") %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design)

Kim003_ug_output <- Individual_Kim003_ug_DB %>%
  merge.data.frame(Kim003_KW_score_ug, by = c("scenarios")) %>%
  arrange(subject_id, scenarios) %>%
  relocate(p, subject_id, treatment_id, paper_id, Game_type, scenarios, choice, A, endowment, gender, age, Design, KW_Normative) %>%
  subset.data.frame(select = -c(p)) %>%
  mutate(KW_Personal = NA,
         Bicchieri_Empirical = NA,
         Bicchieri_Normative = NA,
         Bicchieri_Personal = NA)

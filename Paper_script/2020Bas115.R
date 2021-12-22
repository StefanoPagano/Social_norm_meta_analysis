# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2020Bas115")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
dg=read.csv("Basic_Verrina_2021.csv", sep=",")

# norms file
ns_column = c(1,21:61)
norms <- dg %>% subset.data.frame(select = ns_column)

# recoding
norms <- norms %>%
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
         DG_SN_11 = recode(dg_sn_11, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DGT_SN_1 = recode(dgt_sn_1, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DGT_SN_2 = recode(dgt_sn_2, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DGT_SN_3 = recode(dgt_sn_3, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DGT_SN_4 = recode(dgt_sn_4, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DGT_SN_5 = recode(dgt_sn_5, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DGT_SN_6 = recode(dgt_sn_6, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DGT_SN_7 = recode(dgt_sn_7, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DGT_SN_8 = recode(dgt_sn_8, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         DGT_SN_9 = recode(dgt_sn_9, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_1 = recode(ug_sn_1, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_2 = recode(ug_sn_2, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_3 = recode(ug_sn_3, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_4 = recode(ug_sn_4, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_5 = recode(ug_sn_5, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_6 = recode(ug_sn_6, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_7 = recode(ug_sn_7, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_8 = recode(ug_sn_8, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_9 = recode(ug_sn_9, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_10 = recode(ug_sn_10, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         UG_SN_11 = recode(ug_sn_11, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1))

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2020Bas115", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG Social-----------------
# get information on treatment

# cleaning DG
## code :  subject code
## give: amount of money given to other
## social : treatment variable -> 1. social, 0. private

coldg = c("code","give","social")

# 1. Choice dataframe ----
dgS_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = social == 1) %>%
  mutate(endowment = 10, cooperation = give/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2020Bas115", TreatmentCode = "1a")


# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## code
## social
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,43:53)
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 1) %>% summarise(n_sub_N = n())

## compute norm 
dgS_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 1) %>%
  summarise_at(vars(DG_SN_1:DG_SN_11), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- dgS_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020Bas115", TreatmentCode = "1a")

positive_appropriateness <- dgS_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dgS_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dgS_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dgS_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
dgS_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = social == 1) %>%
  summarise_at(vars(DG_SN_1:DG_SN_11), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dgS_final_norms <- merge.data.frame(dgS_appropriateness_sum, dgS_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020Bas115", 
         TreatmentCode = "1a", 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(dgS_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dgS_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dgS_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m))


# DG Private-----------------
# get information on treatment

# cleaning DG
## code :  subject code
## give: amount of money given to other
## social : treatment variable -> 1. social, 0. private

coldg = c("code","give","social")

# 1. Choice dataframe ----
dgP_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = social == 0) %>%
  mutate(endowment = 10, cooperation = give/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2020Bas115", TreatmentCode = "2a")


# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## code
## social
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,43:53)
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 0) %>% summarise(n_sub_N = n())

## compute norm 
dgP_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 0) %>%
  summarise_at(vars(DG_SN_1:DG_SN_11), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- dgP_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020Bas115", TreatmentCode = "2a") %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- dgP_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dgP_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dgP_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dgP_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
dgP_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = social == 0) %>%
  summarise_at(vars(DG_SN_1:DG_SN_11), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dgP_final_norms <- merge.data.frame(dgP_appropriateness_sum, dgP_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020Bas115", 
         TreatmentCode = "2a", 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(dgP_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dgP_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dgP_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# DG Tax Social-----------------
# get information on treatment

# cleaning DG
## code :  subject code
## give_DG_tax: amount of money given to other in dg tax game
## social : treatment variable -> 1. social, 0. private

coldg = c("code","give_DG_tax","social")

# 1. Choice dataframe ----
dgtS_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = social == 1) %>%
  mutate(endowment = 12, cooperation = give_DG_tax/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2020Bas115", TreatmentCode = "1b")


# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## code
## social
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,12,1.5))
norms_columns <- c(1,2,54:62)
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 1) %>% summarise(n_sub_N = n())

## compute norm 
dgtS_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 1) %>%
  summarise_at(vars(DGT_SN_1:DGT_SN_9), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- dgtS_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020Bas115", TreatmentCode = "1b") %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- dgtS_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dgtS_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dgtS_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dgtS_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
dgtS_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = social == 1) %>%
  summarise_at(vars(DGT_SN_1:DGT_SN_9), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dgtS_final_norms <- merge.data.frame(dgtS_appropriateness_sum, dgtS_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020Bas115", 
         TreatmentCode = "1b",
         Avg_NE = as.integer(donation)/12,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(dgtS_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dgtS_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dgtS_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# DG Tax Private-----------------
# get information on treatment

# cleaning DG
## code :  subject code
## give_DG_tax: amount of money given to other in dg tax game
## social : treatment variable -> 1. social, 0. private

coldg = c("code","give_DG_tax","social")

# 1. Choice dataframe ----
dgtP_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = social == 0) %>%
  mutate(endowment = 12, cooperation = give_DG_tax/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2020Bas115", TreatmentCode = "2b")


# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## code
## social
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,12,1.5))
norms_columns <- c(1,2,54:62)
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 0) %>% summarise(n_sub_N = n())

## compute norm 
dgtP_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 0) %>%
  summarise_at(vars(DGT_SN_1:DGT_SN_9), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- dgtP_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020Bas115", TreatmentCode = "2b") %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- dgtP_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dgtP_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dgtP_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dgtP_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
dgtP_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = social == 0) %>%
  summarise_at(vars(DGT_SN_1:DGT_SN_9), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dgtP_final_norms <- merge.data.frame(dgtP_appropriateness_sum, dgtP_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020Bas115", 
         TreatmentCode = "2b", 
         Avg_NE = as.integer(donation)/12,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(dgtP_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dgtP_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dgtP_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# UG Social-----------------
# get information on treatment

# cleaning UG
## code :  subject code
## receive: amount of money given to other
## role_UG: which role subject was assigned in UG (1 = proposer, 2 = responder)
## social : treatment variable -> 1. social, 0. private

coldg = c("code","give_UG","social", "role_UG")

# 1. Choice dataframe ----
ugS_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = social == 1) %>%
  subset.data.frame(subset = role_UG == 1) %>%
  mutate(endowment = 10, cooperation = give_UG/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2020Bas115", TreatmentCode = "1c")


# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## code
## social
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,63:73)
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 1) %>% summarise(n_sub_N = n())

## compute norm 
ugS_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 1) %>%
  summarise_at(vars(UG_SN_1:UG_SN_11), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- ugS_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020Bas115", TreatmentCode = "1c") %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- ugS_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(ugS_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- ugS_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- ugS_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
ugS_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = social == 1) %>%
  summarise_at(vars(UG_SN_1:UG_SN_11), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

ugS_final_norms <- merge.data.frame(ugS_appropriateness_sum, ugS_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020Bas115", 
         TreatmentCode = "1c", 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(ugS_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(ugS_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(ugS_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)


# UG Private-----------------
# get information on treatment

# cleaning UG
## code :  subject code
## receive: amount of money given to other
## role_UG: which role subject was assigned in UG (1 = proposer, 2 = responder)
## social : treatment variable -> 1. social, 0. private

coldg = c("code","give_UG","social", "role_UG")

# 1. Choice dataframe ----
ugP_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = social == 0) %>%
  subset.data.frame(subset = role_UG == 1) %>%
  mutate(endowment = 10, cooperation = give_UG/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2020Bas115", TreatmentCode = "2c")


# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## code
## social
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,63:73)
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 0) %>% summarise(n_sub_N = n())

## compute norm 
ugP_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = social == 0) %>%
  summarise_at(vars(UG_SN_1:UG_SN_11), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- ugP_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020Bas115", TreatmentCode = "2c") %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- ugP_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(ugP_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- ugP_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- ugP_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
ugP_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = social == 0) %>%
  summarise_at(vars(UG_SN_1:UG_SN_11), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

ugP_final_norms <- merge.data.frame(ugP_appropriateness_sum, ugP_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020Bas115", 
         TreatmentCode = "2c", 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(ugP_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(ugP_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(ugP_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

write.csv(db_appropriateness, file = paste(csv_path_output, paste(db_appropriateness$PaperID[1], "_avg_kw.csv", sep = ""), sep = ""), row.names = F)
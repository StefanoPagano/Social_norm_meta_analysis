# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2021Kon127")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
norms_s1=read.csv("kw_raw_study1.csv", sep=",")
norms_s2=read.csv("kw_raw_study2.csv", sep=",")


# recoding
norms_s1 <- norms_s1 %>%
  mutate(DG_SN_0 = recode(player.decision10_0, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_1 = recode(player.decision9_1, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_2 = recode(player.decision8_2, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_3 = recode(player.decision7_3, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_4 = recode(player.decision6_4, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_5 = recode(player.decision5_5, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_6 = recode(player.decision4_6, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_7 = recode(player.decision3_7, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_8 = recode(player.decision2_8, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_9 = recode(player.decision1_9, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_10 = recode(player.decision0_10, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1))

norms_s2 <- norms_s2 %>%
  mutate(DG_SN_0 = recode(player.decision10_0, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_1 = recode(player.decision9_1, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_2 = recode(player.decision8_2, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_3 = recode(player.decision7_3, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_4 = recode(player.decision6_4, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_5 = recode(player.decision5_5, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_6 = recode(player.decision4_6, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_7 = recode(player.decision3_7, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_8 = recode(player.decision2_8, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_9 = recode(player.decision1_9, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1),
         DG_SN_10 = recode(player.decision0_10, `-3` = -1, `-2` = -0.6, `-1` = -0.3, `1` = 0.3, `2` = 0.6, `3` = 1))

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2021Kon127", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# Study 1 - DG Baseline-----------------
# get information on treatment

# cleaning DG
## participant.id_in_session
## participant.code
## participant._current_app_name
## player.treatment
## player.decision10_0

s1_dg_baseline_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2021Kon127", TreatmentCode = 1)

# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,7,15,68:78)
n_sub_N = norms_s1 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "baseline") %>% summarise(n_sub_N = sum(!is.na(DG_SN_0)))

## compute norm 
s1_dg_baseline_appropriateness_sum <- norms_s1 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "baseline") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- s1_dg_baseline_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
s1_dg_baseline_norms_var <- norms_s1[, norms_columns] %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "baseline") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

s1_dg_baseline_final_norms <- merge.data.frame(s1_dg_baseline_appropriateness_sum, s1_dg_baseline_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2021Kon127", 
         TreatmentCode = 1, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Sd_Avg_NE = sd(s1_dg_baseline_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(s1_dg_baseline_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(s1_dg_baseline_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m))

# Study 1 - DG Always-----------------
# get information on treatment

# cleaning DG
## participant.id_in_session
## participant.code
## participant._current_app_name
## player.treatment
## player.decision10_0

s1_dg_always_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2021Kon127", TreatmentCode = 2)

# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,7,15,68:78)
n_sub_N = norms_s1 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "always remind") %>% summarise(n_sub_N = sum(!is.na(DG_SN_0)))

## compute norm 
s1_dg_always_appropriateness_sum <- norms_s1 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "always remind") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- s1_dg_always_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
s1_dg_always_norms_var <- norms_s1[, norms_columns] %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "always remind") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

s1_dg_always_final_norms <- merge.data.frame(s1_dg_always_appropriateness_sum, s1_dg_always_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2021Kon127", 
         TreatmentCode = 2, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Sd_Avg_NE = sd(s1_dg_always_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(s1_dg_always_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(s1_dg_always_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# Study 1 - DG Never remind-----------------
# get information on treatment

# cleaning DG
## participant.id_in_session
## participant.code
## participant._current_app_name
## player.treatment
## player.decision10_0

s1_dg_nr_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2021Kon127", TreatmentCode = 3)

# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,7,15,68:78)
n_sub_N = norms_s1 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "never remind") %>% summarise(n_sub_N = sum(!is.na(DG_SN_0)))

## compute norm 
s1_dg_nr_appropriateness_sum <- norms_s1 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "never remind") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- s1_dg_nr_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
s1_dg_nr_norms_var <- norms_s1[, norms_columns] %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "never remind") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

s1_dg_nr_final_norms <- merge.data.frame(s1_dg_nr_appropriateness_sum, s1_dg_nr_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2021Kon127", 
         TreatmentCode = 3, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Sd_Avg_NE = sd(s1_dg_nr_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(s1_dg_nr_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(s1_dg_nr_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# Study 1 - DG No conflict-----------------
# get information on treatment

# cleaning DG
## participant.id_in_session
## participant.code
## participant._current_app_name
## player.treatment
## player.decision10_0

s1_dg_nc_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2021Kon127", TreatmentCode = 4)

# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,7,15,68:78)
n_sub_N = norms_s1 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "no conflict") %>% summarise(n_sub_N = sum(!is.na(DG_SN_0)))

## compute norm 
s1_dg_nc_appropriateness_sum <- norms_s1 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "no conflict") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- s1_dg_nc_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
s1_dg_nc_norms_var <- norms_s1[, norms_columns] %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "no conflict") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

s1_dg_nc_final_norms <- merge.data.frame(s1_dg_nc_appropriateness_sum, s1_dg_nc_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2021Kon127", 
         TreatmentCode = 4, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Sd_Avg_NE = sd(s1_dg_nc_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(s1_dg_nc_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(s1_dg_nc_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# Study 2 - DG Baseline-----------------
# get information on treatment

# cleaning DG
## participant.id_in_session
## participant.code
## participant._current_app_name
## player.treatment
## player.decision10_0

s2_dg_baseline_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2021Kon127", TreatmentCode = 7)

# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,7,15,73:83)
n_sub_N = norms_s2 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "baseline") %>% summarise(n_sub_N = sum(!is.na(DG_SN_0)))

## compute norm 
s2_dg_baseline_appropriateness_sum <- norms_s2 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "baseline") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- s2_dg_baseline_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
s2_dg_baseline_norms_var <- norms_s2[, norms_columns] %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "baseline") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

s2_dg_baseline_final_norms <- merge.data.frame(s2_dg_baseline_appropriateness_sum, s2_dg_baseline_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2021Kon127", 
         TreatmentCode = 7, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Sd_Avg_NE = sd(s2_dg_baseline_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(s2_dg_baseline_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(s2_dg_baseline_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# Study 2 - DG Always-----------------
# get information on treatment

# cleaning DG
## participant.id_in_session
## participant.code
## participant._current_app_name
## player.treatment
## player.decision10_0

s2_dg_always_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2021Kon127", TreatmentCode = 8)

# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,7,15,73:83)
n_sub_N = norms_s2 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "always remind") %>% summarise(n_sub_N = sum(!is.na(DG_SN_0)))

## compute norm 
s2_dg_always_appropriateness_sum <- norms_s2 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "always remind") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- s2_dg_always_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
s2_dg_always_norms_var <- norms_s2[, norms_columns] %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "always remind") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

s2_dg_always_final_norms <- merge.data.frame(s2_dg_always_appropriateness_sum, s2_dg_always_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2021Kon127", 
         TreatmentCode = 8, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Sd_Avg_NE = sd(s2_dg_always_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(s2_dg_always_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(s2_dg_always_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# Study 2 - DG Never remind-----------------
# get information on treatment

# cleaning DG
## participant.id_in_session
## participant.code
## participant._current_app_name
## player.treatment
## player.decision10_0

s2_dg_nr_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2021Kon127", TreatmentCode = 9)

# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,7,15,73:83)
n_sub_N = norms_s2 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "never remind") %>% summarise(n_sub_N = sum(!is.na(DG_SN_0)))

## compute norm 
s2_dg_nr_appropriateness_sum <- norms_s2 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "never remind") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- s2_dg_nr_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
s2_dg_nr_norms_var <- norms_s2[, norms_columns] %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "never remind") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

s2_dg_nr_final_norms <- merge.data.frame(s2_dg_nr_appropriateness_sum, s2_dg_nr_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2021Kon127", 
         TreatmentCode = 9, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Sd_Avg_NE = sd(s2_dg_nr_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(s2_dg_nr_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(s2_dg_nr_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# Study 2 - DG No conflict-----------------
# get information on treatment

# cleaning DG
## participant.id_in_session
## participant.code
## participant._current_app_name
## player.treatment
## player.decision10_0

s2_dg_nc_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2021Kon127", TreatmentCode = 10)

# 2. Beliefs dataframe ----
## KW: send from 0 to 10, step 1
## KW scale: 1: VI; 2: I; 3: RI; 4: RA; 5: A; 6: VA -> recoding done.

label_col = as.character(seq(0,10,1))
norms_columns <- c(1,2,7,15,73:83)
n_sub_N = norms_s2 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "no conflict") %>% summarise(n_sub_N = sum(!is.na(DG_SN_0)))

## compute norm 
s2_dg_nc_appropriateness_sum <- norms_s2 %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "no conflict") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- s2_dg_nc_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
s2_dg_nc_norms_var <- norms_s2[, norms_columns] %>%
  subset.data.frame(subset = participant._current_app_name == "kwtask") %>%
  subset.data.frame(subset = player.treatment == "no conflict") %>%
  summarise_at(vars(DG_SN_0:DG_SN_10), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

s2_dg_nc_final_norms <- merge.data.frame(s2_dg_nc_appropriateness_sum, s2_dg_nc_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2021Kon127", 
         TreatmentCode = 10, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Sd_Avg_NE = sd(s2_dg_nc_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(s2_dg_nc_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(s2_dg_nc_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)


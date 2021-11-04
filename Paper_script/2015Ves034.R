# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2015Ves034")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

ug=read.csv("data.csv", sep = ",")

colnames(ug) <- c("Subject", "Session", "Condition", "Incentives", "Approriateness_first", "AR0", "AR1", "AR2", "AR3", "AR4", "AR5", "AR6", "AR7", "AR8", "AR9", "AR10", "FR0", "FR1", "FR2", "FR3", "FR4", "FR5", "FR6", "FR7", "FR8", "FR9", "FR10", "Gender", "Age")

## the next file contains all data except the conditional PG elicitations, which we did later
norms <- ug %>% 
  subset.data.frame(subset = Condition == c(1,2)) %>%
  mutate(AR0_M = recode(AR0, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR1_M = recode(AR1, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR2_M = recode(AR2, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR3_M = recode(AR3, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR4_M = recode(AR4, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR5_M = recode(AR5, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR6_M = recode(AR6, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR7_M = recode(AR7, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR8_M = recode(AR8, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR9_M = recode(AR9, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1),
         AR10_M = recode(AR10, `1`= -1, `2` = -1/3, `3` = 1/3, `4` = 1))

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2015Ves034", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# UG Incentivized-Appropriateness first ---------------
# get information on treatment

# cleaning UG
## no choice experiment, only norm elicitation

# 1. Choice dataframe ----

ug1_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2015Ves034", TreatmentCode = 1)

# 2. Beliefs dataframe ----
## Experiment condition : incentives + appropriateness first (1), incentives + fairness first (2), no incentives + appropriateness first (3), no incentives + fairness first (4)
## TreatmentCode in database : KW_Incentivized_first (1) ; KW_Incentivized_last (4)
## Endowment : 10
## KW scale: 1: VI; 2: I; 3: A; 4: VA

label_col = as.character(seq(0,10,1))

## compute norm 
ug1_appropriateness_sum <- norms %>%
  subset.data.frame(subset = Condition == 1) %>%
  summarise_at(vars(AR0_M:AR10_M), sum, na.rm=T) %>% 
  t.data.frame() %>%
  cbind.data.frame(donation=label_col)

## compute variance norm
ug1_norms_var <- norms %>%
  subset.data.frame(subset = Condition == 1) %>%
  summarise_at(vars(AR0_M:AR10_M), var, na.rm=T) %>% 
  t.data.frame() %>%
  cbind.data.frame(donation=label_col)

ug1_final_norms <- merge.data.frame(ug1_appropriateness_sum, ug1_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2015Ves034", 
         TreatmentCode = 1, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Strength_NE = sort(ug1_appropriateness_sum$., decreasing = T)[1]/sort(ug1_appropriateness_sum$., decreasing = T)[2]) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(ug1_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(ug1_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))

# UG Incentivized-Appropriateness last ---------------
# get information on treatment

# cleaning UG
## no choice experiment, only norm elicitation

# 1. Choice dataframe ----

ug2_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2015Ves034", TreatmentCode = 4)

# 2. Beliefs dataframe ----
## Experiment condition : incentives + appropriateness first (1), incentives + fairness first (2), no incentives + appropriateness first (3), no incentives + fairness first (4)
## TreatmentCode in database : KW_Incentivized_first (1) ; KW_Incentivized_last (4)
## Endowment : 10
## KW scale: 1: VI; 2: I; 3: A; 4: VA

label_col = as.character(seq(0,10,1))

## compute norm 
ug2_appropriateness_sum <- norms %>%
  subset.data.frame(subset = Condition == 2) %>%
  summarise_at(vars(AR0_M:AR10_M), sum, na.rm=T) %>% 
  t.data.frame() %>%
  cbind.data.frame(donation=label_col)

## compute variance norm
ug2_norms_var <- norms %>%
  subset.data.frame(subset = Condition == 2) %>%
  summarise_at(vars(AR0_M:AR10_M), var, na.rm=T) %>% 
  t.data.frame() %>%
  cbind.data.frame(donation=label_col)

ug2_final_norms <- merge.data.frame(ug2_appropriateness_sum, ug2_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2015Ves034", 
         TreatmentCode = 4, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y,
         Strength_NE = sort(ug2_appropriateness_sum$., decreasing = T)[1]/sort(ug2_appropriateness_sum$., decreasing = T)[2]) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(ug2_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(ug2_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

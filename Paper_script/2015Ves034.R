library(tidyverse)
library(readxl)
rm(list = ls())

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2015Ves034")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

ug=read.csv("data.csv", sep = ",")

colnames(ug) <- c("Subject", "Session", "Condition", "Incentives", "Approriateness_first", "AR0", "AR1", "AR2", "AR3", "AR4", "AR5", "AR6", "AR7", "AR8", "AR9", "AR10", "FR0", "FR1", "FR2", "FR3", "FR4", "FR5", "FR6", "FR7", "FR8", "FR9", "FR10", "Gender", "Age")

## the next file contains all data except the conditional PG elicitations, which we did later
norms <- ug %>% 
  subset.data.frame(subset = Condition == c(1,2))

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2015Ves034", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# UG Incentivized-Appropriateness first ---------------
# get information on treatment

# cleaning UG
## no choice experiment, only norm elicitation

# 1. Choice dataframe ----


# 2. Beliefs dataframe ----
## subject
## elicit_norms
## frame_tax
## endowment
## action : for subjects in the norm elicitation experiment the keep action being rated.
## norm: for subjects in the norm elicitation experiment, the norm rating. 
## KW scale: -1 = VI, -0.6 = I, -0.2 = SI, 0.2 = SA, 0.6 = A, 1 = VA.

label_col = as.character(seq(0,10,1))

## compute norm 
dg_appropriateness_sum <- norms %>%
  subset.data.frame(subset = Condition == 0) %>%
  group_by(action) %>%
  summarise(coop = sum(norm))
# cbind.data.frame(donation=label_col)

## compute variance norm
dg_norms_var <- norms %>%
  subset.data.frame(subset = frame_tax == 0) %>%
  group_by(action) %>%
  summarise(var_coop = var(norm))
# cbind.data.frame(donation=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "action") %>% 
  subset.data.frame(subset = coop == max(coop)) %>% 
  mutate(PaperID = "2019Cha026", 
         TreatmentCode = 1, 
         avg_NE = action/10,
         var_NE = var_coop) %>%
  subset.data.frame(select = -c(coop, var_coop, action))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))
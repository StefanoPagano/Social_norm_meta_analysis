library(tidyverse)
library(readxl)
rm(list = ls())

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2019Dro082")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

dg=read.csv("Drouvelis-Isen-Marx donations experiment.csv", sep=";")

## the next file contains all data except the conditional DG elicitations, which we did later
norms =read.csv("Drouvelis-Isen-Marx norms experiment.csv", sep=",")


# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2019Dro082", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG BASE ---------------
# get information on treatment

# cleaning DG

## donation : amount donated from earnings
## bonusDonation : amount donated from windfall income
## subjectId : subject in the session
## earnings : results of real-effort task - or bonus income (1-3�)
## bonus3 : Indicator for windfall income of 3 GBP (versus control group with windfall income of 1 GBP)

coldg = c("subjectId","earnings","donation","bonusDonation","bonus3")

# 1. Choice dataframe ----
dg_dta_coop <- dg %>% subset.data.frame(select = coldg) %>%
  mutate(cooperation = donation/earnings) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2019Dro082", TreatmentCode = 1)

# 2. Beliefs dataframe ----
## Subject
## Treatment
## earn_TOT
## KW scale: -1 = VI, -0.6 = I, -0.2 = SI, 0.2 = SA, 0.6 = A, 1 = VA.

label_col = as.character(0,0.1,0.25,0.50,1.00,1.50,2.00)
col_treat = c(1:7,21:24)

## compute norm 
dg_appropriateness_sum <- norms %>%
  subset.data.frame(select = col_treat) %>%
  summarise_at(vars(norm1_0:norm1_200), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation_range=label_col)

## compute variance norm
dg_norms_var <- norms %>%
  summarise_at(vars(norm1_0:norm1_200), var, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation_range=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation_range") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2019Dro082", 
         TreatmentCode = 1, 
         Avg_NE = as.integer(donation_range)/13,
         Var_NE = ..y) %>%
  subset.data.frame(select = -c(..x, ..y, donation_range))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)
## Read in packages
library(Hmisc)
library(foreign)
library(tidyverse)
library(readxl)
rm(list = ls())

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2016Kim003/2016Kim003_data/Data_JEEA_MS_5107")

#pgs=read.csv("PG_Data_bysubj.txt", sep="\t")
tgs=read.csv("TG_Data_bysubj.csv", sep="\t")
#ug=read.csv("UG_Data.txt", sep="\t")
dg=read.csv("DG_Data.csv", sep="\t")
## the next file contains all data except the conditional PG elicitations, which we did later
norms1=read.csv("Norm_Elicitation_Data.csv")

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2016Kim003", select = c(n_Paper, PaperID, TreatmentCode, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma))

# DG -----------------
# get information on treatment

# cleaning DG
## exp_id : date and time of experiment
## exp_num : unique number of the experiment
## subj_id : exp_num * 100 + subj
## subj : subject in the session
## gr_id : unique group id in the dataset
## role : 1 proposer; 0 responder
## sent : amount sent; -1 if not applicable
coldg = c("exp_id","exp_num","subj_id","subj", "role", "sent")

# 1. Choice dataframe ----
dg_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = role == 1) %>% 
  mutate(endowment = 16, cooperation = sent/endowment) %>% 
  summarise(mean_cooperation = mean(cooperation, na.rm =T),
            var_cooperation = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2016Kim003", TreatmentCode = 7)

# 2. Beliefs dataframe ----
## answers[1-9] : kw appropriateness
## actions: send 16/0; 14/2; 12/4; ...; 0/16
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
label_col = as.character(seq(0,16,2))
dg_columns <- c(1, 3, 4, 11:19)
## compute norm 
dg_appropriateness_sum <- norms1 %>% subset.data.frame(select = dg_columns) %>% 
  summarise_at(vars(answers.1.:answers.9.), sum, na.rm=T) %>% t.data.frame() %>% cbind.data.frame(donation=label_col)
# endowment is 16
dg_norm <- as.integer(label_col[which.max(dg_appropriateness_sum)])/16

## compute variance norm
dg_norms_var <- norms1[, dg_columns] %>% 
  summarise_at(vars(answers.1.:answers.9.), var, na.rm=T) %>% t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% mutate(PaperID = "2016Kim003", 
                                                         TreatmentCode = 7, 
                                                         avg_NE = as.integer(donation)/16,
                                                         var_NE = ..y) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine datasets
finaldf <- meta_dataset %>% merge.data.frame(dg_dta_coop, all.x=T) %>% merge.data.frame(final_norms, all.x=T)

# TG ---------------------

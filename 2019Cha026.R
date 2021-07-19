library(tidyverse)
library(readxl)
rm(list = ls())

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2019Cha026")

dg=read_excel("data.xls")

## the next file contains all data except the conditional PG elicitations, which we did later
norms <- dg %>% subset.data.frame(select = c(subject, elicit_norms, frame_tax, endowment, action, norm), subset = c(elicit_norms == 1, endowment == 10))

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2016Kim003", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG -----------------
# get information on treatment

# cleaning DG
## all round with 10 token initial endowment for dictator
## exp_id : date and time of experiment
## exp_num : unique number of the experiment
## subj_id : exp_num * 100 + subj
## subj : subject in the session
## gr_id : unique group id in the dataset
## role : 1 proposer; 0 responder
## sent : amount sent; -1 if not applicable
coldg = c("exp_id","exp_num","subj_id","subj", "role", "sent")
dg_endowment = 10
# 1. Choice dataframe ----
dg_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = role == 1) %>% 
  mutate(dg_endowment, cooperation = sent/dg_endowment) %>% 
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

## compute variance norm
dg_norms_var <- norms1[, dg_columns] %>% 
  summarise_at(vars(answers.1.:answers.9.), var, na.rm=T) %>% t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% mutate(PaperID = "2016Kim003", 
                                                         TreatmentCode = 7, 
                                                         avg_NE = as.integer(donation)/dg_endowment,
                                                         var_NE = ..y) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))
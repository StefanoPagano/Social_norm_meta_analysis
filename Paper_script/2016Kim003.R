## Read in packages
#library(Hmisc)
#library(foreign)
library(tidyverse)
library(readxl)
rm(list = ls())

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2016Kim003/2016Kim003_data/Data_JEEA_MS_5107")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv"

pgg=read.csv("PG_Data_bysubj.txt", sep="\t")
tgs=read.table("TG_Data_byperiod.txt", header = T)
ug=read.table("UG_Data.txt", header = T)
dg=read.csv("DG_Data.csv", sep="\t")
## the next file contains all data except the conditional PG elicitations, which we did later
norms1=read.csv("Norm_Elicitation_Data.csv")

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2016Kim003", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

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

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))

# TG ---------------------
## only first round, only proposer action

coltg = c("exp_id","exp_num","prop_id","sent","per")

# 1. Choice dataframe ----
tg_dta_coop <- tgs %>% subset.data.frame(select = coltg, subset = per== 1) %>% 
  mutate(endowment = 80, cooperation = sent/endowment) %>% 
  summarise(mean_cooperation = mean(cooperation, na.rm =T),
            var_cooperation = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2016Kim003", TreatmentCode = 6)

# 2. Beliefs dataframe ----
## answers[34-42] : kw appropriateness
## actions: send 0;10;20;...;80
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
label_col = as.character(seq(0,80,10))
tg_columns <- c(1, 3, 4, 44:52)
## compute norm 
tg_appropriateness_sum <- norms1 %>% subset.data.frame(select = tg_columns) %>% 
  summarise_at(vars(answers.34.:answers.42.), sum, na.rm=T) %>% t.data.frame() %>% cbind.data.frame(donation=label_col)

## compute variance norm
tg_norms_var <- norms1[, tg_columns] %>% 
  summarise_at(vars(answers.34.:answers.42.), var, na.rm=T) %>% t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

tg_final_norms <- merge.data.frame(tg_appropriateness_sum, tg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% mutate(PaperID = "2016Kim003", 
                                                          TreatmentCode = 6, 
                                                          avg_NE = as.integer(donation)/80,
                                                          var_NE = ..y) %>% 
                                                          subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf =  meta_dataset %>% 
  merge.data.frame(tg_dta_coop, by = c("PaperID","TreatmentCode")) %>% merge.data.frame(tg_final_norms) %>% rbind.data.frame(finaldf)
  # right_join(finaldf, by = c("PaperID","TreatmentCode")) %>% 
  # mutate(avg_NE = coalesce(.[["avg_NE.x"]],  .[["avg_NE.y"]]), 
  #        mean_cooperation = coalesce(.[["mean_cooperation.x"]], .[["mean_cooperation.y"]]),
  #        var_NE = coalesce(.[["var_NE.x"]], .[["var_NE.y"]]),
  #        var_cooperation = coalesce(.[["var_cooperation.x"]], .[["var_cooperation.y"]]))



# UG -----------------
# get information on treatment

# cleaning UG
## exp_id : date and time of experiment
## exp_num : unique number of the experiment
## subj_id : exp_num * 100 + subj
## subj : subject in the session
## gr_id : unique group id in the dataset
## role : 1 proposer; 0 responder
## sent : amount sent; -1 if not applicable
colug = c("exp_id","exp_num","subj_id","subj", "role", "sent")

# 1. Choice dataframe ----
ug_dta_coop <- ug %>% subset.data.frame(select = colug, subset = role == 1) %>% 
  mutate(endowment = 16, cooperation = sent/endowment) %>% 
  summarise(mean_cooperation = mean(cooperation, na.rm =T),
            var_cooperation = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2016Kim003", TreatmentCode = 8)

# 2. Beliefs dataframe ----
## answers[23-31] : kw appropriateness
## actions: send 16/0; 14/2; 12/4; ...; 0/16
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
label_col = as.character(seq(0,16,2))
ug_columns <- c(1, 3, 4, 33:41)
## compute norm 
ug_appropriateness_sum <- norms1 %>% subset.data.frame(select = ug_columns) %>% 
  summarise_at(vars(answers.23.:answers.31.), sum, na.rm=T) %>% t.data.frame() %>% cbind.data.frame(donation=label_col)

## compute variance norm
ug_norms_var <- norms1[, ug_columns] %>% 
  summarise_at(vars(answers.23.:answers.31.), var, na.rm=T) %>% t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

ug_final_norms <- merge.data.frame(ug_appropriateness_sum, ug_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% mutate(PaperID = "2016Kim003", 
                                                          TreatmentCode = 8, 
                                                          avg_NE = as.integer(donation)/16,
                                                          var_NE = ..y) %>% 
                                                          subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf =  meta_dataset %>% 
  merge.data.frame(ug_dta_coop, by = c("PaperID","TreatmentCode")) %>% merge.data.frame(ug_final_norms) %>% rbind.data.frame(finaldf)
# right_join(finaldf, by = c("PaperID","TreatmentCode")) %>% 
#   mutate(avg_NE = coalesce(.[["avg_NE.x"]],  .[["avg_NE.y"]]), 
#          mean_cooperation = coalesce(.[["mean_cooperation.x"]], .[["mean_cooperation.y"]]),
#          var_NE = coalesce(.[["var_NE.x"]], .[["var_NE.y"]]),
#          var_cooperation = coalesce(.[["var_cooperation.x"]], .[["var_cooperation.y"]]))


# PGG (1:Sort_PG)-----------------
# get information on treatment

# cleaning PGG
## exp_id : date and time of experiment
## exp_num : unique number of the experiment
## subj_id : exp_num * 100 + subj
## subj : subject in the session
## treat : select the following number to use the correct TreatmentCode
### treat = 1: RF task, sorting into groups, PG 10 periods with "the rule is" instructions
### treat = 2:  RF task, sorting into groups, PG 10 periods with "no rule" instructions
### treat = 4: PG 10 periods, then  RF task with "rule"
### treat = 5:  RF task, then PG not sorted
### treat = 9: PG 10 periods, then  RF task with "no rule"
## gr_id : unique group id in the dataset
## putin1 : amount put in group account by subject in period 1 of PG
#colpgg = c("exp_id","exp_num","subj_id","subj","treat", "putin1")

# 1. Choice dataframe, no choice for this experiment, that matches norm elicitation ----
pgg_dta_coop <- data.frame(mean_cooperation = NA, var_cooperation = NA) %>% 
  
# pgg %>% subset.data.frame(select = colpgg, subset = treat == 1) %>% 
#   mutate(endowment = 50, cooperation = sent/endowment) %>% 
#   summarise(mean_cooperation = mean(cooperation, na.rm =T),
#             var_cooperation = var(cooperation, na.rm = T)) %>% 
mutate(PaperID = "2016Kim003", TreatmentCode = 9)

# 2. Beliefs dataframe ----
## answers[12-21] : kw appropriateness
## actions: send 16/0; 14/2; 12/4; ...; 0/16
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
label_col = as.character(seq(0,50,5))
pgg_columns <- c(1, 3, 4, 22:32)
### compute norm 
pgg_appropriateness_sum <- norms1 %>% subset.data.frame(select = pgg_columns) %>% 
  summarise_at(vars(answers.12.:answers.22.), sum, na.rm=T) %>% t.data.frame() %>% cbind.data.frame(donation=label_col)

### compute variance norm
pgg_norms_var <- norms1[, pgg_columns] %>% 
  summarise_at(vars(answers.12.:answers.22.), var, na.rm=T) %>% t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

pgg_final_norms <- merge.data.frame(pgg_appropriateness_sum, pgg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% mutate(PaperID = "2016Kim003", 
                                                          TreatmentCode = 9, 
                                                          avg_NE = as.integer(donation)/50,
                                                          var_NE = ..y) %>% 
                                                          subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf =  meta_dataset %>% 
  merge.data.frame(pgg_dta_coop, by = c("PaperID","TreatmentCode")) %>% merge.data.frame(pgg_final_norms) %>% rbind.data.frame(finaldf)


write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID, "_finaldf.csv")))

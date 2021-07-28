## Read in packages
library(tidyverse)
library(readxl)
rm(list = ls())

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Gac013/2017Gac013_data")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

dg=read_xls("data_all.xls") %>% subset.data.frame(subset = B_take == 0, select = 1:9)
tog = read_xls("data_all.xls") %>% subset.data.frame(subset = B_take == 1, select = 1:9)

norms1_dg=read_xls("data_all.xls") %>% subset.data.frame(select = 10:37, subset = N_take == 0)
norms1_tog=read_xls("data_all.xls") %>% subset.data.frame(select = 10:37, subset = N_take == 1)

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2016Kim003", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG -----------------
# get information on treatment

# cleaning DG
## B_session_id : date and time of experiment of behavior
## B_take : dummy variable; 1 if take or give game, 0 if give only game (DG)
## B_studentid : id of student
## B_treatment : Peer or not peer
## B_D2_sharingX : amount sent (from 0 to 4) by Dictator number 2, conditional on dictator number 1 giving X
## Computed --> avg_sent : median of amount sent across the 5 scenarios in the strategy method;
## endowment : actual endowment is 12 

# cleaning ToG
## B_session_id : date and time of experiment of behavior
## B_take : dummy variable; 1 if take or give game, 0 if give only game (DG)
## B_studentid : id of student
## B_treatment : Peer or not peer
## B_D2_sharingX : amount sent (from 0 to 4) by Dictator number 2, conditional on dictator number 1 giving X
## Computed --> avg_sent : median of amount sent across the 5 scenarios in the strategy method;
## endowment : actual endowment of D2 is 9

# 1. Choice dataframe ----
dg_dta_coop <- dg %>% rowwise() %>% mutate(avg_sent = median(c(B_D2sharing0, B_D2sharing1, B_D2sharing2, B_D2sharing3, B_D2sharing4))) %>%
  mutate(endowment = 12, cooperation = avg_sent/endowment) %>% group_by(B_treatment) %>%
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Gac013", TreatmentCode = ifelse(B_treatment=="PEER",1,2)) %>% subset.data.frame(select = -c(B_treatment))

tog_dta_coop <- tog %>% rowwise() %>%
  mutate(avg_sent = median(c(B_D2sharing0, B_D2sharing1, B_D2sharing2, B_D2sharing3, B_D2sharing4))-3) %>%
  mutate(endowment_d = 12, endowment_r = 6, sum_endow = endowment_d+endowment_r,
         cooperation = endowment_r/sum_endow + avg_sent/sum_endow) %>% 
  group_by(B_treatment) %>%
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Gac013", TreatmentCode = ifelse(B_treatment=="PEER",3,4)) %>% subset.data.frame(select = -c(B_treatment))

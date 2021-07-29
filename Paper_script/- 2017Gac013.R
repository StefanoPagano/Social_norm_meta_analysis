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
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2017Gac013", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

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
dg_dta_coop <- dg %>% rowwise() %>% 
  mutate(avg_sent = median(c(B_D2sharing0, B_D2sharing1, B_D2sharing2, B_D2sharing3, B_D2sharing4))) %>%
  mutate(endowment = 12, cooperation = avg_sent/endowment) %>% group_by(B_treatment) %>%
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Gac013", TreatmentCode = ifelse(B_treatment=="PEER",1,2)) %>% subset.data.frame(select = -c(B_treatment))

tog_dta_coop <- tog %>% rowwise() %>%
  mutate(avg_sent = median(c(B_D2sharing0, B_D2sharing1, B_D2sharing2, B_D2sharing3, B_D2sharing4))-3) %>%
  mutate(endowment_d = 9, endowment_r = 6, sum_endow = endowment_d+endowment_r,
         cooperation = endowment_r/sum_endow + avg_sent/sum_endow) %>% 
  group_by(B_treatment) %>%
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Gac013", TreatmentCode = ifelse(B_treatment=="PEER",3,4)) %>% subset.data.frame(select = -c(B_treatment))

# 2. Beliefs dataframe ----
## DG
## actions: send 0;10;20;...;80
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
## compute norm 
label_col = as.character(seq(0,4,1))
dg_appropriateness_sum <- norms1_dg %>% 
  rowwise() %>% 
  mutate(give_0 = median(c(N_norm11, N_norm21, N_norm31,N_norm41, N_norm51)),
         give_1 = median(c(N_norm12, N_norm22, N_norm32,N_norm42, N_norm52)),
         give_2 = median(c(N_norm13, N_norm23, N_norm33,N_norm43, N_norm53)),
         give_3 = median(c(N_norm14, N_norm24, N_norm34,N_norm44, N_norm54)),
         give_4 = median(c(N_norm15, N_norm25, N_norm35,N_norm45, N_norm55))) %>% group_by(N_treatment) %>%
  summarise_at(vars(give_0:give_4), sum, na.rm=T) %>%
  pivot_longer(cols = c(give_0, give_1, give_2, give_3, give_4), names_to = "Donation") %>% mutate(sent=recode(Donation, give_0 = 0, give_1 = 1, give_2 =2, give_3 = 3, give_4 = 4))

## compute variance norm
dg_norms_var <- norms1_dg %>% 
  rowwise() %>% 
  mutate(give_0 = median(c(N_norm11, N_norm21, N_norm31,N_norm41, N_norm51)),
         give_1 = median(c(N_norm12, N_norm22, N_norm32,N_norm42, N_norm52)),
         give_2 = median(c(N_norm13, N_norm23, N_norm33,N_norm43, N_norm53)),
         give_3 = median(c(N_norm14, N_norm24, N_norm34,N_norm44, N_norm54)),
         give_4 = median(c(N_norm15, N_norm25, N_norm35,N_norm45, N_norm55))) %>% group_by(N_treatment) %>%
  summarise_at(vars(give_0:give_4), var, na.rm=T) %>%
  pivot_longer(cols = c(give_0, give_1, give_2, give_3, give_4), names_to = "Donation") %>%
  mutate(sent=recode(Donation, give_0 = 0, give_1 = 1, give_2 =2, give_3 = 3, give_4 = 4))

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = c("sent", "N_treatment")) %>%
  subset.data.frame(subset = value.x == max(value.x[N_treatment == 0]) | value.x == max(value.x[N_treatment == 1])) %>% 
  mutate(PaperID = "2017Gac013", 
         TreatmentCode = ifelse(N_treatment == 0, 2, 1), 
         Avg_NE = as.integer(sent)/12,
         Var_NE = value.y) %>% 
  subset.data.frame(select = -c(sent, value.x, value.y, Donation.x, Donation.y, N_treatment))

# 3. combine dataset ----
finaldf =  meta_dataset %>% 
  merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))

## ToG
## actions: send 0;10;20;...;80
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
## compute norm 
tog_appropriateness_sum <- norms1_tog %>% 
  rowwise() %>% 
  mutate(give_0 = median(c(N_norm11, N_norm21, N_norm31,N_norm41, N_norm51)),
         give_1 = median(c(N_norm12, N_norm22, N_norm32,N_norm42, N_norm52)),
         give_2 = median(c(N_norm13, N_norm23, N_norm33,N_norm43, N_norm53)),
         give_3 = median(c(N_norm14, N_norm24, N_norm34,N_norm44, N_norm54)),
         give_4 = median(c(N_norm15, N_norm25, N_norm35,N_norm45, N_norm55))) %>% group_by(N_treatment) %>%
  summarise_at(vars(give_0:give_4), sum, na.rm=T) %>%
  pivot_longer(cols = c(give_0, give_1, give_2, give_3, give_4), names_to = "Donation") %>% mutate(sent=recode(Donation, give_0 = -3, give_1 = -2, give_2 =-1, give_3 = 0, give_4 = 1))

## compute variance norm
tog_norms_var <- norms1_tog %>% 
  rowwise() %>% 
  mutate(give_0 = median(c(N_norm11, N_norm21, N_norm31,N_norm41, N_norm51)),
         give_1 = median(c(N_norm12, N_norm22, N_norm32,N_norm42, N_norm52)),
         give_2 = median(c(N_norm13, N_norm23, N_norm33,N_norm43, N_norm53)),
         give_3 = median(c(N_norm14, N_norm24, N_norm34,N_norm44, N_norm54)),
         give_4 = median(c(N_norm15, N_norm25, N_norm35,N_norm45, N_norm55))) %>% group_by(N_treatment) %>%
  summarise_at(vars(give_0:give_4), var, na.rm=T) %>%
  pivot_longer(cols = c(give_0, give_1, give_2, give_3, give_4), names_to = "Donation") %>%
  mutate(sent=recode(Donation, give_0 = -3, give_1 = -2, give_2 =-1, give_3 = 0, give_4 = 1))

tog_final_norms <- merge.data.frame(tog_appropriateness_sum, tog_norms_var, by = c("sent", "N_treatment")) %>%
  subset.data.frame(subset = value.x == max(value.x[N_treatment == 0]) | value.x == max(value.x[N_treatment == 1])) %>% 
  mutate(PaperID = "2017Gac013", 
         TreatmentCode = ifelse(N_treatment == 0, 4, 3), 
         endowment_d = 9, 
         endowment_r = 6, 
         sum_endow = endowment_d+endowment_r,
         Avg_NE = endowment_r/sum_endow + as.integer(sent)/sum_endow,
         Var_NE = value.y) %>% 
  subset.data.frame(select = -c(sent, value.x, value.y, Donation.x, Donation.y, N_treatment, endowment_r, sum_endow, endowment_d))

## Final dataset merging
finaldf <- meta_dataset %>% 
  merge.data.frame(tog_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(tog_final_norms) %>% 
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)
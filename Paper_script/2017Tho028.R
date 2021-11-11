# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Tho028")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

dg=read_excel("libcons_alldata.xlsx", sheet = "alldata")

## the next file contains all data except the conditional PG elicitations, which we did later
#norms <- dg %>% subset.data.frame()

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2017Tho028", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG Base-----------------
# get information on treatment

# cleaning DG
## exp_id		ztree timestamp of the session
## exp_num		unique number of the session
## Subject		session specific subject number
## subj		unique subject number
## treat   1: BASE treatment (standard DG, with constraint to offer no more than half pie)
##         2: ASYM1 treatment (same as 1, 	computer allocates with prob 1/3 instead of Dictator,
##                    Receiver does not know who proposed)
##         3: ASYM2 treatment (same as ASYM1, Dictator can propose any amount)
## sent		>=0: the amount sent to the receiver
## role : 1 proposer; 0 responder
## decider		-1: Receiver; 0: Dictator who does not choose; 1: Dictator who chooses

coldg = c("exp_id","exp_num","Subject","subj", "treat", "role", "decider", "sent")

# 1. Choice dataframe ----
dgb_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = role == 1) %>%
  subset.data.frame(subset = decider == 1) %>%
  subset.data.frame(subset = treat == 1) %>%
  mutate(endowment = 20, cooperation = sent/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Tho028", TreatmentCode = 1)

# 2. Beliefs dataframe ----
## base[0-5] : kw appropriateness
## actions: send 0,2,4,6,8,10
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA

label_col = as.character(seq(0,10,2))
dg_columns <- c(1:10, 28, 33, 34)
n_sub_N = dg %>% subset.data.frame(select = dg_columns) %>%
  subset.data.frame(subset = treat == 1) %>%
  subset.data.frame(subset = role == 1) %>% summarise(n_sub_N = n())

## compute norm 
dgb_appropriateness_sum <- dg %>% subset.data.frame(select = dg_columns) %>%
  subset.data.frame(subset = treat == 1) %>%
  subset.data.frame(subset = role == 1) %>%
  summarise_at(vars(base0:base5), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

## compute variance norm
dgb_norms_var <- dg[, dg_columns] %>%
  subset.data.frame(subset = treat == 1) %>%
  subset.data.frame(subset = role == 1) %>%
  summarise_at(vars(base0:base5), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dgb_final_norms <- merge.data.frame(dgb_appropriateness_sum, dgb_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2017Tho028", 
         TreatmentCode = 1, 
         Avg_NE = as.integer(donation)/20,
         Var_NE = ..y,
         Sd_Avg_NE = sd(dgb_appropriateness_sum$Kw_m)) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dgb_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dgb_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m))


# DG Asym1-----------------
# get information on treatment

# cleaning DG
## exp_id		ztree timestamp of the session
## exp_num		unique number of the session
## Subject		session specific subject number
## subj		unique subject number
## treat   1: BASE treatment (standard DG, with constraint to offer no more than half pie)
##         2: ASYM1 treatment (same as 1, 	computer allocates with prob 1/3 instead of Dictator,
##                    Receiver does not know who proposed)
##         3: ASYM2 treatment (same as ASYM1, Dictator can propose any amount)
## sent		>=0: the amount sent to the receiver
## role : 1 proposer; 0 responder
## decider		-1: Receiver; 0: Dictator who does not choose; 1: Dictator who chooses

coldg = c("exp_id","exp_num","Subject","subj", "treat", "role", "decider", "sent")

# 1. Choice dataframe ----
dga_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = role == 1) %>%
  subset.data.frame(subset = decider == 1) %>%
  subset.data.frame(subset = treat == 2) %>%
  mutate(endowment = 20, cooperation = sent/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Tho028", TreatmentCode = 2)

# 2. Beliefs dataframe ----
## base[0-5] : kw appropriateness
## actions: send 0,2,4,6,8,10
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA

label_col = as.character(seq(0,10,2))
dg_columns <- c(1:4, 11:16, 28, 33, 34)
n_sub_N = dg %>% subset.data.frame(select = dg_columns) %>%
  subset.data.frame(subset = treat == 2) %>%
  subset.data.frame(subset = role == 1) %>% summarise(n_sub_N = n())

## compute norm 
dga_appropriateness_sum <- dg %>% subset.data.frame(select = dg_columns) %>%
  subset.data.frame(subset = treat == 2) %>%
  subset.data.frame(subset = role == 1) %>%
  summarise_at(vars(asym1_0:asym1_5), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

## compute variance norm
dga_norms_var <- dg[, dg_columns] %>%
  subset.data.frame(subset = treat == 2) %>%
  subset.data.frame(subset = role == 1) %>%
  summarise_at(vars(asym1_0:asym1_5), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dga_final_norms <- merge.data.frame(dga_appropriateness_sum, dga_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2017Tho028", 
         TreatmentCode = 2, 
         Avg_NE = as.integer(donation)/20,
         Var_NE = ..y,
         Sd_Avg_NE = sd(dga_appropriateness_sum$Kw_m)) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dga_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dga_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)


write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)
# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2018Her061")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

#file of choice and norms
dg=read_excel("meta.xlsx", sheet = "behavior")
norms = read_excel("meta.xlsx", sheet = "sn")


# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2018Her061", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG GENExperiment_Money-----------------
# get information on treatment

# cleaning DG
## Subject: incremental number that indicates the subject number
## monetary: monetary experiment (1); non-monetary experiment (0)
## action: amount of euros given to the other person


coldg = c("Subject","monetary","action")

# 1. Choice dataframe ----
dg_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = monetary == 1) %>%
  mutate(endowment = 10, cooperation = action/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2018Her061", TreatmentCode = 9)

# 2. Beliefs dataframe ----
## SUBJECTS
## EXP : 1. monetary experiment; 2. non-monetary experiment
## SA_X : kw appropriateness of different scenarios
## KW scale: -1: VI; -1/3: I; 1/3: A; 1: VA

label_col = as.character(seq(0,10,1))
norms_columns <- c(1:13)

## compute norm 
dg_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = EXP == 1) %>%
  summarise_at(vars(SA_0:SA_10), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

## compute variance norm
dg_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = EXP == 1) %>%
  summarise_at(vars(SA_0:SA_10), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2018Her061", 
         TreatmentCode = 9, 
         Avg_NE = as.integer(donation)/10,
         Var_NE = ..y) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)


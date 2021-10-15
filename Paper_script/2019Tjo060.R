# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2019Tjo060")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

# norm files - experiment 8 and 9
norms_8=read_excel("JBEE_Experiment_8_Norm.xlsx", sheet = "Sheet1")
norms_9=read_excel("JBEE_Experiment_9_Norm.xlsx", sheet = "Sheet1")

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2019Tjo060", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# Experiment 8 - DG Norm-----------------
# get information on treatment

# cleaning DG
## ID
## Sesjion: session
## Mann: gender
## Erfaring: experience, Number of particapting in experimeriments? 0 = none, 1 = once, 2 = twice, and 3 = 3 or more.
## player.decision10_0

ex8_dg_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2019Tjo060", TreatmentCode = 8)

# 2. Beliefs dataframe ----
## KW: send from 0 to 800, step 200
## KW scale: -1: VI; -1/3: I; 1/3: A; 1: VA

label_col = as.character(seq(0,800,200))
norms_columns <- c(1:10)

## compute norm 
ex8_dg_appropriateness_sum <- norms_8 %>% subset.data.frame(select = norms_columns) %>%
  summarise_at(vars(S4H1:S4H5), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

## compute variance norm
ex8_dg_norms_var <- norms_8[, norms_columns] %>%
  summarise_at(vars(S4H1:S4H5), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

ex8_dg_final_norms <- merge.data.frame(ex8_dg_appropriateness_sum, ex8_dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2019Tjo060", 
         TreatmentCode = 8, 
         Avg_NE = as.integer(donation)/800,
         Var_NE = ..y) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))



# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(ex8_dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(ex8_dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))

# Experiment 9 - DG Norm-----------------
# get information on treatment

# cleaning DG
## i: player code
## Semester = numbers of semester as student enrolled , 1= one semester, 2 = 2 semesters,.4= 3 and more semesters.
## Mann: gender
## Erfaring: experience, Number of particapting in experimeriments? 0 = none, 1 = once, 2 = twice, and 3 = 3 or more.
## player.decision10_0

ex9_dg_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>%
  mutate(PaperID = "2019Tjo060", TreatmentCode = 9)

# 2. Beliefs dataframe ----
## KW: send from 0 to 800, step 200
## KW scale: -1: VI; -1/3: I; 1/3: A; 1: VA

label_col = as.character(seq(0,800,200))
norms_columns <- c(1:9)

## compute norm 
ex9_dg_appropriateness_sum <- norms_9 %>% subset.data.frame(select = norms_columns) %>%
  summarise_at(vars(Handling1:Handling5), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

## compute variance norm
ex9_dg_norms_var <- norms_9[, norms_columns] %>%
  summarise_at(vars(Handling1:Handling5), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

ex9_dg_final_norms <- merge.data.frame(ex9_dg_appropriateness_sum, ex9_dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2019Tjo060", 
         TreatmentCode = 9, 
         Avg_NE = as.integer(donation)/800,
         Var_NE = ..y) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(ex9_dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(ex9_dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)


library(tidyverse)
library(readxl)
rm(list = ls())

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Sen116")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
dg=read_excel("Senci_Breccia_Freidin_Raw data.xlsx", sheet = "Hoja1")

colnames(dg) <- c("Participant",	"Treatment",	"Kept", "Gender", "Age_Studies", "N_known_participants",	"N_previous_participation",	"Right_DR",	"Left_DR",	"Right_Quadr_Diff_DR",	"Left_Quadr_Diff_DR",	"Politics")


# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2017Sen116", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG Baseline-----------------
# get information on treatment

# cleaning DG
## Participant :  subject code
## Treatment: Control or PN
## DG Decision (money kept) -> Kept

coldg = c("Participant","Treatment","Kept")

# 1. Choice dataframe ----
dg_base_dta_coop <- dg %>% subset.data.frame(select = coldg, Treatment == "Control") %>%
  mutate(endowment = 50, cooperation = (endowment-Kept)/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Sen116", TreatmentCode = 1)


# 2. Beliefs dataframe ----
dg_base_final_norms <- data.frame(Avg_NE = NA, Var_NE = NA) %>%
  mutate(PaperID = "2017Sen116", TreatmentCode = 1)


# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dg_base_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_base_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))

# DG PN-----------------
# get information on treatment

# cleaning DG
## Participant :  subject code
## Treatment: Control or PN
## DG Decision (money kept) -> Kept

coldg = c("Participant","Treatment","Kept")

# 1. Choice dataframe ----
dg_PN_dta_coop <- dg %>% subset.data.frame(select = coldg, Treatment == "PN") %>%
  mutate(endowment = 50, cooperation = (endowment-Kept)/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Sen116", TreatmentCode = 2)


# 2. Beliefs dataframe ----
dg_PN_final_norms <- data.frame(Avg_NE = NA, Var_NE = NA) %>%
  mutate(PaperID = "2017Sen116", TreatmentCode = 2)


# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dg_PN_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_PN_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

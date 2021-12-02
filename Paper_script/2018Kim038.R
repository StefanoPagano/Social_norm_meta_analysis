# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2018Kim038")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
dg=read_excel("data-compiled.xls", sheet = "data-changed")

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2018Kim038", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG Neutral-----------------
# get information on treatment

# cleaning DG
## exp_id
## where: Italy (4): mini game; USA (1)/Canada (2): ToG frame; Netherlands (0): DG Neutral; Turkey (3): ??
## session
## kept
## sent

coldg = c("exp_id","where","session","sent")

# 1. Choice dataframe ----
dg_neutral_dta_coop <- dg %>% subset.data.frame(select = coldg, where == 0) %>%
  mutate(endowment = 100, cooperation = sent/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2018Kim038", TreatmentCode = 1)


# 2. Beliefs dataframe ----
dg_neutral_final_norms <- data.frame(Avg_NE = NA, Var_NE = NA, Sd_Avg_NE = NA, Sd_Avg_NE_min_max = NA, specificity = NA) %>%
  mutate(PaperID = "2018Kim038", TreatmentCode = 1)

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dg_neutral_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_neutral_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))

# DG ToG frame-----------------
# get information on treatment

# cleaning DG
## exp_id
## where: Italy (4): mini game; USA (1)/Canada (2): ToG frame; Netherlands (0): DG Neutral; Turkey (3): ??
## session
## kept
## sent

coldg = c("exp_id","where","session","sent")

# 1. Choice dataframe ----
dg_tog_dta_coop <- dg %>% subset.data.frame(select = coldg, where == 1 | where == 2) %>%
  mutate(endowment = 100, cooperation = sent/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2018Kim038", TreatmentCode = 2)

# 2. Beliefs dataframe ----
dg_tog_final_norms <- data.frame(Avg_NE = NA, Var_NE = NA, Sd_Avg_NE = NA, Sd_Avg_NE_min_max = NA, specificity = NA) %>%
  mutate(PaperID = "2018Kim038", TreatmentCode = 2)

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dg_tog_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_tog_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

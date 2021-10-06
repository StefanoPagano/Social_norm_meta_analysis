# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2020And089")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
dg=read_excel("2020And089_data.xlsx", sheet = "public decisions data")
# only columns needed
dg <- dg %>% subset.data.frame(select = c(1,4,19:20))

# rename columns
colnames(dg) <- c("Treatment", "sent", "Age", "Experiment")

# add id variable as a progressive numbers
dg <- dg %>% mutate(id = c(1:374))

# norms file
norms=read_excel("DecisionRevealProsocial_NormElicitationData.xlsx", sheet = "datanew")

# rename columns
colnames(norms) <- c("Condition", "KW00", "KW10", "KW20", "KW30", "KW40", "KW50", "AppropPGG_COOPERATE", "AppropPGG_DEFECT","Approp_Switch","Approp_DontSwitch","Approp_Suffocate","AppropDontSuffocate","Common_DonateChoice","Common_DonateAverage","Common_PGG","Common_UtilSwitch","Common_UtilSuffocateBaby","Age","Gender")

# recoding
norms <- norms %>% mutate(id = c(1:195)) %>%
  mutate(sent = recode(Common_DonateChoice, `1` = 0, 
                                            `2` = 10, 
                                            `3` = 20, 
                                            `4` = 30, 
                                            `5` = 40, 
                                            `6` = 50)) %>%
  mutate(KW00_M = recode(KW00, `-2` = -1, `-1` = -1/3, `1` = 1/3, `2` = 1),
         KW10_M = recode(KW10, `-2` = -1, `-1` = -1/3, `1` = 1/3, `2` = 1),
         KW20_M = recode(KW20, `-2` = -1, `-1` = -1/3, `1` = 1/3, `2` = 1),
         KW30_M = recode(KW30, `-2` = -1, `-1` = -1/3, `1` = 1/3, `2` = 1),
         KW40_M = recode(KW40, `-2` = -1, `-1` = -1/3, `1` = 1/3, `2` = 1),
         KW50_M = recode(KW50, `-2` = -1, `-1` = -1/3, `1` = 1/3, `2` = 1))

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2020And089", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG Private-----------------
# get information on treatment

# cleaning DG
## Treatment :  1 if private; or 2 if public
## id: progressive number created in script
## sent : How much money (SEK) player decide to donate to the carity

coldg = c("id","Treatment","sent")

# 1. Choice dataframe ----
dg_dta_coop <- dg %>% subset.data.frame(select = coldg, Treatment == 1) %>%
  mutate(endowment = 50, cooperation = sent/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2020And089", TreatmentCode = 1)
  

# 2. Beliefs dataframe ----
## KW: send 0,10,20,30,40,50
## id
## Condition
## KW scale: -2: VI; -1: I; 1: A; 2: VA -> recoding done.

label_col = as.character(seq(0,50,10))
norms_columns <- c(1,21,23:28)

## compute norm 
dg_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 0) %>%
  summarise_at(vars(KW00_M:KW50_M), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

## compute variance norm
dg_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = Condition == 0) %>%
  summarise_at(vars(KW00_M:KW50_M), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020And089", 
         TreatmentCode = 1, 
         Avg_NE = as.integer(donation)/50,
         Var_NE = ..y) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))

# DG Public-----------------
# get information on treatment

# cleaning DG
## Condition :  0 if private; or 1 if public
## id: progressive number created in script
## Common_DonateChoice : How much money (SEK) player decide to donate to the carity

coldg = c("id","Treatment","sent")

# 1. Choice dataframe ----
dg2_dta_coop <- dg %>% subset.data.frame(select = coldg, Treatment == 2) %>%
  mutate(endowment = 50, cooperation = sent/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2020And089", TreatmentCode = 2)


# 2. Beliefs dataframe ----
## KW: send 0,10,20,30,40,50
## id
## Condition
## KW scale: -2: VI; -1: I; 1: A; 2: VA -> recoding done.

label_col = as.character(seq(0,50,10))
dg_columns <- c(1,21,23:28)

## compute norm 
dg_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 1) %>%
  summarise_at(vars(KW00_M:KW50_M), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

## compute variance norm
dg_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = Condition == 1) %>%
  summarise_at(vars(KW00_M:KW50_M), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg2_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020And089", 
         TreatmentCode = 2, 
         Avg_NE = as.integer(donation)/50,
         Var_NE = ..y) %>% 
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg2_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg2_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

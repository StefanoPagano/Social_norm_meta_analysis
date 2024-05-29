# set wd 
setwd("G:/Mon Drive/Meta-analysis beliefs/Dati paper/2020And089")

csv_path_output <- "~/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
dg=read_excel("2020And089_data.xlsx", sheet = "public decisions data")
# only columns needed
dg <- dg %>% subset.data.frame(select = c(1,4,6,19:20))

# rename columns
colnames(dg) <- c("Treatment", "sent", "Pdg_coop", "Age", "Experiment")

# add id variable as a progressive numbers
dg <- dg %>% mutate(id = c(1:374)) %>%
  mutate(Pdg_cooperate = dplyr::recode(Pdg_coop, `1` = 0, `2` = 1 ))

# norms file
norms=read_excel("DecisionRevealProsocial_NormElicitationData.xlsx", sheet = "datanew")

# rename columns
colnames(norms) <- c("Condition", "KW00", "KW10", "KW20", "KW30", "KW40", "KW50", "AppropPGG_COOPERATE", "AppropPGG_DEFECT","Approp_Switch","Approp_DontSwitch","Approp_Suffocate","AppropDontSuffocate","Common_DonateChoice","Common_DonateAverage","Common_PGG","Common_UtilSwitch","Common_UtilSuffocateBaby","Age","Gender")

# recoding
norms <- norms %>% mutate(id = c(1:195)) %>%
  mutate(sent = dplyr::recode(Common_DonateChoice, `1` = 0, `2` = 10, `3` = 20, `4` = 30, `5` = 40, `6` = 50)) %>%
  mutate(KW00_M = dplyr::recode(KW00, `-1` = -1/3, `-2` = -1, `1` = 1/3, `2` = 1),
         KW10_M = dplyr::recode(KW10, `-1` = -1/3, `-2` = -1, `1` = 1/3, `2` = 1),
         KW20_M = dplyr::recode(KW20, `-1` = -1/3, `-2` = -1, `1` = 1/3, `2` = 1),
         KW30_M = dplyr::recode(KW30, `-1` = -1/3, `-2` = -1, `1` = 1/3, `2` = 1),
         KW40_M = dplyr::recode(KW40, `-1` = -1/3, `-2` = -1, `1` = 1/3, `2` = 1),
         KW50_M = dplyr::recode(KW50, `-1` = -1/3, `-2` = -1, `1` = 1/3, `2` = 1),
         KWPDG_Coop = dplyr::recode(AppropPGG_COOPERATE,`-1` = -1/3, `-2` = -1, `1` = 1/3, `2` = 1),
         KWPDG_Def = dplyr::recode(AppropPGG_DEFECT, `-1` = -1/3, `-2` = -1, `1` = 1/3, `2` = 1),
         EE_PDG = dplyr::recode(Common_PGG, `1` = 1, `2` = 0))

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/Mon Drive/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2020And089", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# Donation G Private-----------------
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
norms_columns <- c(1,15,16,21,23:31)
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 0) %>% summarise(n_sub_N = n())

## compute norm 
dg_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 0) %>%
  summarise_at(vars(KW00_M:KW50_M), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- dg_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020And089", TreatmentCode = 1)

positive_appropriateness <- dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dg_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dg_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
dg_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = Condition == 0) %>%
  summarise_at(vars(KW00_M:KW50_M), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_EE <- norms [, norms_columns] %>%
  subset.data.frame(subset = Condition == 0) %>% 
  summarise(Avg_EE = mean(Common_DonateAverage, na.rm =T))

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020And089", 
         TreatmentCode = 1, 
         Avg_NE = as.integer(donation)/50,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(dg_appropriateness_sum$Kw_m),
         Avg_EE = as.numeric(dg_EE/50),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m))

# Donation G Public-----------------
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
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 1) %>% summarise(n_sub_N = n())

## compute norm 
dg2_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 1) %>%
  summarise_at(vars(KW00_M:KW50_M), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- dg2_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020And089", TreatmentCode = 2) %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- dg2_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dg2_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dg2_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dg2_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
dg2_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = Condition == 1) %>%
  summarise_at(vars(KW00_M:KW50_M), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg2_EE <- norms [, norms_columns] %>%
  subset.data.frame(subset = Condition == 1) %>% 
  summarise(Avg_EE = mean(Common_DonateAverage, na.rm =T))

dg2_final_norms <- merge.data.frame(dg2_appropriateness_sum, dg2_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020And089", 
         TreatmentCode = 2, 
         Avg_NE = as.integer(donation)/50,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(dg2_appropriateness_sum$Kw_m),
         Avg_EE = as.numeric(dg2_EE/50),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg2_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg2_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# PDG Private (mini PGG)----------------- 
#### dplyr::recode di defect (F), calcolo di quanti fanno cooperate
# get information on treatment
# cleaning PGG
## Treatment :  1 if private; or 2 if public
## id: progressive number created in script
## contribution : How much money (SEK) player decide to contribute

colpdg = c("id","Treatment","Pdg_cooperate", "Experiment")

# 1. Choice dataframe ----
pdg_dta_coop <- dg %>% subset.data.frame(select = colpdg, subset = Treatment == 1) %>%
  subset.data.frame(subset = Experiment == 2) %>%
  summarise(Avg_coop = mean(Pdg_cooperate, na.rm =T), Var_coop = var(Pdg_cooperate, na.rm = T)) %>% 
  mutate(PaperID = "2020And089", TreatmentCode = 3)


# 2. Beliefs dataframe ----
label_col <- c(1, 0) # 1=cooperate, 0=defect
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 1) %>% summarise(n_sub_N = n())

## compute norm 
pdg_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 1) %>%
  summarise_at(vars(KWPDG_Coop:KWPDG_Def), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- pdg_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020And089", TreatmentCode = 3) %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- pdg_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(pdg_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- pdg_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- pdg_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
pdg_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = Condition == 1) %>%
  summarise_at(vars(KWPDG_Coop:KWPDG_Def), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

pdg_EE <- norms[, norms_columns] %>%
  subset.data.frame(subset = Condition == 1) %>% 
  summarise(Avg_EE = mean(EE_PDG, na.rm =T))

pdg_final_norms <- merge.data.frame(pdg_appropriateness_sum, pdg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020And089", 
         TreatmentCode = 3, 
         Avg_NE = as.integer(donation),
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(pdg_appropriateness_sum$Kw_m),
         Avg_EE = as.numeric(pdg_EE),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(pdg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(pdg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)


# PGG Private-----------------
# get information on treatment

# cleaning PGG
## Treatment :  1 if private; or 2 if public
## id: progressive number created in script
## contribution : How much money (SEK) player decide to contribute

colpdg = c("id","Treatment","Pdg_cooperate", "Experiment")

# 1. Choice dataframe ----
pdg2_dta_coop <- dg %>% subset.data.frame(select = colpdg, subset = Treatment == 2) %>%
  subset.data.frame(subset = Experiment == 2) %>%
  summarise(Avg_coop = mean(Pdg_cooperate, na.rm =T), Var_coop = var(Pdg_cooperate, na.rm = T)) %>% 
  mutate(PaperID = "2020And089", TreatmentCode = 4)

# 2. Beliefs dataframe ----
## compute norm 
n_sub_N = norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 0) %>% summarise(n_sub_N = n())

pdg2_appropriateness_sum <- norms %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Condition == 0) %>%
  summarise_at(vars(KWPDG_Coop:KWPDG_Def), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- pdg2_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2020And089", TreatmentCode = 4) %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- pdg2_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(pdg2_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- pdg2_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- pdg2_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
pdg2_norms_var <- norms[, norms_columns] %>%
  subset.data.frame(subset = Condition == 0) %>%
  summarise_at(vars(KWPDG_Coop:KWPDG_Def), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

pdg2_EE <- norms[, norms_columns] %>%
  subset.data.frame(subset = Condition == 0) %>% 
  summarise(Avg_EE = mean(EE_PDG, na.rm =T))

pdg2_final_norms <- merge.data.frame(pdg2_appropriateness_sum, pdg2_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2020And089", 
         TreatmentCode = 4, 
         Avg_NE = as.integer(donation),
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(pdg2_appropriateness_sum$Kw_m),
         Avg_EE = as.numeric(pdg2_EE),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(pdg2_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(pdg2_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

write.csv(db_appropriateness, file = paste(csv_path_output, paste(db_appropriateness$PaperID[1], "_avg_kw.csv", sep = ""), sep = ""), row.names = F)

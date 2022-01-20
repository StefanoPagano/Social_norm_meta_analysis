# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Ves049")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
pgg=read_excel("2017Ves049_data.xlsx", sheet = "Individual level data", skip = 3)

colnames(pgg) <- c("Subject",	"Group",	"Treatment",	"R1",	"R2",	"R3",	"R4",	"R5",	"R6",	"R7",	"R8",	"R9",	"R10",	"R11",	"R12",	"R13",	"R14",	"R15",	"R16",	"R17",	"R18",	"R19",	"R20",	"KW20",	"KW15", "KW10",	"KW5",	"Age", "Gender",	"Major",	"Y_I_euro")

# recoding
pgg <- pgg %>%
  mutate(KW20_re = recode(KW20, `1` = -1, `2` = -0.3, `3` = 0.3, `4` = 1),
         KW15_re = recode(KW15, `1` = -1, `2` = -0.3, `3` = 0.3, `4` = 1),
         KW10_re = recode(KW10, `1` = -1, `2` = -0.3, `3` = 0.3, `4` = 1),
         KW5_re = recode(KW5, `1` = -1, `2` = -0.3, `3` = 0.3, `4` = 1))

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2017Ves049", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# PGG TREATMENT 1-----------------
# get information on treatment

# cleaning DG
## exp_id
## where: Italy (4): mini game; USA (1)/Canada (2): ToG frame; Netherlands (0): DG Neutral; Turkey (3): ??
## session
## kept
## sent

colpgg = c("Subject",	"Group",	"Treatment",	"R1")

# 1. Choice dataframe ----
pgg1_dta_coop <- pgg %>% subset.data.frame(select = colpgg, Treatment == 1) %>%
  mutate(endowment = 20, cooperation = R1/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Ves049", TreatmentCode = "3a")

# 2. Beliefs dataframe ----
## KW: send from 20 to 5, step 5
## KW scale: 1: VI; 2: I; 3: A; 4: VA -> recoding done.

label_col = as.character(seq(20,5,-5))
norms_columns <- c(1:3, 32:35)
n_sub_N = pgg %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Treatment == 1) %>% summarise(n_sub_N = n())

## compute norm 
pgg1_appropriateness_sum <- pgg %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Treatment == 1) %>%
  summarise_at(vars(KW20_re:KW5_re), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- pgg1_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2017Ves049", TreatmentCode = "3a")

positive_appropriateness <- pgg1_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(pgg1_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- pgg1_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- pgg1_appropriateness_sum %>% mutate(delta_max = 0)
  
}


## compute variance norm
pgg1_norms_var <- pgg[, norms_columns] %>%
  subset.data.frame(subset = Treatment == 1) %>%
  summarise_at(vars(KW20_re:KW5_re), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

pgg1_final_norms <- merge.data.frame(pgg1_appropriateness_sum, pgg1_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2017Ves049", 
         TreatmentCode = "3a", 
         Avg_NE = as.integer(donation)/20,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(pgg1_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(pgg1_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(pgg1_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m))

# PGG TREATMENT 2-----------------
# get information on treatment

# cleaning DG
## exp_id
## where: Italy (4): mini game; USA (1)/Canada (2): ToG frame; Netherlands (0): DG Neutral; Turkey (3): ??
## session
## kept
## sent

colpgg = c("Subject",	"Group",	"Treatment",	"R1")

# 1. Choice dataframe ----
pgg2_dta_coop <- pgg %>% subset.data.frame(select = colpgg, Treatment == 2) %>%
  mutate(endowment = 20, cooperation = R1/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Ves049", TreatmentCode = "2a")

# 2. Beliefs dataframe ----
## KW: send from 20 to 5, step 5
## KW scale: 1: VI; 2: I; 3: A; 4: VA -> recoding done.

label_col = as.character(seq(20,5,-5))
norms_columns <- c(1:3, 32:35)
n_sub_N = pgg %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Treatment == 2) %>% summarise(n_sub_N = n())

## compute norm 
pgg2_appropriateness_sum <- pgg %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Treatment == 2) %>%
  summarise_at(vars(KW20_re:KW5_re), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- pgg2_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2017Ves049", TreatmentCode = "2a") %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- pgg2_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(pgg2_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- pgg2_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- pgg2_appropriateness_sum %>% mutate(delta_max = 0)
  
}


## compute variance norm
pgg2_norms_var <- pgg[, norms_columns] %>%
  subset.data.frame(subset = Treatment == 2) %>%
  summarise_at(vars(KW20_re:KW5_re), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

pgg2_final_norms <- merge.data.frame(pgg2_appropriateness_sum, pgg2_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2017Ves049", 
         TreatmentCode = "2a", 
         Avg_NE = as.integer(donation)/20,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(pgg2_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(pgg2_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(pgg2_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)

# PGG TREATMENT 3-----------------
# get information on treatment

# cleaning DG
## exp_id
## where: Italy (4): mini game; USA (1)/Canada (2): ToG frame; Netherlands (0): DG Neutral; Turkey (3): ??
## session
## kept
## sent

colpgg = c("Subject",	"Group",	"Treatment",	"R1")

# 1. Choice dataframe ----
pgg3_dta_coop <- pgg %>% subset.data.frame(select = colpgg, Treatment == 1) %>%
  mutate(endowment = 20, cooperation = R1/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Ves049", TreatmentCode = "1a")

# 2. Beliefs dataframe ----
## KW: send from 20 to 5, step 5
## KW scale: 1: VI; 2: I; 3: A; 4: VA -> recoding done.

label_col = as.character(seq(20,5,-5))
norms_columns <- c(1:3, 32:35)
n_sub_N = pgg %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Treatment == 3) %>% summarise(n_sub_N = n())

## compute norm 
pgg3_appropriateness_sum <- pgg %>% subset.data.frame(select = norms_columns) %>%
  subset.data.frame(subset = Treatment == 3) %>%
  summarise_at(vars(KW20_re:KW5_re), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- pgg3_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2017Ves049", TreatmentCode = "1a") %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- pgg3_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(pgg3_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- pgg3_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- pgg3_appropriateness_sum %>% mutate(delta_max = 0)
  
}


## compute variance norm
pgg3_norms_var <- pgg[, norms_columns] %>%
  subset.data.frame(subset = Treatment == 3) %>%
  summarise_at(vars(KW20_re:KW5_re), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

pgg3_final_norms <- merge.data.frame(pgg3_appropriateness_sum, pgg3_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2017Ves049", 
         TreatmentCode = "1a", 
         Avg_NE = as.integer(donation)/20,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(pgg3_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(pgg3_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(pgg3_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)


write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

write.csv(db_appropriateness, file = paste(csv_path_output, paste(db_appropriateness$PaperID[1], "_avg_kw.csv", sep = ""), sep = ""), row.names = F)

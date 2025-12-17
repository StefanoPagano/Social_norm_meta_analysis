# set wd 
setwd(paste(root, "Dati paper/2019Tjo060", sep=""))

csv_path_output <- "~/GitHub/Social_norm_meta_analysis/Paper_csv/"

# norm files - experiment 8 and 9
norms_8=read_excel("JBEE_Experiment_8_Norm.xlsx", sheet = "Sheet1")
norms_9=read_excel("JBEE_Experiment_9_Norm.xlsx", sheet = "Sheet1")

# meta-information dataset
meta_dataset <- read_xlsx(path = paste(root, "Social Norms meta.xlsx", sep=""), sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2019Tjo060", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))

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
n_sub_N = norms_8 %>% subset.data.frame(select = norms_columns) %>% summarise(n_sub_N = n())

## compute norm 
ex8_dg_appropriateness_sum <- norms_8 %>% subset.data.frame(select = norms_columns) %>%
  summarise_at(vars(S4H1:S4H5), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- ex8_dg_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2019Tjo060", TreatmentCode = 8)

positive_appropriateness <- ex8_dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(ex8_dg_appropriateness_sum$Kw_m) < 0){
  
negative_appropriateness <- ex8_dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
  mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)

} else {
  
  negative_appropriateness <- ex8_dg_appropriateness_sum %>% mutate(delta_max = 0)
  
}

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
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(ex8_dg_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))


# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(ex8_dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(ex8_dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m))

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
n_sub_N = norms_9 %>% subset.data.frame(select = norms_columns) %>% summarise(n_sub_N = n())

## compute norm 
ex9_dg_appropriateness_sum <- norms_9 %>% subset.data.frame(select = norms_columns) %>%
  summarise_at(vars(Handling1:Handling5), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- ex9_dg_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2019Tjo060", TreatmentCode = 9) %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- ex9_dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(ex9_dg_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- ex9_dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- ex9_dg_appropriateness_sum %>% mutate(delta_max = 0)
  
}

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
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(ex9_dg_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(ex9_dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(ex9_dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

write.csv(db_appropriateness, file = paste(csv_path_output, paste(db_appropriateness$PaperID[1], "_avg_kw.csv", sep = ""), sep = ""), row.names = F)

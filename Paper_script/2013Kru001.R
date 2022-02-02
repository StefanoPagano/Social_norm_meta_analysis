# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2013Kru001")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
dg=read_excel("behavior_bully_standard_data_2009_05_18.xlsx") 

dg <- dg %>%
  mutate(id= row_number())

# norms file
norms=read_excel("merged_2012.xlsx")

norms_kw_std <- norms %>% filter(standard==1 & amount %in% c(0:10) & !is.na(rating))
length(unique(norms_kw_std$subjectid))

norms_kw_bly <- norms %>% filter(bully==1 & amount %in% c(0:10) & !is.na(rating))
length(unique(norms_kw_bly$subjectid))

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2013Kru001", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))


# Standard-----------------
## 1. Choice dataframe ----
kw_std_coop <- dg %>% 
  select(id, bully, share) %>%
  filter(bully == 0) %>%
  mutate(endowment = 10, cooperation = share/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2013Kru001", TreatmentCode = "1a")

## 2. Beliefs dataframe ----
n_sub_N = norms_kw_std %>% 
  select(subjectid, standard) %>%
  summarise(n=length(unique(subjectid)))

### compute norm 
kw_std_appropriateness_sum <- norms_kw_std %>%
  group_by(amount) %>%
  summarise(sum = as.numeric(sum(rating)),
            n = as.numeric(n_sub_N), 
            Kw_m=as.numeric(sum/n_sub_N)) %>%
  mutate(donation = 10 - amount) 

colnames(kw_std_appropriateness_sum) <- c("kept", "appropriateness", "n_sub", "Kw_m", "donation")

db_appropriateness <- kw_std_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2013Kru001", TreatmentCode = "1a")

positive_appropriateness <- kw_std_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(kw_std_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- kw_std_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- kw_std_appropriateness_sum %>% mutate(delta_max = 0)
  
}

### compute variance norm
kw_std_norms_var <- norms_kw_std %>%
  group_by(amount) %>%
  summarise(var = var(rating)) %>%
  mutate(donation = 10 - amount)

colnames(kw_std_norms_var) <- c("kept", "var","donation")

kw_bly_final_norms <- merge.data.frame(kw_std_appropriateness_sum, kw_std_norms_var, by = "donation") %>% 
  subset.data.frame(subset = appropriateness == max(appropriateness)) %>% 
  mutate(PaperID = "2013Kru001", 
         TreatmentCode = "1a", 
         Avg_NE = as.integer(donation)/10,
         Var_NE = var, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(as.numeric(kw_std_appropriateness_sum$Kw_m)),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(appropriateness, var, donation, kept.x, kept.y))

## 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(kw_std_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(kw_bly_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub, Kw_m))


# Bully-----------------
## 1. Choice dataframe ----
kw_bly_coop <- dg %>% 
  select(id, bully, share) %>%
  filter(bully == 1) %>%
  mutate(endowment = 5, cooperation = (share-5)/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2013Kru001", TreatmentCode = "1b")

## 2. Beliefs dataframe ----
n_sub_N = norms_kw_bly %>% 
  select(subjectid, standard) %>%
  summarise(n=length(unique(subjectid)))

### compute norm 
kw_bly_appropriateness_sum <- norms_kw_bly %>%
  group_by(amount) %>%
  summarise(sum = as.numeric(sum(rating)),
            n = as.numeric(n_sub_N), 
            Kw_m=as.numeric(sum/n_sub_N))%>%
  mutate(donation = 5 - amount) 

colnames(kw_bly_appropriateness_sum) <- c("kept", "appropriateness", "n_sub", "Kw_m", "donation")

db_appropriateness <- kw_bly_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2013Kru001", TreatmentCode = "1b") %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- kw_bly_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(kw_bly_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- kw_bly_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- kw_bly_appropriateness_sum %>% mutate(delta_max = 0)
  
}

### compute variance norm
kw_bly_norms_var <- norms_kw_bly %>%
  group_by(amount) %>%
  summarise(var = var(rating)) %>%
  mutate(donation = 5 - amount)

colnames(kw_bly_norms_var) <- c("kept", "var","donation")

kw_bly_final_norms <- merge.data.frame(kw_bly_appropriateness_sum, kw_bly_norms_var, by = "donation") %>% 
  subset.data.frame(subset = appropriateness == max(appropriateness)) %>% 
  mutate(PaperID = "2013Kru001", 
         TreatmentCode = "1b", 
         Avg_NE = as.integer(donation)/10,
         Var_NE = var, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(as.numeric(kw_bly_appropriateness_sum$Kw_m)),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(appropriateness, var, donation, kept.x,kept.y))

## 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(kw_bly_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(kw_bly_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

write.csv(db_appropriateness, file = paste(csv_path_output, paste(db_appropriateness$PaperID[1], "_avg_kw.csv", sep = ""), sep = ""), row.names = F)

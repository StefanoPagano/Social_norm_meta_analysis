# set wd - norms data linked with krupka and weber 2013
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2013Kru001")

csv_path_output <- "~/Documents/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
dg=read_excel("list_data.xlsx") 
sum(dg$n)

dg <- as.data.frame(lapply(dg, rep, dg$n)) %>%
  mutate(id = row_number())
length(unique(dg$id))

# norms file
norms=read_excel("merged_2012.xlsx")

## amount = keep!!!
norms_list_give <- norms %>% filter(ListGive==T & 
                                      amount >= 0.0 & 
                                      amount <= 6.0 & 
                                      !is.na(rating))
length(unique(norms_list_give$subjectid))

norms_list_take1 <- norms %>% filter(ListTake==T & 
                                      amount >= 0.0 & 
                                      amount <= 6.0 & 
                                      amount != 5.5 &
                                      !is.na(rating))
length(unique(norms_list_take1$subjectid))

# meta-information dataset
meta_dataset <- read_xlsx(path = "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2007Lis165", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))


# Give version -----------------
## 1. Choice dataframe ----
list_give_coop <- dg %>% 
  select(id, take, Amount) %>%
  filter(take == 0) %>%
  mutate(endowment = 5, 
         cooperation = Amount/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2007Lis165", TreatmentCode = "1a")

## 2. Beliefs dataframe ----
n_sub_N = norms_list_give %>% 
  select(subjectid, ListGive) %>%
  summarise(n=length(unique(subjectid)))

### compute norm 
list_give_appropriateness_sum <- norms_list_give %>%
  group_by(amount) %>%
  summarise(sum = as.numeric(sum(rating)),
            n = as.numeric(n_sub_N), 
            Kw_m=as.numeric(sum/n_sub_N)) %>%
  mutate(donation = 5 - amount) 

colnames(list_give_appropriateness_sum) <- c("kept", "appropriateness", "n_sub", "Kw_m", "donation")

db_appropriateness <- list_give_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2007Lis165", TreatmentCode = "1a")

positive_appropriateness <- list_give_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(list_give_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- list_give_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- list_give_appropriateness_sum %>% mutate(delta_max = 0)
  
}

### compute variance norm
list_give_norms_var <- norms_list_give %>%
  group_by(amount) %>%
  summarise(var = var(rating)) %>%
  mutate(donation = 5 - amount)

colnames(list_give_norms_var) <- c("kept", "var", "donation")

list_give_final_norms <- merge.data.frame(list_give_appropriateness_sum, list_give_norms_var, by = "donation") %>% 
  subset.data.frame(subset = appropriateness == max(appropriateness)) %>% 
  mutate(PaperID = "2007Lis165", 
         TreatmentCode = "1a", 
         Avg_NE = as.numeric(donation)/5,
         Var_NE = var, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(as.numeric(list_give_appropriateness_sum$Kw_m)),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(appropriateness, var, donation, kept.x, kept.y))

## 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(list_give_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(list_give_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub, Kw_m))

# Take 1$ version -----------------
## 1. Choice dataframe ----
list_take1_coop <- dg %>% 
  select(id, take, Amount) %>%
  filter(take == 1) %>%
  mutate(endowment = 5, 
         cooperation = Amount/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2007Lis165", TreatmentCode = "1b")

## 2. Beliefs dataframe ----
n_sub_N = norms_list_take1 %>% 
  select(subjectid, ListTake) %>%
  summarise(n=length(unique(subjectid)))

### compute norm 
list_take1_appropriateness_sum <- norms_list_take1 %>%
  group_by(amount) %>%
  summarise(sum = as.numeric(sum(rating)),
            n = as.numeric(n_sub_N), 
            Kw_m=as.numeric(sum/n_sub_N)) %>%
  mutate(donation = 5 - amount) 

colnames(list_take1_appropriateness_sum) <- c("kept", "appropriateness", "n_sub", "Kw_m", "donation")

db_appropriateness <- list_take1_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2007Lis165", TreatmentCode = "1b") %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- list_take1_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(list_take1_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- list_take1_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- list_take1_appropriateness_sum %>% mutate(delta_max = 0)
  
}

### compute variance norm
list_take1_norms_var <- norms_list_take1 %>%
  group_by(amount) %>%
  summarise(var = var(rating)) %>%
  mutate(donation = 5 - amount)

colnames(list_take1_norms_var) <- c("kept", "var", "donation")

list_take1_final_norms <- merge.data.frame(list_take1_appropriateness_sum, list_take1_norms_var, by = "donation") %>% 
  subset.data.frame(subset = appropriateness == max(appropriateness)) %>% 
  mutate(PaperID = "2007Lis165", 
         TreatmentCode = "1b", 
         Avg_NE = as.numeric(donation)/5,
         Var_NE = var, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(as.numeric(list_take1_appropriateness_sum$Kw_m)),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(appropriateness, var, donation, kept.x, kept.y))

## 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(list_take1_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(list_take1_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

write.csv(db_appropriateness, file = paste(csv_path_output, paste(db_appropriateness$PaperID[1], "_avg_kw.csv", sep = ""), sep = ""), row.names = F)

# set wd - data linked with krupka and weber 2013
setwd("G:/Mon Drive/Meta-analysis beliefs/Dati paper/2013Kru001")

csv_path_output <- "~/GitHub/Social_norm_meta_analysis/Paper_csv/"

# choice file
dg=read_excel("Lazear_combined_kru.xlsx") %>%
  filter(choice == 1,
         option >= 0,
         sortingdata == 1) %>%
  select(uniquechoiceid, option)


# norms file
norms=read_excel("merged_2012.xlsx") %>% filter(amount >= 0 &
                                                  amount <=10 &
                                                  pass == 1 &
                                                  passplay == 1 &
                                                  !is.na(rating))

length(unique(norms$subjectid))


# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/Mon Drive/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2012Laz164", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))


# Exp 1 no sorting -----------------
## 1. Choice dataframe ----
dg_coop <- dg %>%
  mutate(endowment = 10, 
         cooperation = option/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2012Laz164", TreatmentCode = "3")

## 2. Beliefs dataframe ----
n_sub_N = norms %>% 
  select(subjectid) %>%
  summarise(n=length(unique(subjectid)))

### compute norm 
dg_appropriateness_sum <- norms %>%
  group_by(amount) %>%
  summarise(sum = as.numeric(sum(rating)),
            n = as.numeric(n_sub_N), 
            Kw_m=as.numeric(sum/n_sub_N)) %>%
  mutate(donation = 10 - amount) 

colnames(dg_appropriateness_sum) <- c("kept", "appropriateness", "n_sub", "Kw_m", "donation")

db_appropriateness <- dg_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2012Laz164", TreatmentCode = "3")

positive_appropriateness <- dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dg_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dg_appropriateness_sum %>% mutate(delta_max = 0)
  
}

### compute variance norm
dg_norms_var <- norms %>%
  group_by(amount) %>%
  summarise(var = var(rating)) %>%
  mutate(donation = 10 - amount)

colnames(dg_norms_var) <- c("kept", "var", "donation")

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = appropriateness == max(appropriateness)) %>% 
  mutate(PaperID = "2012Laz164", 
         TreatmentCode = "3", 
         Avg_NE = as.numeric(donation)/10,
         Var_NE = var, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(as.numeric(dg_appropriateness_sum$Kw_m)),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(appropriateness, var, donation, kept.x, kept.y))

## 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(dg_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub, Kw_m)) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

write.csv(db_appropriateness, file = paste(csv_path_output, paste(db_appropriateness$PaperID[1], "_avg_kw.csv", sep = ""), sep = ""), row.names = F)

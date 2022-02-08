# import data
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2022Tve168")

tve_df <- read.csv("non_linear_cpr_game.csv", sep = ",") %>%
  select(participant.code, subsession.round_number, player.contribution, session.label, player.empirical_expectations0:player.empirical_expectations4, player.personal_normative_beliefs, player.normative_expectations0:player.normative_expectations4) %>%
  filter(subsession.round_number==1)

tve_cpr_baseline <- tve_df %>%
  filter(session.label=="BASELINE")

tve_cpr_messaging <- tve_df %>%
  filter(session.label=="TREATMENT")

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2022Tve168", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))


# Baseline-----------------
## 1. Choice dataframe ----
tve_baseline_coop <- tve_cpr_baseline %>%
  mutate(endowment = 30, cooperation = (30-player.contribution)/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2022Tve168", TreatmentCode = "1a")

## 2. Beliefs dataframe ----
n_sub_N = tve_cpr_baseline %>% 
  select(participant.code) %>%
  summarise(n=length(unique(participant.code)))

### compute norm 
tve_baseline_appropriateness_sum <- tve_cpr_baseline %>%
  reshape(direction = "long", varying = 11:15, v.names = "NE") %>%
  group_by(participant.code) %>%
  summarise()

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
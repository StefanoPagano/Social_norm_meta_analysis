# set wd 
setwd("G:/Mon Drive/Meta-analysis beliefs/Dati paper/2019Dro082")

csv_path_output <- "~/GitHub/Social_norm_meta_analysis/Paper_csv/"

dg=read.csv("Drouvelis-Isen-Marx donations experiment.csv", sep=";")

## the next file contains all data except the conditional DG elicitations, which we did later
norms =read.csv("Drouvelis-Isen-Marx norms experiment.csv", sep=",")


# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/Mon Drive/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2019Dro082", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) 
# %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG BASE ---------------
# get information on treatment

# cleaning DG

## donation : amount donated from earnings
## bonusDonation : amount donated from windfall income
## subjectId : subject in the session
## earnings : results of real-effort task - or bonus income (1-3�)
## bonus3 : Indicator for windfall income of 3 GBP (versus control group with windfall income of 1 GBP)

coldg = c("subjectId","earnings","donation","bonusDonation","bonus3")

# 1. Choice dataframe ----
dg_dta_coop <- dg %>% subset.data.frame(select = coldg) %>%
  mutate(cooperation = donation/earnings) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2019Dro082", TreatmentCode = "1a")

# 2. Beliefs dataframe ----
## Subject
## Treatment
## earn_TOT
## KW scale: -1 = VI, -0.6 = I, -0.2 = SI, 0.2 = SA, 0.6 = A, 1 = VA.

label_col = as.character(c(0,0.1,0.25,0.50,1.00,1.50,2.00))
col_treat = c(1:7,21:24)
n_sub_N = norms %>%
  subset.data.frame(select = col_treat) %>% summarise(n_sub_N = n())

## compute norm 
dg_appropriateness_sum <- norms %>%
  subset.data.frame(select = col_treat) %>%
  summarise_at(vars(norm1_0:norm1_200), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- dg_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2019Dro082", TreatmentCode = "1a")

positive_appropriateness <- dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dg_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dg_appropriateness_sum %>% mutate(delta_max = 0)
  
}


## compute variance norm
dg_norms_var <- norms %>%
  summarise_at(vars(norm1_0:norm1_200), var, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2019Dro082", 
         TreatmentCode = "1a", 
         Avg_NE = as.integer(donation)/13,
         Var_NE = ..y, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(dg_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

write.csv(db_appropriateness, file = paste(csv_path_output, paste(db_appropriateness$PaperID[1], "_avg_kw.csv", sep = ""), sep = ""), row.names = F)

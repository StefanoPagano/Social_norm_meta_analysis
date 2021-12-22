# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2019Cha026")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

dg=read_excel("data.xls", sheet = "Sheet1", 
              col_types = c("numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric"))

## the next file contains all data except the conditional PG elicitations, which we did later
norms <- dg %>% 
  subset.data.frame(select = c(subject, elicit_norms, frame_tax, endowment, action, order,  norm), 
                    subset = elicit_norms == 1) %>%
  subset.data.frame(subset = endowment == 10) %>%
  subset.data.frame(subset = order == 1)
  

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2019Cha026", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG neutral-frame ---------------
# get information on treatment

# cleaning DG
## all round with 10 token initial endowment for dictator

## subject : subject in the session
## elicit_norms : experiment subject was in. 0 = Choice experiment, 1 = Norm elicitation experiment
## frame_tax: treatment subject was in. 0 = Neutrally-framed, 1 = Tax-framed
## endowment : we take only rounds where the dictator had 10 tokens
## keep: for subjects in the choice experiment, the number of tokens they kept for themselves.
coldg = c("subject","elicit_norms", "order", "frame_tax","endowment", "keep")

# 1. Choice dataframe ----
dgn_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = frame_tax == 0) %>%
  subset.data.frame(subset = elicit_norms == 0) %>%
  subset.data.frame(subset = endowment == 10) %>%
  subset.data.frame(subset = order == 1) %>%  
  mutate(cooperation = (endowment - keep)/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2019Cha026", TreatmentCode = 1)

# 2. Beliefs dataframe ----
## subject
## elicit_norms
## frame_tax
## endowment
## action : for subjects in the norm elicitation experiment the keep action being rated.
## norm: for subjects in the norm elicitation experiment, the norm rating. 
## KW scale: -1 = VI, -0.6 = I, -0.2 = SI, 0.2 = SA, 0.6 = A, 1 = VA.

label_col = as.character(seq(0,10,1))
n_sub_N = norms %>%
  subset.data.frame(subset = frame_tax == 0 & order == 1 & endowment == 10 & elicit_norms == 1) %>% summarise(n_sub_N = n()/11)

## compute norm 
dgn_appropriateness_sum <- norms %>%
  subset.data.frame(subset = frame_tax == 0) %>%
  group_by(action) %>%
  summarise(coop = sum(norm))%>%
  mutate(n_sub_N, Kw_m = coop/n_sub_N)
  # cbind.data.frame(donation=label_col)

positive_appropriateness <- dgn_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dgn_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dgn_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dgn_appropriateness_sum %>% mutate(delta_max = 0)
  
}


## compute variance norm
dgn_norms_var <- norms %>%
  subset.data.frame(subset = frame_tax == 0) %>%
  group_by(action) %>%
  summarise(var_norm = var(norm))
  # cbind.data.frame(donation=label_col)

dgn_final_norms <- merge.data.frame(dgn_appropriateness_sum, dgn_norms_var, by = "action") %>% 
  subset.data.frame(subset = coop == max(coop)) %>% 
  mutate(PaperID = "2019Cha026", 
         TreatmentCode = 1, 
         Avg_NE = action/10,
         Var_NE = var_norm, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(dgn_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(coop, var_norm, action))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dgn_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dgn_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m))


# DG tax-frame ---------------
# get information on treatment

# cleaning DG
## all round with 10 token initial endowment for dictator

## subject : subject in the session
## elicit_norms : experiment subject was in. 0 = Choice experiment, 1 = Norm elicitation experiment
## frame_tax: treatment subject was in. 0 = Neutrally-framed, 1 = Tax-framed
## endowment : we take only rounds where the dictator had 10 tokens
## keep: for subjects in the choice experiment, the number of tokens they kept for themselves.

# 1. Choice dataframe ----
dgt_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = frame_tax == 1) %>%
  subset.data.frame(subset = elicit_norms == 0) %>%
  subset.data.frame(subset = order == 1) %>%  
  subset.data.frame(subset = endowment == 10) %>%
  mutate(cooperation = (endowment - keep)/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2019Cha026", TreatmentCode = 2)

# 2. Beliefs dataframe ----
## subject
## elicit_norms
## frame_tax
## endowment
## action : for subjects in the norm elicitation experiment the keep action being rated.
## norm: for subjects in the norm elicitation experiment, the norm rating. 
## KW scale: -1 = VI, -0.6 = I, -0.2 = SI, 0.2 = SA, 0.6 = A, 1 = VA.

label_col = as.character(seq(0,10,1))
n_sub_N = norms %>%
  subset.data.frame(subset = frame_tax == 1 & order == 1 & endowment == 10 & elicit_norms == 1) %>% summarise(n_sub_N = n()/11)

## compute norm 
dgt_appropriateness_sum <- norms %>%
  subset.data.frame(subset = frame_tax == 1) %>%
  group_by(action) %>%
  summarise(coop = sum(norm)) %>%
  mutate(n_sub_N, Kw_m = coop/n_sub_N)
# cbind.data.frame(donation=label_col)

positive_appropriateness <- dgt_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dgt_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dgt_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dgt_appropriateness_sum %>% mutate(delta_max = 0)
  
}


## compute variance norm
dgt_norms_var <- norms %>%
  subset.data.frame(subset = frame_tax == 1) %>%
  group_by(action) %>%
  summarise(var_norm = var(norm))
# cbind.data.frame(donation=label_col)

dgt_final_norms <- merge.data.frame(dgt_appropriateness_sum, dgt_norms_var, by = "action") %>% 
  subset.data.frame(subset = coop == max(coop)) %>% 
  mutate(PaperID = "2019Cha026", 
         TreatmentCode = 2, 
         Avg_NE = action/10,
         Var_NE = var_norm, Avg_KW_m = Kw_m,
         Sd_Avg_NE = sd(dgt_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
         specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
         max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(coop, var_norm, action))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dgt_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dgt_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)


write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

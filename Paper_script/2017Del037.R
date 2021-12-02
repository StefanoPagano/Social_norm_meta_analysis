# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2017Del037")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"

dg=read.csv("DATA_full.csv", sep=",")

## the next file contains all data except the conditional DG elicitations, which we did later
norms <- dg %>% 
  subset.data.frame(select = c(ID, Subject, Treatment, Dictator, DICT_oth,
                              KWansw.1.,
                              KWansw.2.,
                              KWansw.3.,
                              KWansw.4.,
                              KWansw.5.,
                              KWansw.6.,
                              KWansw.7.)) %>%
  mutate(KW1 = recode(KWansw.1., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW2 = recode(KWansw.2., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW3 = recode(KWansw.3., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW4 = recode(KWansw.4., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW5 = recode(KWansw.5., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW6 = recode(KWansw.6., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1),
         KW7 = recode(KWansw.7., `2` = -1, `4` = -1/3, `6` = 1/3, `8` = 1)) 
  #subset.data.frame(subset = endowment == 10)

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2017Del037", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG BASE ---------------
# get information on treatment

# cleaning DG
## dictators have different initial endowment related to real effort task

## Subject : subject in the session
## Treatment : name of treatment -> IN, OUT, BASE
## earn_TOT : we take only rounds where the dictator had 10 tokens
## DICT_oth: the number of tokens they given to other.
coldg = c("Subject","Treatment","earn_TOT","DICT_oth")

# 1. Choice dataframe ----
dg_base_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "BASE") %>%
  mutate(cooperation = DICT_oth/earn_TOT) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Del037", TreatmentCode = 1)

# 2. Beliefs dataframe ----
## Subject
## Treatment
## earn_TOT
## KW scale: -1 = VI, -1/3 = I, 1/3 = A, 1 = VA.

label_col = as.character(seq(0,6,1))
n_sub_N = norms %>%
  subset.data.frame(subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "BASE") %>% 
  summarise(n_sub_N = n())


## compute norm 
dg_base_appropriateness_sum <- norms %>%
  subset.data.frame(subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "BASE") %>%
  summarise_at(vars(KW1:KW7), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- dg_base_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
dg_base_norms_var <- norms %>%
  subset.data.frame(subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "BASE") %>%
  summarise_at(vars(KW1:KW7), var, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_base_final_norms <- merge.data.frame(dg_base_appropriateness_sum, dg_base_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2017Del037", 
         TreatmentCode = 1, 
         Avg_NE = as.integer(donation)/6,
         Var_NE = ..y,
         Sd_Avg_NE = sd(dg_base_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         Sum_delta_max = sum(positive_appropriateness$delta_max)) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_base_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg_base_final_norms[1,], all.x=T, by = c("PaperID","TreatmentCode")) %>% 
  subset.data.frame(select = -c(n_sub_N, Kw_m))


# DG IN treatment ---------------
# get information on treatment

# cleaning DG
## dictators have different initial endowment related to real effort task

## Subject : subject in the session
## Treatment : name of treatment -> IN, OUT, BASE
## earn_TOT : we take only rounds where the dictator had 10 tokens
## DICT_oth: the number of tokens they given to other.
coldg = c("Subject","Treatment","earn_TOT","DICT_oth")

# 1. Choice dataframe ----
dg_in_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "IN") %>%
  mutate(cooperation = DICT_oth/earn_TOT) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Del037", TreatmentCode = 2)

# 2. Beliefs dataframe ----
## Subject
## Treatment
## earn_TOT
## KW scale: -1 = VI, -1/3 = I, 1/3 = A, 1 = VA.

label_col = as.character(seq(0,6,1))
n_sub_N = norms %>%
  subset.data.frame(subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "IN") %>% 
  summarise(n_sub_N = n())

## compute norm 
dg_in_appropriateness_sum <- norms %>%
  subset.data.frame(subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "IN") %>%
  summarise_at(vars(KW1:KW7), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- dg_in_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
dg_in_norms_var <- norms %>%
  subset.data.frame(subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "IN") %>%
  summarise_at(vars(KW1:KW7), var, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_in_final_norms <- merge.data.frame(dg_in_appropriateness_sum, dg_in_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2017Del037", 
         TreatmentCode = 2, 
         Avg_NE = as.integer(donation)/6,
         Var_NE = ..y,
         Sd_Avg_NE = sd(dg_in_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         Sum_delta_max = sum(positive_appropriateness$delta_max)) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_in_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg_in_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)


# DG OUT treatment ---------------
# get information on treatment

# cleaning DG
## dictators have different initial endowment related to real effort task

## Subject : subject in the session
## Treatment : name of treatment -> IN, OUT, BASE
## earn_TOT : we take only rounds where the dictator had 10 tokens
## DICT_oth: the number of tokens they given to other.
coldg = c("Subject","Treatment","earn_TOT","DICT_oth")

# 1. Choice dataframe ----
dg_out_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "OUT") %>%
  mutate(cooperation = DICT_oth/earn_TOT) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2017Del037", TreatmentCode = 3)

# 2. Beliefs dataframe ----
## Subject
## Treatment
## earn_TOT
## KW scale: -1 = VI, -1/3 = I, 1/3 = A, 1 = VA.

label_col = as.character(seq(0,6,1))
n_sub_N = norms %>%
  subset.data.frame(subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "OUT") %>% 
  summarise(n_sub_N = n())

## compute norm 
dg_out_appropriateness_sum <- norms %>%
  subset.data.frame(subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "OUT") %>%
  summarise_at(vars(KW1:KW7), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

positive_appropriateness <- dg_out_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

## compute variance norm
dg_out_norms_var <- norms %>%
  subset.data.frame(subset = Dictator == 1) %>%
  subset.data.frame(subset = Treatment == "IN") %>%
  summarise_at(vars(KW1:KW7), var, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_out_final_norms <- merge.data.frame(dg_out_appropriateness_sum, dg_out_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2017Del037", 
         TreatmentCode = 3, 
         Avg_NE = as.integer(donation)/6,
         Var_NE = ..y,
         Sd_Avg_NE = sd(dg_out_appropriateness_sum$Kw_m),
         Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
         Sum_delta_max = sum(positive_appropriateness$delta_max)) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_out_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg_out_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)


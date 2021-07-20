library(tidyverse)
library(readxl)
rm(list = ls())

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
  subset.data.frame(select = c(subject, elicit_norms, frame_tax, endowment, action, norm), 
                    subset = elicit_norms == 1) %>%
  subset.data.frame(subset = endowment == 10)

# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2019Cha026", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG neutral-frame ---------------
# get information on treatment

# cleaning DG
## all round with 10 token initial endowment for dictator

## subject : subject in the session
## elicit_norms : experiment subject was in. 0 = Choice experiment, 1 = Norm elicitation experiment
## frame_tax: treatment subject was in. 0 = Neutrally-framed, 1 = Tax-framed
## endowment : we take only rounds where the dictator had 10 tokens
## keep: for subjects in the choice experiment, the number of tokens they kept for themselves.
coldg = c("subject","elicit_norms","frame_tax","endowment", "keep")

# 1. Choice dataframe ----
dg_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = frame_tax == 0) %>%
  subset.data.frame(subset = elicit_norms == 0) %>%
  subset.data.frame(subset = endowment == 10) %>%
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

## compute norm 
dg_appropriateness_sum <- norms %>%
  subset.data.frame(subset = frame_tax == 0) %>%
  group_by(action) %>%
  summarise(coop = sum(norm))
  # cbind.data.frame(donation=label_col)

## compute variance norm
dg_norms_var <- norms %>%
  subset.data.frame(subset = frame_tax == 0) %>%
  group_by(action) %>%
  summarise(var_coop = var(norm))
  # cbind.data.frame(donation=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "action") %>% 
  subset.data.frame(subset = coop == max(coop)) %>% 
  mutate(PaperID = "2019Cha026", 
    TreatmentCode = 1, 
    avg_NE = action/10,
    var_NE = var_coop) %>%
  subset.data.frame(select = -c(coop, var_coop, action))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode"))


# DG tax-frame ---------------
# get information on treatment

# cleaning DG
## all round with 10 token initial endowment for dictator

## subject : subject in the session
## elicit_norms : experiment subject was in. 0 = Choice experiment, 1 = Norm elicitation experiment
## frame_tax: treatment subject was in. 0 = Neutrally-framed, 1 = Tax-framed
## endowment : we take only rounds where the dictator had 10 tokens
## keep: for subjects in the choice experiment, the number of tokens they kept for themselves.
coldg = c("subject","elicit_norms","frame_tax","endowment", "keep")

# 1. Choice dataframe ----
dg_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = frame_tax == 1) %>%
  subset.data.frame(subset = elicit_norms == 0) %>%
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

## compute norm 
dg_appropriateness_sum <- norms %>%
  subset.data.frame(subset = frame_tax == 1) %>%
  group_by(action) %>%
  summarise(coop = sum(norm))
# cbind.data.frame(donation=label_col)

## compute variance norm
dg_norms_var <- norms %>%
  subset.data.frame(subset = frame_tax == 1) %>%
  group_by(action) %>%
  summarise(var_coop = var(norm))
# cbind.data.frame(donation=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "action") %>% 
  subset.data.frame(subset = coop == max(coop)) %>% 
  mutate(PaperID = "2019Cha026", 
         TreatmentCode = 2, 
         avg_NE = action/10,
         var_NE = var_coop) %>%
  subset.data.frame(select = -c(coop, var_coop, action))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)


write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)
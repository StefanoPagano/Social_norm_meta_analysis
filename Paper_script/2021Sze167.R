# FIRST WAVE ----

# import data
setwd(paste(root, "Dati paper/2021Sze167", sep=""))

csv_path_output <- "~/GitHub/Social_norm_meta_analysis/Paper_csv/"

# meta-information dataset
meta_dataset <- read_xlsx(path = paste(root, "Social Norms meta.xlsx", sep=""), sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2021Sze167", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))

sze_df <- read.csv("full.csv", sep = ",") %>% 
  select(treatment, id, round, excluded, contribution, contribute_cond, pnb, ee_avg, ne_avg)

# data treatment high-low round 1 ----
sze_HighLow_round1 <- sze_df %>%
  filter(treatment == "High (0.9) to Low (0.6)" & 
           round==1 & 
           !(is.na(contribution)) & 
           excluded!="Exluded")

sze_1a_coop <- sze_HighLow_round1 %>%
  mutate(endowment = 100, cooperation = contribution/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2021Sze167", TreatmentCode = "1a")

## 2. Beliefs dataframe

### compute normative expectation 
NE_1a <- sze_HighLow_round1 %>%
  summarise(Avg_NE=mean(ne_avg, na.rm=T)/100,
            Var_NE=var(ne_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1a")

### compute empirical expectation
EE_1a <- sze_HighLow_round1 %>%
  summarise(Avg_EE=mean(ee_avg, na.rm=T)/100,
            Var_EE=var(ee_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1a")

### compute personal normative beliefs
PNB_1a <- sze_HighLow_round1 %>%
  summarise(Avg_PNB=mean(pnb, na.rm=T)/100,
            Var_PNB=var(pnb, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1a")

## merge norms
Sze_1a_final_norms <- inner_join(NE_1a, EE_1a, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_1a, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset
finaldf <- meta_dataset %>% merge.data.frame(sze_1a_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Sze_1a_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) 

# data treatment high-low round 1 ----
sze_HighLow_round15 <- sze_df %>%
  filter(treatment == "High (0.9) to Low (0.6)" & 
           round==15 & 
           !(is.na(contribution)) & 
           excluded!="Exluded")

sze_1b_coop <- sze_HighLow_round15 %>%
  mutate(endowment = 100, cooperation = contribution/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2021Sze167", TreatmentCode = "1b")

## 2. Beliefs dataframe

### compute normative expectation 
NE_1b <- sze_HighLow_round15 %>%
  summarise(Avg_NE=mean(ne_avg, na.rm=T)/100,
            Var_NE=var(ne_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1b")

### compute empirical expectation
EE_1b <- sze_HighLow_round15 %>%
  summarise(Avg_EE=mean(ee_avg, na.rm=T)/100,
            Var_EE=var(ee_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1b")

### compute personal normative beliefs
PNB_1b <- sze_HighLow_round15 %>%
  summarise(Avg_PNB=mean(pnb, na.rm=T)/100,
            Var_PNB=var(pnb, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1b")

## merge norms
Sze_1b_final_norms <- inner_join(NE_1b, EE_1b, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_1b, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset
finaldf <- meta_dataset %>% merge.data.frame(sze_1b_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Sze_1b_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf)

# data treatment low-high round 1 ----
sze_LowHigh_round1 <- sze_df %>%
  filter(treatment == "Low (0.6) to High (0.9)" & 
           round==1 & 
           !(is.na(contribution)) & 
           excluded!="Exluded")

sze_2a_coop <- sze_LowHigh_round1 %>%
  mutate(endowment = 100, cooperation = contribution/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2021Sze167", TreatmentCode = "2a")

## 2. Beliefs dataframe

### compute normative expectation 
NE_2a <- sze_LowHigh_round1 %>%
  summarise(Avg_NE=mean(ne_avg, na.rm=T)/100,
            Var_NE=var(ne_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2a")

### compute empirical expectation
EE_2a <- sze_LowHigh_round1 %>%
  summarise(Avg_EE=mean(ee_avg, na.rm=T)/100,
            Var_EE=var(ee_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2a")

### compute personal normative beliefs
PNB_2a <- sze_LowHigh_round1 %>%
  summarise(Avg_PNB=mean(pnb, na.rm=T)/100,
            Var_PNB=var(pnb, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2a")

## merge norms
Sze_2a_final_norms <- inner_join(NE_2a, EE_2a, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_2a, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset
finaldf <- meta_dataset %>% merge.data.frame(sze_2a_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Sze_2a_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf)

# data treatment low-high round 15 ----
sze_LowHigh_round15 <- sze_df %>%
  filter(treatment == "Low (0.6) to High (0.9)" & 
           round==15 & 
           !(is.na(contribution)) & 
           excluded!="Exluded")

sze_2b_coop <- sze_LowHigh_round15 %>%
  mutate(endowment = 100, cooperation = contribution/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2021Sze167", TreatmentCode = "2b")

## 2. Beliefs dataframe ----

### compute normative expectation 
NE_2b <- sze_LowHigh_round15 %>%
  summarise(Avg_NE=mean(ne_avg, na.rm=T)/100,
            Var_NE=var(ne_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2b")

### compute empirical expectation
EE_2b <- sze_LowHigh_round15 %>%
  summarise(Avg_EE=mean(ee_avg, na.rm=T)/100,
            Var_EE=var(ee_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2b")

### compute personal normative beliefs
PNB_2b <- sze_LowHigh_round15 %>%
  summarise(Avg_PNB=mean(pnb, na.rm=T)/100,
            Var_PNB=var(pnb, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2b")

## merge norms
Sze_2b_final_norms <- inner_join(NE_2b, EE_2b, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_2b, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(sze_2b_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Sze_2b_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf)

# REPLICATED EXP ----

sze_df_rep <- read.csv("full_replicated.csv", sep = ",") %>% 
  select(treatment, id, round, excluded, contribution, contribute_cond, pnb, ee_avg, ne_avg)

# data treatment high-low round 1 ----
sze_HighLow_round1_rep <- sze_df_rep %>%
  filter(treatment == "High (0.9) to Low (0.6)" & 
           round==1 & 
           !(is.na(contribution)) & 
           excluded!="Exluded")

sze_1a_rep_coop <- sze_HighLow_round1_rep %>%
  mutate(endowment = 100, cooperation = contribution/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2021Sze167", TreatmentCode = "1a_rep")

## 2. Beliefs dataframe

### compute normative expectation 
NE_1a_rep <- sze_HighLow_round1_rep %>%
  summarise(Avg_NE=mean(ne_avg, na.rm=T)/100,
            Var_NE=var(ne_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1a_rep")

### compute empirical expectation
EE_1a_rep <- sze_HighLow_round1_rep %>%
  summarise(Avg_EE=mean(ee_avg, na.rm=T)/100,
            Var_EE=var(ee_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1a_rep")

### compute personal normative beliefs
PNB_1a_rep <- sze_HighLow_round1_rep %>%
  summarise(Avg_PNB=mean(pnb, na.rm=T)/100,
            Var_PNB=var(pnb, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1a_rep")

## merge norms
Sze_1a_rep_final_norms <- inner_join(NE_1a_rep, EE_1a_rep, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_1a_rep, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset
finaldf <- meta_dataset %>% merge.data.frame(sze_1a_rep_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Sze_1a_rep_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf)

# data treatment high-low round 1 ----
sze_HighLow_round15_rep <- sze_df_rep %>%
  filter(treatment == "High (0.9) to Low (0.6)" & 
           round==15 & 
           !(is.na(contribution)) & 
           excluded!="Exluded")

sze_1b_rep_coop <- sze_HighLow_round15_rep %>%
  mutate(endowment = 100, cooperation = contribution/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2021Sze167", TreatmentCode = "1b_rep")

## 2. Beliefs dataframe

### compute normative expectation 
NE_1b_rep <- sze_HighLow_round15_rep %>%
  summarise(Avg_NE=mean(ne_avg, na.rm=T)/100,
            Var_NE=var(ne_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1b_rep")

### compute empirical expectation
EE_1b_rep <- sze_HighLow_round15_rep %>%
  summarise(Avg_EE=mean(ee_avg, na.rm=T)/100,
            Var_EE=var(ee_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1b_rep")

### compute personal normative beliefs
PNB_1b_rep <- sze_HighLow_round15_rep %>%
  summarise(Avg_PNB=mean(pnb, na.rm=T)/100,
            Var_PNB=var(pnb, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "1b_rep")

## merge norms
Sze_1b_rep_final_norms <- inner_join(NE_1b_rep, EE_1b_rep, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_1b_rep, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset
finaldf <- meta_dataset %>% merge.data.frame(sze_1b_rep_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Sze_1b_rep_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf)

# data treatment low-high round 1 ----
sze_LowHigh_round1_rep <- sze_df_rep %>%
  filter(treatment == "Low (0.6) to High (0.9)" & 
           round==1 & 
           !(is.na(contribution)) & 
           excluded!="Exluded")

sze_2a_rep_coop <- sze_LowHigh_round1_rep %>%
  mutate(endowment = 100, cooperation = contribution/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2021Sze167", TreatmentCode = "2a_rep")

## 2. Beliefs dataframe

### compute normative expectation 
NE_2a_rep <- sze_LowHigh_round1_rep %>%
  summarise(Avg_NE=mean(ne_avg, na.rm=T)/100,
            Var_NE=var(ne_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2a_rep")

### compute empirical expectation
EE_2a_rep <- sze_LowHigh_round1_rep %>%
  summarise(Avg_EE=mean(ee_avg, na.rm=T)/100,
            Var_EE=var(ee_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2a_rep")

### compute personal normative beliefs
PNB_2a_rep <- sze_LowHigh_round1_rep %>%
  summarise(Avg_PNB=mean(pnb, na.rm=T)/100,
            Var_PNB=var(pnb, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2a_rep")

## merge norms
Sze_2a_rep_final_norms <- inner_join(NE_2a_rep, EE_2a_rep, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_2a_rep, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset
finaldf <- meta_dataset %>% merge.data.frame(sze_2a_rep_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Sze_2a_rep_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf)

# data treatment low-high round 15 ----
sze_LowHigh_round15_rep <- sze_df_rep %>%
  filter(treatment == "Low (0.6) to High (0.9)" & 
           round==15 & 
           !(is.na(contribution)) & 
           excluded!="Exluded")

sze_2b_rep_coop <- sze_LowHigh_round15_rep %>%
  mutate(endowment = 100, cooperation = contribution/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2021Sze167", TreatmentCode = "2b_rep")

## 2. Beliefs dataframe ----

### compute normative expectation 
NE_2b_rep <- sze_LowHigh_round15_rep %>%
  summarise(Avg_NE=mean(ne_avg, na.rm=T)/100,
            Var_NE=var(ne_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2b_rep")

### compute empirical expectation
EE_2b_rep <- sze_LowHigh_round15_rep %>%
  summarise(Avg_EE=mean(ee_avg, na.rm=T)/100,
            Var_EE=var(ee_avg, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2b_rep")

### compute personal normative beliefs
PNB_2b_rep <- sze_LowHigh_round15_rep %>%
  summarise(Avg_PNB=mean(pnb, na.rm=T)/100,
            Var_PNB=var(pnb, na.rm=T)/100) %>%
  mutate(PaperID = "2021Sze167", 
         TreatmentCode = "2b_rep")

## merge norms
Sze_2b_rep_final_norms <- inner_join(NE_2b_rep, EE_2b_rep, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_2b_rep, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(sze_2b_rep_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Sze_2b_rep_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

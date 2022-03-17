# import data
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2022Tve168")
csv_path_output <- "~/Documents/GitHub/Social_norm_meta_analysis/Paper_csv/"

tve_df <- read.csv("non_linear_cpr_game.csv", sep = ",") %>%
  select(participant.code, subsession.round_number, player.contribution, session.label, player.empirical_expectations0:player.empirical_expectations4, player.personal_normative_beliefs, player.normative_expectations0:player.normative_expectations4) %>%
  filter(subsession.round_number==1)

tve_cpr_baseline <- tve_df %>%
  filter(session.label=="BASELINE")

tve_cpr_messaging <- tve_df %>%
  filter(session.label=="TREATMENT")

# meta-information dataset
meta_dataset <- read_xlsx(path = "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2022Tve168", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) #%>% mutate(TreatmentCode = as.numeric(TreatmentCode))


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

### compute normative expectation 
NE_baseline <- tve_cpr_baseline %>%
  reshape(direction = "long", varying = 11:15, v.names = "NE") %>%
  group_by(participant.code) %>%
  summarise(mean_sub_NE=mean(NE, na.rm=T)) %>%
  summarise(Avg_NE=mean(mean_sub_NE, na.rm=T)/30,
            Var_NE=var(mean_sub_NE, na.rm=T)/30) %>%
  mutate(PaperID = "2022Tve168", 
         TreatmentCode = "1a")

### compute empirical expectation
EE_baseline <- tve_cpr_baseline %>%
  reshape(direction = "long", varying = 5:9, v.names = "EE") %>%
  group_by(participant.code) %>%
  summarise(mean_sub_EE=mean(EE, na.rm=T)) %>%
  summarise(Avg_EE=mean(mean_sub_EE, na.rm=T)/30,
            Var_EE=var(mean_sub_EE, na.rm=T)/30) %>%
  mutate(PaperID = "2022Tve168", 
         TreatmentCode = "1a")

### compute personal normative beliefs
PNB_baseline <- tve_cpr_baseline %>%
  summarise(Avg_PNB=mean(player.personal_normative_beliefs, na.rm=T)/30,
            Var_PNB=var(player.personal_normative_beliefs, na.rm=T)/30) %>%
  mutate(PaperID = "2022Tve168", 
         TreatmentCode = "1a")

## merge norms
Tve_baseline_final_norms <- inner_join(NE_baseline, EE_baseline, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_baseline, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(tve_baseline_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Tve_baseline_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) 

# Treatment -----------------
## 1. Choice dataframe ----
tve_treatment_coop <- tve_cpr_messaging %>%
  mutate(endowment = 30, cooperation = (30-player.contribution)/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2022Tve168", TreatmentCode = "1b")

## 2. Beliefs dataframe ----
n_sub_N = tve_cpr_messaging %>% 
  select(participant.code) %>%
  summarise(n=length(unique(participant.code)))

### compute normative expectation 
NE_treatment <- tve_cpr_messaging %>%
  reshape(direction = "long", varying = 11:15, v.names = "NE") %>%
  group_by(participant.code) %>%
  summarise(mean_sub_NE=mean(NE, na.rm=T)) %>%
  summarise(Avg_NE=mean(mean_sub_NE, na.rm=T)/30,
            Var_NE=var(mean_sub_NE, na.rm=T)/30) %>%
  mutate(PaperID = "2022Tve168", 
         TreatmentCode = "1b")

### compute empirical expectation
EE_treatment <- tve_cpr_messaging %>%
  reshape(direction = "long", varying = 5:9, v.names = "EE") %>%
  group_by(participant.code) %>%
  summarise(mean_sub_EE=mean(EE, na.rm=T)) %>%
  summarise(Avg_EE=mean(mean_sub_EE, na.rm=T)/30,
            Var_EE=var(mean_sub_EE, na.rm=T)/30) %>%
  mutate(PaperID = "2022Tve168", 
         TreatmentCode = "1b")

### compute personal normative beliefs
PNB_treatment <- tve_cpr_messaging %>%
  summarise(Avg_PNB=mean(player.personal_normative_beliefs, na.rm=T)/30,
            Var_PNB=var(player.personal_normative_beliefs, na.rm=T)/30) %>%
  mutate(PaperID = "2022Tve168", 
         TreatmentCode = "1b")

## merge norms
Tve_treatment_final_norms <- inner_join(NE_treatment, EE_treatment, by=c("PaperID","TreatmentCode")) %>% 
  inner_join(PNB_treatment, by=c("PaperID","TreatmentCode")) %>%
  mutate(Avg_KW_m = NA,
         Sd_Avg_NE = NA,
         Sd_Avg_NE_min_max = NA,
         specificity_plus = NA,
         specificity_min = NA,
         max_sigma = NA)

## 3. combine dataset ----
finaldf <- meta_dataset %>% merge.data.frame(tve_treatment_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(Tve_treatment_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  rbind.data.frame(finaldf)

write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

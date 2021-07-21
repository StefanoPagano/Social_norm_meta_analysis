library(tidyverse)
library(readxl)
library(writexl)
rm(list = ls())


#### Insert excel file path ####

Social_Norms_meta <- read_excel("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Other/Report_202107/"
#### List of Games ####

ls_game <- c("DG", "DG Tax", "UG", "PGG", "TG", "BG", "GEG", "PDG", "Donation Game", "Investment game", "ToG", "Tax Game")

#### Treatments stats ####
df_treat <- Social_Norms_meta %>%
  subset.data.frame(subset = Target == "Y") %>%
  subset.data.frame(subset = Game_type %in% ls_game) %>%
  subset.data.frame(subset = 
                      Environment %in% c("Classroom", 
                                         "Lab", 
                                         "Online", 
                                         "Lab-in-the-field", 
                                         "virtual Lab")) %>%
group_by(Game_type) %>%
  summarise(
    Treatments = n(),
    Between_subjects_beliefs = sum(Separate_sample_beliefs == "Y", na.rm = T),
    KW = sum(Method_elicitation == "KW", na.rm = T),
    Bicchieri = sum(Method_elicitation == "Bicchieri", na.rm = T),
    KW_Bicchieri = sum(Method_elicitation == "Both", na.rm = T),
    Monetary_Punishment = sum(Punishment == "Monetary", na.rm = T),
    Choice_Method_Direct = sum(Choice_Method == "Direct", na.rm = T),
    Choice_Method_Strategy = sum(Choice_Method == "Strategy", na.rm = T),
    OnlyNorms = sum(Choice_Method == "OnlyNorms", na.rm = T),
    Choice_Method_Both = sum(Choice_Method == "Both", na.rm = T),
    Available_Data = sum(StatusTreatment_Roma == "6-Complete", na.rm = T)
  ) %>%
  arrange(-Treatments)

write.csv(df_treat, file = paste(csv_path_output, paste("Treatments.csv", sep = ""), sep = ""), row.names = F)

#### Dataset stats ####
df_data <- Social_Norms_meta %>%
  subset.data.frame(subset = TreatmentCode %in% c("1","1a","1aa")) %>%
  group_by(PaperID, StatusTreatment_Roma) %>%
  summarise(
    N_Paper = n(),
    ) %>%
  group_by(StatusTreatment_Roma) %>%
  summarise(N_Paper = n())

write.csv(df_treat, file = paste(csv_path_output, paste("Treatments.csv", sep = ""), sep = ""), row.names = F)

df_tot <- df_data %>% summarise(sum(N_Paper))

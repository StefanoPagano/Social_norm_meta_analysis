#### SOCIAL NORMS META-ANALYSIS INFORMATION ####

rm(list=ls())

meta_dataset <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL")

game_type_in_target = c("DG", "PGG", "PDG", "Donation Game", "UG", "TG", "ToG", "CPR", "GEG", "DG Tax", "BG", "Lying DG", "Third-Party Lying DG", "Tax Game", "Investment Game")

# 2.1 PAPER SEARCH

## number of treatments in target
meta_filtered <- meta_dataset %>% subset.data.frame(subset = Game_type %in% game_type_in_target & Monetary_Incentivized_experiment == "Y" & Method_elicitation != "N")

table_treatments_in_target_per_game <- meta_filtered %>% 
  group_by(Game_type) %>% 
  summarise(treatments_in_target_per_game = n (),
            between_subjects_beliefs = sum(Separate_sample_beliefs == "Y", na.rm = T),
            KW = sum(Method_elicitation == "KW", na.rm = T),
            Bicchieri = sum(Method_elicitation == "Bicchieri", na.rm = T),
            KW_and_Bicchieri = sum(Method_elicitation == "Both", na.rm = T),
            Only_Norms = sum(Choice_Method == "OnlyNorms", na.rm = T),
            Available_data = sum(StatusTreatment_Roma == "6-Complete", na.rm = T)) %>% 
  arrange(-treatments_in_target_per_game)

Sum_of_treatments_in_target = sum(table_treatments_in_target_per_game$treatments_in_target_per_game)

kable(table_treatments_in_target_per_game)

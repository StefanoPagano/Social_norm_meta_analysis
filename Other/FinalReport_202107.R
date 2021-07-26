library(tidyverse)
library(readxl)
library(ggplot2)
#library(writexl)
rm(list = ls())


# Insert excel file path

Social_Norms_meta <- read_excel("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL")

csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Other/Report_202107/"
# List of Games

ls_game <- c("DG", "DG Tax", "UG", "PGG", "TG", "BG", "GEG", "PDG", "Donation Game", "Investment game", "ToG", "Tax Game", "CPR", "Lying DG", "Third-Party Lying DG")


#### Treatments stats ####
df_treat <- Social_Norms_meta %>%
  subset.data.frame(subset = Target == "Y") %>%
  subset.data.frame(subset = Game_type %in% ls_game) %>%
  
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

#### Data collection status per treatment####
df_data <- Social_Norms_meta %>%
  group_by(StatusTreatment_Roma) %>%
  summarise(
    N_treatments = n(),
  ) %>%
  replace_na(list(StatusTreatment_Roma  = "Duplicates"))

write.csv(df_data, file = paste(csv_path_output, paste("Data Collection.csv", sep = ""), sep = ""), row.names = F)


#### Paper Search and Selection - (Slide 3) ####
T_paper_sel = Social_Norms_meta %>% subset.data.frame(subset = Target == "Y")
N_Paper = length(levels(as.factor(T_paper_sel$PaperID)))

Tot_Treatment <- df_treat %>% summarise(sum(Treatments))

paste("Total number of papers: ", N_Paper)
paste("Total number of treatments: ", Tot_Treatment)


#### Papers per year ####
df_year <- Social_Norms_meta %>% subset.data.frame(select = c(PaperID, Year)) %>%
  group_by(Year) %>%
  distinct(PaperID, .keep_all = T) %>%
  summarise(n = n())

plot_year <- ggplot(df_year, aes(Year, n)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  labs(x="Year", 
       y="Number of papers", 
       title = "Papers per Year",
       caption = "Data from Social Norms meta.xlsx") 

plot_year

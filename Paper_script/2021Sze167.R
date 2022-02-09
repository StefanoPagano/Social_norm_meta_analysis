# import data
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2021Sze167")
csv_path_output <- "C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/"


sze_df <- read.csv("full.csv", sep = ",") %>% 
  select(treatment, id, round, excluded, contribution, contribute_cond, pnb, ee_avg, ne_avg, disaster_probability)

# data treatment high-low round 1
sze_HighLow_round1 <- sze_df %>%
  filter(disaster_probability==0.9, round==1 & !(is.na(contribution)) & excluded!="Exluded")

# data treatment high-low round 1
sze_HighLow_round15 <- sze_df %>%
  filter(disaster_probability==0.6, round==15 & !(is.na(contribution)) & excluded!="Exluded")

# data treatment low-high round 1
sze_LowHigh_round1 <- sze_df %>%
  filter(disaster_probability==0.6, round==1 & !(is.na(contribution)) & excluded!="Exluded")

# data treatment low-high round 15
sze_LowHigh_round1 <- sze_df %>%
  filter(disaster_probability==0.9, round==15 & !(is.na(contribution)) & excluded!="Exluded")
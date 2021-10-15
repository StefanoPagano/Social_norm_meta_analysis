library(ggplot2)
library(tidyverse)

setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/")

# read data 
master <- read.csv("Paper_csv/Master.csv")
DG <- master %>% subset.data.frame(Game_type=="DG")

# H1 : correlation between average cooperation and norm (NE)
cor.test(master$Avg_coop, master$Avg_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Avg_NE, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm")

# H2a : correlation between average cooperation and norm (NE)
cor.test(master$Avg_coop, master$Var_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Var_NE, y=Avg_coop, color=Game_type)) + geom_point() + geom_smooth()

# H2b : correlation between variance cooperation and norm (NE)
cor.test(master$Var_coop, master$Var_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Var_NE, y=Var_coop, color=Game_type)) + geom_point() + geom_smooth()

## DG analysis

# H1
cor.test(DG$Avg_coop, DG$Avg_NE, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Avg_NE, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm")

# H2a : correlation between average cooperation and norm (NE)
cor.test(DG$Avg_coop, DG$Var_NE, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Var_NE, y=Avg_coop)) + geom_point() + geom_smooth()

# H2b : correlation between variance cooperation and norm (NE)
cor.test(DG$Var_coop, DG$Var_NE, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Var_NE, y=Var_coop, color=Game_type)) + geom_point() + geom_smooth()

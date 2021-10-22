library(ggplot2)
library(tidyverse)

setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/")

# read data 
master <- read.csv("Paper_csv/Master.csv") %>% mutate(norm_str = Avg_NE/Var_NE)
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

#H3 
cor.test(master$Avg_coop, master$norm_str, method= "spearman", exact = F)
ggplot(data=master, aes(x=norm_str, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm")

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

#H3 
cor.test(DG$Avg_coop, DG$norm_str, method= "spearman", exact = F)
ggplot(data=DG, aes(x=norm_str, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm")

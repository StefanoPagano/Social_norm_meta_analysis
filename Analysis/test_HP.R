library(ggplot2)
library()

# read data 
master <- read.csv("../Paper_csv/Master.csv")
DG <- master %>% subset.data.frame(Game_type=="DG")

# H1 : correlation between average cooperation and norm (NE)
cor.test(master$Avg_coop, master$Avg_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Avg_NE, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm")

# H2 : correlation between average cooperation and norm (NE)
cor.test(master$Avg_coop, master$Var_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Var_NE, y=Avg_coop, color=Game_type)) + geom_point() + geom_smooth()

## DG analysis

# H1
cor.test(DG$Avg_coop, DG$Avg_NE, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Avg_NE, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm")

# H2 : correlation between average cooperation and norm (NE)
cor.test(DG$Avg_coop, DG$Var_NE, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Var_NE, y=Avg_coop)) + geom_point() + geom_smooth()


library(ggplot2)
library()

# read data 
master <- read.csv("../Paper_csv/Master.csv")

# H1 : correlation between average cooperation and norm (NE)

cor.test(master$Avg_coop, master$Avg_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Avg_NE, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm") + g

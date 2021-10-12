library(ggplot2)

# read data 
master <- read.csv("../Paper_csv/Master.csv")

# H1 : correlation between average cooperation and norm (NE)

cor.test(master$Avg_coop, master$Avg_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=))
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggpubr)

master <- Strength_comparison #%>% filter(strength_old>1 & strength_old <8.5)

ggplot(data=master, aes(x=strength_old, y=delta_old)) + geom_point() + geom_smooth(method = "lm") + ggtitle("old strength") + stat_cor(method = "spearman") + xlab("strength") + ylab("delta")

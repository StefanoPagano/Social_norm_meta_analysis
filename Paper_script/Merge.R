library(tidyverse)
library(readxl)
rm(list = ls())

# Run paper script
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2015Ves034.R")
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2016Kim003.R")
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2017Tho028.R")
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2018Her061.R")
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2019Cha026.R")
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2019Tjo060.R")
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2020And089.R")
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2020Bas115.R")
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2021Kon127.R")
#problema paper... forse la task iniziale influenza molto la norma... non è 50% ma 83%
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2017Del037.R")

# Create the master file -> merge all csv file
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/")
Paper_034=read.csv("2015Ves034_finaldf.csv", sep=",")
master <- Paper_034

Paper_003=read.csv("2016Kim003_finaldf.csv", sep=",")
master <- Paper_003 %>% 
  rbind.data.frame(master)

Paper_028=read.csv("2017Tho028_finaldf.csv", sep=",")
master <- Paper_028 %>% 
  rbind.data.frame(master)

Paper_061=read.csv("2018Her061_finaldf.csv", sep=",")
master <- Paper_061 %>% 
  rbind.data.frame(master)

Paper_026=read.csv("2019Cha026_finaldf.csv", sep=",")
master <- Paper_026 %>% 
  rbind.data.frame(master)

Paper_060=read.csv("2019Tjo060_finaldf.csv", sep=",")
master <- Paper_060 %>% 
  rbind.data.frame(master)

Paper_089=read.csv("2020And089_finaldf.csv", sep=",")
master <- Paper_089 %>% 
  rbind.data.frame(master)

Paper_115=read.csv("2020Bas115_finaldf.csv", sep=",")
master <- Paper_115 %>% 
  rbind.data.frame(master)

Paper_127=read.csv("2021Kon127_finaldf.csv", sep=",")
master <- Paper_127 %>% 
  rbind.data.frame(master)

Paper_037=read.csv("2017Del037_finaldf.csv", sep=",")
master <- Paper_037 %>% 
  rbind.data.frame(master)


# save master file as csv
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/Paper_csv/")

write.csv(master, file = "Master.csv", row.names = F)

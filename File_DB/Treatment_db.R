library(tidyverse)
library(readxl)
rm(list = ls())

# Run paper script
setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2013Kru001.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2016Kim003.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2019Cha026.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2017Tho028.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2015Ves034.R")

#problema paper... forse la task iniziale influenza molto la norma... non � 50% ma 83%
setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2017Del037.R")

# i trattamenti sono diversi in base alla nazione... mancano i dati sulle norme
setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2018Kim038.R")

# media norma 100%
setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2017Ves049.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2019Tjo060.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2018Her061.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2019Dro082.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2020And089.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2020Bas115.R")

# mancano i dati di kw, da richiedere
setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2017Sen116.R")

# setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
# source("2021Kon127.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2012Laz164.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2007Lis165.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2021Sze167.R")

setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_script/")
source("2022Tve168.R")

rm(list = ls())


# Create the master file -> merge all csv file
setwd("~/Documents/GitHub/Social_norm_meta_analysis/Paper_csv/")

Paper_001=read.csv("2013Kru001_finaldf.csv", sep=",")
master <- Paper_001

Paper_003=read.csv("2016Kim003_finaldf.csv", sep=",")
master <- Paper_003 %>% 
  rbind.data.frame(master)

Paper_026=read.csv("2019Cha026_finaldf.csv", sep=",")
master <- Paper_026 %>% 
  rbind.data.frame(master)

Paper_028=read.csv("2017Tho028_finaldf.csv", sep=",")
master <- Paper_028 %>% 
  rbind.data.frame(master)

Paper_034=read.csv("2015Ves034_finaldf.csv", sep=",")
master <- Paper_034 %>% 
  rbind.data.frame(master)

Paper_037=read.csv("2017Del037_finaldf.csv", sep=",")
master <- Paper_037 %>% 
  rbind.data.frame(master)

Paper_038=read.csv("2018Kim038_finaldf.csv", sep=",")
master <- Paper_038 %>%
  rbind.data.frame(master)

Paper_049=read.csv("2017Ves049_finaldf.csv", sep=",")
master <- Paper_049 %>% 
  rbind.data.frame(master)

Paper_060=read.csv("2019Tjo060_finaldf.csv", sep=",")
master <- Paper_060 %>% 
  rbind.data.frame(master)

Paper_061=read.csv("2018Her061_finaldf.csv", sep=",")
master <- Paper_061 %>% 
  rbind.data.frame(master)

Paper_082=read.csv("2019Dro082_finaldf.csv", sep=",")
master <- Paper_082 %>% 
  rbind.data.frame(master)

Paper_089=read.csv("2020And089_finaldf.csv", sep=",")
master <- Paper_089 %>% 
  rbind.data.frame(master)

Paper_115=read.csv("2020Bas115_finaldf.csv", sep=",")
master <- Paper_115 %>% 
  rbind.data.frame(master)

Paper_116=read.csv("2017Sen116_finaldf.csv", sep=",")
master <- Paper_116 %>% 
  rbind.data.frame(master)
# 
# Paper_127=read.csv("2021Kon127_finaldf.csv", sep=",")
# master <- Paper_127 %>% 
#   rbind.data.frame(master)

Paper_164=read.csv("2012Laz164_finaldf.csv", sep=",")
master <- Paper_164 %>% 
  rbind.data.frame(master)

Paper_165=read.csv("2007Lis165_finaldf.csv", sep=",")
master <- Paper_165 %>% 
  rbind.data.frame(master)

Paper_167=read.csv("2021Sze167_finaldf.csv", sep=",")
master <- Paper_167 %>% 
  rbind.data.frame(master)

Paper_168=read.csv("2022Tve168_finaldf.csv", sep=",")
master <- Paper_168 %>% 
  rbind.data.frame(master)


# save master file as csv
setwd("~/Documents/GitHub/Social_norm_meta_analysis/File_DB/Output/")

write.csv(master, file = "Treatment.csv", row.names = F)

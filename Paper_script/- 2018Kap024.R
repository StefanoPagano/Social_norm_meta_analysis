## Read in packages
library(tidyverse)
library(readxl)
rm(list = ls())

# set wd 
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2018Kap024/2018Kap024_data")

dg_s1=read.csv("Study1_NHB.csv", sep=",")

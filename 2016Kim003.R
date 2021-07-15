## Read in packages
library(Hmisc)
library(foreign)
library(tidyverse)
library(readxl)
rm(list = ls())

# set wd 
setwd("G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Dati paper/2016Kim003/2016Kim003_data/Data_JEEA_MS_5107")

#pgs=read.csv("PG_Data_bysubj.txt", sep="\t")
#tgs=read.csv("TG_Data_bysubj.csv", sep="\t")
#ug=read.csv("UG_Data.txt", sep="\t")
dg=read.csv("DG_Data.csv", sep="\t")
## the next file contains all data except the conditional PG elicitations, which we did later
norms1=read.csv("Norm_Elicitation_Data.csv")

# basic dataset
meta_dataset <- read_xlsx(path = "G:/My Drive/Meta-analysis beliefs/Social Norms meta.xlsx") ## STEFANO : TO BE CHANGED // need to create a main folder with other files ##

# DG -----------------
# get information on treatment

# cleaning DG
## exp_id : date and time of experiment
## exp_num : unique number of the experiment
## subj_id : exp_num * 100 + subj
## subj : subject in the session
## gr_id : unique group id in the dataset
## role : 1 proposer; 0 responder
## sent : amount sent; -1 if not applicable
coldg = c("exp_id","exp_num","subj_id","subj", "role", "sent")

## compute average donation
dg_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = role == 1) %>% 
  mutate(endowment = 16, cooperation = sent/endowment) %>% 
  summarise(mean_cooperation = mean(cooperation))
## compute variance donations
dg_dta_var <- dg %>% subset.data.frame(select = coldg, subset = role == 1) %>% 
  mutate(endowment = 16, cooperation = sent/endowment) %>% 
  summarise(var_cooperation = var(cooperation))

## answers[1-9] : kw appropriateness
## actions: send 16/0; 14/2; 12/4; ...; 0/16
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
label_col = as.character(seq(0,16,2))
dg_columns <- c(1, 3, 4, 11:19)
## compute norm 
dg_appropriateness_sum <- norms1[, dg_columns] %>% summarise_at(vars(answers.1.:answers.9.), sum, na.rm=T) 
# endowment is 16
dg_norm <- as.integer(label_col[which.max(dg_appropriateness_sum)])

## compute variance norm
dg_norms_var <- norms1[, dg_columns] %>% summarise_at(vars(answers.1.:answers.9.), var, na.rm=T) %>% 
  subset.data.frame(select = which.max(dg_appropriateness_sum))

# produce individual-level datasets





# print csv ------

## treatment table
#--> usi le info acquisite all'inizio'
## choice table

## belief table



##############################
label_count = 1

for (i in 1:8) {
  Norms_col_sum_i = 11 + i
  Norms_col_sum_j = Norms_col_sum_i-1
  
  answer_i <- norms1[, c(1, 3, 4, 11:19)] %>% summarise(sum(norms1[,Norms_col_sum_i]))
  answer_j <- norms1[, c(1, 3, 4, 11:19)] %>% summarise(sum(norms1[,Norms_col_sum_j]))
  
  if (answer_i > answer_j){
  dg_norms <- answer_i
  label_col = paste("answer.",label_count+1,".", sep = "")
  }
  label_count = label_count + 1
}

Answer_appropriate = paste(dg_norms, label_col)
Answer_appropriate
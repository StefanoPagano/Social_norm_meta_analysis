rm(list=ls())
library(ggplot2)
library(tidyverse)
library(mlogit)
library(sjmisc)
library(ggrepel)
library(readxl)
library(sjPlot)
library(ggpubr)
library(foreign)
library(survival)
library(gridExtra)
library(kableExtra)
library(car)
library(DT)
library(zoo)

# set directory
setwd("File_DB/Output/")
master <- read.csv("Treatment.csv", sep=",") %>%
  mutate(treatment_id = paste(PaperID,"_",TreatmentCode,sep=""))

#scenarios in bully version between -5 and 5. Following I create a unique scenarios between 0 and 10.
beliefs <- read.csv("Subjects_beliefs.csv", sep=",") %>%
  filter(paper_id =="2013Kru001") %>%
  mutate(scenarios_unique=ifelse(treatment_id=="2013Kru001_1b", scenarios+5, scenarios))

choices <- read.csv("Subjects_choices.csv", sep=",") %>%
  filter(paper_id =="2013Kru001") %>%
  mutate(dummy_bully=ifelse(treatment_id=="2013Kru001_1b",1,0)) %>%
  mutate(scenarios_unique=ifelse(treatment_id=="2013Kru001_1b", scenarios+5, scenarios))

#compute avg norm
mean_beliefs <- beliefs %>%
  group_by(treatment_id, scenarios_unique) %>%
  summarise(mean_app = mean(KW_Normative))

utility_df_treatment <- choices %>%
  merge.data.frame(mean_beliefs) %>%
  arrange(paper_id,treatment_id,subject_id,scenarios_unique) %>%
  mutate(payoff = endowment - scenarios)

df <- mlogit.data(utility_df_treatment, choice = "A", shape = "long", alt.var = "scenarios_unique")

ml.paper1 <- mlogit(A ~ 0 + payoff + mean_app, df)
ml.paper2 <- mlogit(A ~ 0 + payoff + mean_app + mean_app:dummy_bully, df)

tab_model(ml.paper1, ml.paper2 , show.aic=T, show.loglik=T, show.se=T, show.ci=F , show.est=T)

all_model <- predict(ml.paper1)

barplot(as.numeric(all_model), names.arg = c(0:10))


#comparing standard vs bully

df_standard <- mlogit.data(utility_df_treatment%>% filter(treatment_id=="2013Kru001_1a"), choice = "A", shape = "long", alt.var = "scenarios_unique")
ml.standard <- mlogit(A ~ 0 + payoff + mean_app, df_standard)
pred_standard <- predict(ml.standard)
barplot(as.numeric(pred_standard), names.arg = c(0:10))

df_bully <- mlogit.data(utility_df_treatment %>% filter(treatment_id=="2013Kru001_1b"), choice = "A", shape = "long", alt.var = "scenarios_unique")
ml.bully <- mlogit(A ~ 0 + payoff + mean_app, df_bully)
pred_bully <- predict(ml.bully)
barplot(as.numeric(pred_bully), names.arg = c(0:10))

plot_data <- data.frame(choice = rep(0:10,2), prob = c(as.numeric(pred_standard),as.numeric(pred_bully)), Type=c(rep("Standard",11),rep("Bully",11)))

ggplot(data=plot_data, aes(y = prob, x = choice, fill=Type)) + geom_col(position = "dodge") + labs(title = "Predict")

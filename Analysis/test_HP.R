library(ggplot2)
library(tidyverse)
library(mclogit)
library(mlogit)
library(sjmisc)
library(ggrepel)
library(sjPlot)
library(readxl)

setwd("C:/Users/stefa/Documenti/CNR/GitHub/Social_norm_meta_analysis/")

# read data 
master <- read.csv("File_DB/Output/Treatment.csv") %>% mutate(scarto = Avg_NE - Avg_coop, Strength_N = Avg_NE/Sd_Avg_NE)
DG <- master %>% subset.data.frame(Game_type=="DG" & Choice_Method== "Direct")
DG_UG <- master %>% subset.data.frame(Game_type=="DG"|Game_type=="UG")
UG <- master %>% subset.data.frame(Game_type=="UG")

# H1 : correlation between average cooperation and norm (NE)
cor.test(master$Avg_coop, master$Avg_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Avg_NE, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm") + geom_text(aes(label=PaperID)) + ggtitle("H1 : correlation between average cooperation and norm (NE)")

# H2a : correlation between average cooperation and norm (NE)
cor.test(master$Avg_coop, master$Strength_N, method= "spearman", exact = F)
ggplot(data=master, aes(x=Sd_Avg_NE, y=Avg_coop, color=Game_type)) + geom_point() + geom_smooth() + geom_text(aes(label=PaperID)) + ggtitle("H2a : correlation between average cooperation and norm (NE)")

# H2b : correlation between variance cooperation and norm (NE)
cor.test(master$Var_coop, master$Strength_N, method= "spearman", exact = F)
ggplot(data=master, aes(x=Strength_N, y=Var_coop, color=Game_type)) + geom_point() + geom_smooth() + geom_text(aes(label=PaperID)) + ggtitle("H2b : correlation between variance cooperation and norm (NE)")

#H3 
cor.test(master$Avg_coop, master$Strength_N, method= "spearman", exact = F)
ggplot(data=master, aes(x=Strength_N, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm") + geom_text(aes(label=PaperID)) + ggtitle("H3")

## DG analysis

# H1
cor.test(DG$Avg_coop, DG$Avg_NE, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Avg_NE, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm") + geom_text(aes(label=PaperID)) + ggtitle("H1 : correlation between average cooperation and norm (NE) \n DG only")

# H2a : correlation between average cooperation and norm (NE)
cor.test(DG$Avg_coop, DG$Strength_N, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Strength_N, y=Avg_coop)) + geom_point() + geom_smooth() + geom_text(aes(label=PaperID)) + ggtitle("H2a : correlation between average cooperation and norm (NE) \n DG only")

# H2b : correlation between variance cooperation and norm (NE)
cor.test(DG$Var_coop, DG$Strength_N, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Strength_N, y=Var_coop, color=Game_type)) + geom_point() + geom_smooth() + geom_text(aes(label=PaperID)) + ggtitle("H2b : correlation between variance cooperation and norm (NE) \n DG only")

#H3 
cor.test(DG$Avg_coop, DG$Strength_N, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Strength_N, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm") + geom_text(aes(label=PaperID)) + ggtitle("H3 \n DG only")

## DG & UG

# H1 : correlation between average cooperation and norm (NE)
wilcox.test(UG$Avg_NE, DG$Avg_NE)
ggplot(data=DG_UG, aes(x=Game_type, y=Avg_NE)) + geom_boxplot(outlier.shape = NA) + scale_y_continuous(limits = c(0, 0.6))


## INDIVIDUAL-LEVEL ANALYSIS
beliefs <- read.csv("File_DB/Output/Subjects_beliefs.csv", sep = ",")
choices <- read.csv("File_DB/Output/Subjects_choices.csv", sep = ",")

# within design -----

## issues: 
### - 345 subjects but only 342 in preliminary analysis: check if subjects are the same and scenarios are all for everyone

beliefs_w <- beliefs %>% subset.data.frame(subset = Design=="Within")
choices_w <- choices %>% subset.data.frame(subset = Design=="Within")
individual_db_w <- merge.data.frame(choices_w, beliefs_w)

# prelim analysis all together
model_1 <- mclogit(cbind(A,scenarios)~KW_Normative,data=individual_db_w)

model_2017Del037 <- mclogit(cbind(A,scenarios)~KW_Normative,data=individual_db_w %>% 
                            subset.data.frame(subset = paper_id =="2017Del037"))

model_2020Bas115 <- mclogit(cbind(A,scenarios)~KW_Normative,data=individual_db_w %>% 
                              subset.data.frame(subset = paper_id=="2020Bas115"))

tab_model(model_1, model_2017Del037, 
          model_2020Bas115, 
          dv.labels = c("general model","Del037_model_1","2020Bas115"))

plot_models(model_1, model_2017Del037, 
          model_2020Bas115)

# between design -----
beliefs_b <- beliefs %>% subset.data.frame(subset = Design=="Between") %>% group_by(paper_id, scenarios) %>% summarise(KW_Normative = mean(KW_Normative))
choices_b <- choices %>% subset.data.frame(subset = Design=="Between")
individual_db_b <- merge.data.frame(choices_b, beliefs_b)

mlogit.db <- mlogit.data(individual_db_b, choice = 'scenarios', shape = 'long')
model_2 <- mlogit(cbind(A,scenarios)~KW_Normative,data=individual_db_b)

model_2016Kim003 <- mclogit(cbind(A,scenarios)~KW_Normative,data=individual_db_b %>% 
                              subset.data.frame(subset = paper_id =="2016Kim003"))

model_2018Her061 <- mclogit(cbind(A,scenarios)~KW_Normative,data=individual_db_b %>% 
                              subset.data.frame(subset = paper_id =="2018Her061"))

model_2019Cha026 <- mclogit(cbind(A,scenarios)~KW_Normative,data=individual_db_b %>% 
                              subset.data.frame(subset = paper_id =="2019Cha026"))

tab_model(model_2, model_2016Kim003, model_2018Her061, model_2019Cha026,
          dv.labels = c("General model between", "2016Kim003", "2018Her061", "2019Cha026"))

plot_models(Tho028_model, Bas115_model, Del037_model)

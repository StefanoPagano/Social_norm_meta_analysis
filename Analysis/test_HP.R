library(ggplot2)
library(tidyverse)
library(mclogit)
library(sjmisc)
library(ggrepel)
library(sjPlot)

setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis/")

# read data 
master <- read.csv("Paper_csv/Master.csv") %>% mutate(norm_str = Avg_NE/Var_NE)
DG <- master %>% subset.data.frame(Game_type=="DG")
DG_UG <- master %>% subset.data.frame(Game_type=="DG"|Game_type=="UG")
UG <- master %>% subset.data.frame(Game_type=="UG")
# H1 : correlation between average cooperation and norm (NE)
cor.test(master$Avg_coop, master$Avg_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Avg_NE, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm") + geom_text(aes(label=PaperID))

# H2a : correlation between average cooperation and norm (NE)
cor.test(master$Avg_coop, master$Var_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Var_NE, y=Avg_coop, color=Game_type)) + geom_point() + geom_smooth() + geom_text(aes(label=PaperID))

# H2b : correlation between variance cooperation and norm (NE)
cor.test(master$Var_coop, master$Var_NE, method= "spearman", exact = F)
ggplot(data=master, aes(x=Var_NE, y=Var_coop, color=Game_type)) + geom_point() + geom_smooth() + geom_text(aes(label=PaperID))

#H3 
cor.test(master$Avg_coop, master$norm_str, method= "spearman", exact = F)
ggplot(data=master, aes(x=norm_str, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm") + geom_text(aes(label=PaperID))

## DG analysis

# H1
cor.test(DG$Avg_coop, DG$Avg_NE, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Avg_NE, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm") + geom_text(aes(label=PaperID))

# H2a : correlation between average cooperation and norm (NE)
cor.test(DG$Avg_coop, DG$Var_NE, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Var_NE, y=Avg_coop)) + geom_point() + geom_smooth() + geom_text(aes(label=PaperID))

# H2b : correlation between variance cooperation and norm (NE)
cor.test(DG$Var_coop, DG$Var_NE, method= "spearman", exact = F)
ggplot(data=DG, aes(x=Var_NE, y=Var_coop, color=Game_type)) + geom_point() + geom_smooth() + geom_text(aes(label=PaperID))

#H3 
cor.test(DG$Avg_coop, DG$norm_str, method= "spearman", exact = F)
ggplot(data=DG, aes(x=norm_str, y=Avg_coop)) + geom_point() + geom_smooth(method = "lm") + geom_text(aes(label=PaperID))

## DG & UG

# H1 : correlation between average cooperation and norm (NE)
wilcox.test(UG$Avg_NE, DG$Avg_NE)
ggplot(data=DG_UG, aes(x=Game_type, y=Avg_NE)) + geom_boxplot(outlier.shape = NA) + scale_y_continuous(limits = c(0, 0.6))


## INDIVIDUAL-LEVEL ANALYSIS
setwd("C:/Users/stefa/Documents/CNR/GitHub/Social_norm_meta_analysis")
Tho028_output <- read.csv("File_DB/Tho028_output.csv", sep = ",")
Bas115_output <- read.csv("File_DB/Bas115_output.csv", sep = ",")
Del037_output <- read.csv("File_DB/Del037_output.csv", sep = ",")

# per treatment 
Tho028_model_1 <- mclogit(cbind(A,scenarios)~KW_score,data=Tho028_output %>% subset.data.frame(subset = treatment_id =="2017Tho028_1"), random = ~1|subject_id)

Tho028_model_2 <- mclogit(cbind(A,scenarios)~KW_score,data=Tho028_output %>% subset.data.frame(subset = treatment_id =="2017Tho028_2"), random = ~1|subject_id)

Bas115_model_1 <- mclogit(cbind(A,scenarios)~KW_score,data=Bas115_output %>% subset.data.frame(subset = treatment_id=="2020Bas115_1a"), random = ~1|subject_id)

Bas115_model_2 <- mclogit(cbind(A,scenarios)~KW_score,data=Bas115_output %>% subset.data.frame(subset = treatment_id=="2020Bas115_2a"), random = ~1|subject_id)

Del037_model_1 <- mclogit(cbind(A,scenarios)~KW_score,data=Del037_output %>% subset.data.frame(treatment_id=="2017Del037_1"), random = ~1|subject_id)

Del037_model_2 <- mclogit(cbind(A,scenarios)~KW_score,data=Del037_output %>% subset.data.frame(treatment_id=="2017Del037_2"), random = ~1|subject_id)

Del037_model_3 <- mclogit(cbind(A,scenarios)~KW_score,data=Del037_output %>% subset.data.frame(treatment_id=="2017Del037_3"), random = ~1|subject_id)

tab_model(Tho028_model_1, Tho028_model_2, 
          Bas115_model_1, Bas115_model_2,
          Del037_model_1, Del037_model_3,
          dv.labels = c("Tho028_model_1", "Tho028_model_2", 
                        "Bas115_model_1", "Bas115_model_2", 
                        "Del037_model_1", "Del037_model_3"))

plot_models(Tho028_model_1, Tho028_model_2, 
            Bas115_model_1, Bas115_model_2,
            Del037_model_1, Del037_model_3)

# per paper
Tho028_model <- mclogit(cbind(A,scenarios)~KW_score,data=Tho028_output, random = ~1|subject_id)

Bas115_model <- mclogit(cbind(A,scenarios)~KW_score,data=Bas115_output, random = ~1|subject_id)

Del037_model <- mclogit(cbind(A,scenarios)~KW_score,data=Del037_output, random = ~1|subject_id)

tab_model(Tho028_model, Bas115_model, Del037_model, 
          dv.labels = c("Tho028_model", "Bas115_model", "Del037_model"))

plot_models(Tho028_model, Bas115_model, Del037_model)



## avg_coop bassi
# read data 

master_sub <- read.csv("Paper_csv/Master.csv") %>% mutate(norm_str = Avg_NE/Var_NE) %>% subset.data.frame(subset = Avg_coop < 0.3)


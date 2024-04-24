## create stata readable files from raw dataset

##### ISSUE : 2016Kim003_7 has no beliefs data for odd numbered scenarios (interpolation). Find why in the originating codes. check lines 467 of individual_between.R

rm(list=ls())
library(tidyverse)
setwd("~/GitHub/Social_norm_meta_analysis")

papers_included<-c("2007Lis165_1a","2007Lis165_1b","2012Laz164_3","2013Kru001_1a","2013Kru001_1b","2016Kim003_7","2018Her061_9","2019Cha026_1","2019Cha026_3","2020And089_1","2020And089_2","2020Bas115_2a")
master <- read.csv("File_DB/Output/Treatment.csv") %>%
  mutate(treatment_id=paste(PaperID,TreatmentCode,sep="_"))
beliefs <- read.csv("File_DB/Output/Subjects_beliefs.csv", sep = ",") %>% filter(treatment_id %in% papers_included)
choices <- read.csv("File_DB/Output/Subjects_choices.csv", sep = ",") %>% filter(treatment_id %in% papers_included)

# interpolating beliefs for kim

## interpolation of appropriateness with scenarios out of game
beliefs_only_kim <- beliefs %>%
  filter(paper_id == "2016Kim003" & treatment_id == "2016Kim003_7") %>%
  dplyr::select(paper_id, treatment_id, subject_id, scenarios, KW_Normative, Game_type, Design) %>%
  mutate(KW_Normative_temp = KW_Normative)

n_subj = length(levels(as.factor(beliefs_only_kim$subject_id)))

kim003_interpolation <- data.frame(paper_id=NA,
                                   treatment_id=NA,
                                   subject_id = NA,
                                   scenarios=NA,
                                   Game_type="DG",
                                   Design="Between",
                                   KW_Normative=NA,
                                   KW_Normative_temp=NA)

for (subj in levels(as.factor(beliefs_only_kim$subject_id))) {
  subj_df <- data.frame(paper_id="2016Kim003",
                        treatment_id="2016Kim003_7",
                        subject_id = subj,
                        Game_type="DG",
                        Design="Between",
                        scenarios=seq(1,15,2),
                        KW_Normative = NA,
                        KW_Normative_temp=NA)
  
  subj_beliefs <- beliefs_only_kim %>%
    filter(subject_id == subj)
  #print(subj_beliefs)
  
  subj_beliefs <- subj_beliefs %>%
    rbind.data.frame(subj_df) %>%
    arrange(paper_id, treatment_id, subject_id, scenarios) %>%
    mutate(KW_Normative = zoo::na.approx(KW_Normative_temp,
                                    maxgap = 1,
                                    rule = 1))
  
  kim003_interpolation <- subj_beliefs %>%
    rbind.data.frame(kim003_interpolation)
  #print(kim003_interpolation)
}

beliefs_all <- beliefs %>% plyr::rbind.fill(kim003_interpolation %>% filter(scenarios %in% seq(1,15,2)) %>% subset.data.frame(select=-c(KW_Normative_temp)))
# create table of beliefs aggregated by paper for all papers (between-subjects and within-subjects)
beliefs_aggregated <- beliefs %>% group_by(treatment_id, scenarios) %>% summarise(mean_app = mean(KW_Normative, na.rm=T), sd_app = sd(KW_Normative, na.rm = T)) 

# create comprehensive table with choices and beliefs
temp <- choices %>% merge.data.frame(beliefs_aggregated, by = c("treatment_id", "scenarios"), all.x = T)


final_data <- temp %>% filter(treatment_id %in% papers_included)

write.csv(file = "Analysis/new_data_utility.csv", x=final_data, row.names = F)

## used for bootstrap
write.csv(file="prova/Subjects_beliefs.csv", x=beliefs_all, row.names = F)

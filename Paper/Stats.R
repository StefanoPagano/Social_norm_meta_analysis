# windows
social_norms <- read_xlsx(path = "G:/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL")
# mac
social_norms <- read_xlsx(path = "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IoJDOQWCFiL1qTzSja6byrAlCelNSTsT/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL")


# number of paper
n_paper_total <- nrow(distinct(social_norms %>% select(PaperID)))
n_treat_total <- nrow(distinct(social_norms %>% select(treatment_id)))

n_paper_first <- nrow(distinct(social_norms %>% filter(Game_type %in% c("DG", "Donation Game", "ToG")) %>% select(PaperID)))
n_treat_first <- nrow(distinct(social_norms %>% filter(Game_type %in% c("DG", "Donation Game", "ToG")) %>% select(treatment_id)))
n_treat_ok <- nrow(distinct(social_norms %>% filter(Game_type %in% c("DG", "Donation Game", "ToG") & StatusTreatment_Roma != "1-Not eligible treatment" & Method_elicitation!="Bicchieri" & Baseline %in% c(1, NA) & Standard_game != "N") %>% select(treatment_id)))

print(paste("Total number of papers: ", n_paper_first, "/", n_paper_total, ". Treatments: ", n_treat_first, "/", n_treat_total, sep = ""))
print(paste("Treatment in target: " , n_treat_ok))

target <- social_norms %>% filter(Game_type %in% c("DG", "Donation Game", "ToG") & StatusTreatment_Roma != "1-Not eligible treatment" & Method_elicitation!="Bicchieri" & Baseline %in% c(1, NA) & Standard_game != "N")


target %>% 
  group_by(Game_type) %>%
  summarise(n=n(),
            between=sum(Separate_sample_beliefs=="Y", na.rm = T),
            KW=sum(Method_elicitation=="KW", na.rm = T),
            BX=sum(Method_elicitation=="Bicchieri", na.rm = T),
            KWBX=sum(Method_elicitation=="Both", na.rm = T),
            OnlyNorm=sum(Choice_Method=="OnlyNorms", na.rm = T),
            Available=sum(StatusTreatment_Roma=="6-Complete", na.rm = T))
library(tidyverse)
library(readxl)


#inserire percorso del file excel
Social_Norms_meta <- read_excel("Google Drive/Il mio Drive/Meta-analysis beliefs/Social Norms meta.xlsx", 
                                sheet = "ALL")

df <- Social_Norms_meta %>%
  group_by(Game_type) %>%
  subset.data.frame(subset = Game_type %in% c("DG","UG", "PGG", "TG", "BG", "GEG", "PDG") & 
                      Environment %in% c("Classroom", "Lab", "Online") & 
                      Method_elicitation=="KW"
                    ) %>%

  summarise(
    N_Treatments = n(),
    N_Separate_beliefs = sum(Separate_sample_beliefs=="Y"),
    N_OneShot_Treatments = sum(One_Shot_Repeated=="OneShot"),
    N_Monetary_Incentivized_experiment = sum(Monetary_Incentivized_experiment=="Y")
  )

ggplot(data=df, aes(x= reorder(Game_type,-N_Treatments), y=N_Treatments, fill=N_Treatments)) + 
  geom_col() + 
  labs(x="Game Type",
       y="Count",
       title="Number of Treatments per Game Types")

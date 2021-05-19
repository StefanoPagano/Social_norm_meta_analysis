library(tidyverse)
library(readxl)
rm(list = ls())

#inserire percorso del file excel
Social_Norms_meta <- read_excel("Google Drive/Il mio Drive/Meta-analysis beliefs/Social Norms meta.xlsx", 
                                sheet = "ALL")

WP_Filter_F <- function()
  {
    X <- readline(prompt = "Would you like to analyze Working paper? TRUE/FALSE ->")
    WP_State <- as.logical(X)
  
    return(WP_State)
}

if (WP_Filter_F()==TRUE) {
    WP_Filter <- "Working Paper"
  } else {
    WP_Filter <- NULL
  }
df <- Social_Norms_meta %>%
  group_by(Game_type) %>%
  subset.data.frame(subset = Game_type %in% c("DG","UG", "PGG", "TG", "BG", "GEG", "PDG", "Donation Game") & 
                      Environment %in% c("Classroom", "Lab", "Online") & 
                      Method_elicitation=="KW" &
                      Published %in% c("Published", WP_Filter)
                    ) %>%

  summarise(
    N_Treatments = n(),
    N_Separate_beliefs = sum(Separate_sample_beliefs=="Y"),
    N_OneShot_Treatments = sum(One_Shot_Repeated=="OneShot"),
    N_Monetary_Incentivized_experiment = sum(Monetary_Incentivized_experiment=="Y")
  ) %>%
  arrange(-N_Treatments)

ggplot(data=df, aes(x= reorder(Game_type,-N_Treatments), y=N_Treatments, fill=N_Treatments)) + 
  geom_col() + 
  labs(x="Game Type",
       y="Count",
       title="Number of Treatments per Game Types")

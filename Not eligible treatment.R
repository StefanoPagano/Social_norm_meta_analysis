library(tidyverse)
library(stringr)
library(readxl)
library(writexl)
rm(list = ls())

#---------------------------------------------------------------------------------------
#Insert excel file path
#---------------------------------------------------------------------------------------

Social_Norms_meta <- read_excel("Google Drive/Il mio Drive/Meta-analysis beliefs/Social Norms meta.xlsx", 
                                sheet = "ALL")

Social_Norms_meta$TreatmentID <- paste(Social_Norms_meta$PaperID,Social_Norms_meta$TreatmentCode,sep = "_")

#---------------------------------------------------------------------------------------
#"Not in" operator
#---------------------------------------------------------------------------------------

'%not_in%' <- Negate('%in%')

#---------------------------------------------------------------------------------------
#Report creation
#---------------------------------------------------------------------------------------
GameTypeList <- c("DG", "PDG", "PGG", "TG", "UG", "GEG", "Donation Game")
EnvironmentList <- c("Lab", "Classroom", "Online")
ElicitList <- c("KW")


GameType_F <- function()
{
  x <- c()
  for (i in Social_Norms_meta$Game_type){
    #print(i)
    if (i %not_in% GameTypeList){
      x <- append(x, "Game Type")
      }else{
        x <- append(x, NA)}
  }
  return(x)
}


Elicitation_F <- function()
{
  x <- c()
  for (i in Social_Norms_meta$Method_elicitation){
    if (i %not_in% ElicitList){
      x <- append(x, "Elicitation")
      }else{
        x <- append(x, NA)}
    }
  return(x)
}



Field_F <- function()
{
  x <- c()
  for (i in Social_Norms_meta$Environment){
    if (i %not_in% EnvironmentList){
      x <- append(x, "Environment")
    }else{
      x <- append(x, NA)}
  }
  return(x)
}

Social_Norms_meta$Problem <- paste(GameType_F(),Elicitation_F(), Field_F(), sep = ", ")
result <- Social_Norms_meta$Problem
result

Report <- subset(Social_Norms_meta, select = c(n_Paper, TreatmentID, Problem))


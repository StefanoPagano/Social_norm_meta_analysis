## STATA LOG FILE READING 
setwd("C:/Users/a.guido/Documents/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Ancillary")
t=14 # modify this based on the number of treatments to consider

library(tidyverse)
# 1. QUESTO SCRIPT LEGGE IL FILE LOG AIC DA STATA ---------------
## MODIFICARE SUFFISSO "_DG" CON "_ToG" O "_DON"
## MODIFICARE n_max = x, x ? UGUALE AL NUMERO DI TRATTAMENTI

log_file = "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/stata_AIC_ONG.log"
df_aic <- read_log(file=log_file,
                   skip=10,
                   n_max = t)
colnames(df_aic) <- c("treatment_id", "Selfish_aic", "Norm_aic", "DA_aic", "FU_aic")

avg_aic <- df_aic %>% 
  summarise(across(Selfish_aic:FU_aic, ~mean(., na.rm=T)))

df_aic <- df_aic %>%
  add_row(treatment_id="Average", avg_aic)

write.csv(df_aic, "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/AIC_DG.csv", row.names = F)

## per modelli con norm uncertainty

log_file = "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/uncertain_stata_AIC_NU.log"
df_aic <- read_log(file=log_file,
                   skip=10,
                   n_max = t)
colnames(df_aic) <- c("treatment_id", "Full")

avg_aic <- df_aic %>% 
  summarise(across(Full, ~mean(., na.rm=T)))

df_aic <- df_aic %>%
  add_row(treatment_id="Average", avg_aic)

write.csv(df_aic, "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/uncertainty_AIC_DG.csv", row.names = F)

# 2. QUESTO SCRIPT LEGGE IL FILE LOG COEFF DA STATA -------------
## MODIFICARE n_max = x, x ? UGUALE AL NUMERO DI TRATTAMENTI

log_file = "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/stata_COEFF_ONG.log"
df_coeff <- read_log(file=log_file,
                     skip=10,
                     n_max = t,
                     col_types=cols("c","n","n","n","n","n","n","n","n","n","n","n","n"))

colnames(df_coeff) <- c("treatment_id", "deltaS", "deltaN", "gammaN", "rhoDA", "sigmaDA", "rhoFU", "sigmaFU", "gammaFU")

observation <- read.csv("../Data/new_data_utility2025-07-23.csv") %>% distinct(treatment_id,subject_id,Game_type) %>% group_by(treatment_id,Game_type) %>% tally()

cols_to_check <- c("sigmaDA", "rhoFU", "sigmaFU")

df_coeff <- df_coeff %>% mutate(across(all_of(cols_to_check), ~ ifelse(. == 0, NA, .))) %>%
  merge.data.frame(observation, by = "treatment_id")

# Uncomment this if want to use obs. weights
#avg_coeff <- df_coeff %>% 
#  summarise(across(deltaS:gammaFU, ~weighted.mean(., w = n, na.rm=T)))
#df_coeff <- df_coeff %>%
#  add_row(treatment_id="Average", avg_coeff, n=sum(df_coeff$n), Game_type="DG")

#QUESTO SCRIPT LEGGE IL FILE LOG STD ERR DA STATA ------

log_file = "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/stata_SE_ONG.log"
df_se <- read_log(file=log_file,
                  skip=10,
                  n_max = t,
                  col_types=cols("c","n","n","n","n","n","n","n","n","n","n","n","n"))

colnames(df_se) <- c("treatment_id", "se_deltaS", "se_deltaN", "se_gammaN", "se_rhoDA", "se_sigmaDA", "se_rhoFU", "se_sigmaFU", "se_gammaFU")

# Uncomment this if want to use obs. weights

#df_se <- df_se %>%
#  merge.data.frame(observation, by = "treatment_id") %>%
#  mutate(weight_SE=n^2)

#avg_se <- df_se %>% 
#  summarise(across(se_deltaS:se_gammaFU, ~weighted.mean(.^2, w = weight_SE, na.rm=T)))
#df_se <- df_se %>%
#  add_row(treatment_id="Average", sqrt(avg_se), n=sum(df_se$n), weight_SE=sum(df_se$weight_SE))

# XXX CHECK ANDREA #################
# avg coefficient with inverse variance
df_se[, 2:9][df_se[, 2:9]==0] <- NA
num <- colSums(df_coeff[,2:9]*(1/df_se[,2:9]^2), na.rm=T)
den <- colSums(1/df_se[,2:9]^2, na.rm=T)
ration <- data.frame(t(num/den))

df_coeff <- df_coeff %>%
  add_row(treatment_id="Average", ration, n=sum(df_coeff$n), Game_type="DG")

# compute variance of coefficient (1/den)
se_average <- data.frame(t(sqrt(1/den)))
colnames(se_average) <- colnames(df_se)[2:9]

df_se <- df_se %>%
  add_row(treatment_id="Average", se_average)#, n=sum(df_se$n) Uncomment when using obs as weights, weight_SE=sum(df_se$weight_SE))

##################


# produce 95% CI of averages
df_models <- merge.data.frame(df_coeff, df_se, by = "treatment_id")
average_ci_u <- df_coeff[t+1,2:9] + 1.96*df_se[t+1,2:9]
average_ci_l <- df_coeff[t+1,2:9] - 1.96*df_se[t+1,2:9]

rbind.data.frame(average_ci_l, df_coeff[t+1,2:9], average_ci_u)
write.csv(round(rbind.data.frame(average_ci_l, df_coeff[t+1,2:9], average_ci_u), 3), "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/95CI_MODEL_DG.csv", row.names = F)
write.csv(df_models, "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/MODEL_DG.csv", row.names = F)

# 3. MODELLI CON NORM UNCERTAINTY ----------------
#QUESTO SCRIPT LEGGE IL FILE LOG COEFF DA STATA
## MODIFICARE n_max = x, x ? UGUALE AL NUMERO DI TRATTAMENTI

log_file = "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/uncertain_stata_COEFF_NU.log"
df_coeff <- read_log(file=log_file,
                     skip=10,
                     n_max = t,
                     col_types=cols("c","n","n","n","n","n","n","n","n","n","n","n","n"))

#colnames(df_coeff) <- c("treatment_id", "basegammaNU", "baseetaNU", "gammaNU", "etaNU", "nuNU", "baserhoNU", "basesigmaNU", "rhoNU", "sigmaNU")
colnames(df_coeff) <- c("treatment_id", "basedeltaNU", "basegammaNU", "baseetaNU", "deltaNU", "gammaNU", "etaNU", "nuNU", "baserhoNU", "basesigmaNU", "rhoNU", "sigmaNU")

observation <- read.csv("../Data/new_data_utility2025-07-23.csv") %>% distinct(treatment_id,subject_id,Game_type) %>% group_by(treatment_id,Game_type) %>% tally()

df_coeff <- df_coeff %>%
  merge.data.frame(observation, by = "treatment_id")

# Uncomment this if want to use obs. weights
#avg_coeff <- df_coeff %>% 
#  summarise(across(basegammaNU:sucaNU, ~weighted.mean(., w = n, na.rm=T)))
#df_coeff <- df_coeff %>%
#  add_row(treatment_id="Average", avg_coeff, n=sum(df_coeff$n), Game_type="DG")

#QUESTO SCRIPT LEGGE IL FILE LOG STD ERR DA STATA

log_file = "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/uncertain_stata_SE_NU.log"
df_se <- read_log(file=log_file,
                  skip=10,
                  n_max = t,
                  col_types=cols("c","n","n","n","n","n","n","n","n","n","n","n","n"))

#colnames(df_se) <- c("treatment_id", "se_basegammaNU", "se_baseetaNU", "se_gammaNU", "se_etaNU", "se_nuNU", )
colnames(df_se) <- c("treatment_id", "se_basedeltaNU", "se_basegammaNU", "se_baseetaNU", "se_deltaNU", "se_gammaNU", "se_etaNU", "se_nuNU", "se_baserhoNU", "se_basesigmaNU", "se_rhoNU", "se_sigmaNU")

#df_se <- df_se %>%
#  merge.data.frame(observation, by = "treatment_id") %>%
#  mutate(weight_SE=n^2)

# Uncomment this if want to use obs. weights
#avg_se <- df_se %>% 
#  summarise(across(se_basegammaNU:se_sucaNU, ~weighted.mean(.^2, w = weight_SE, na.rm=T)))
#df_se <- df_se %>%
#  add_row(treatment_id="Average", sqrt(avg_se), n=sum(df_se$n), weight_SE=sum(df_se$weight_SE))

# XXX CHECK ANDREA #################
# avg coefficient with inverse variance
## remove treatments with issues
##treatment_removed = c("2023Eck169_1a")
##df_coeff[c(which(df_coeff$treatment_id=="2023Eck169_1a")), c("nuNU", "etaNU", "gammaNU", "rhoNU", "sigmaNU")] = NA
##df_se[c(which(df_se$treatment_id=="2023Eck169_1a")), c("se_nuNU", "se_etaNU", "se_gammaNU", "se_rhoNU", "se_sigmaNU")] = NA

p = 11 + 1 # number of parameters plus 1
df_se[, 2:p][df_se[, 2:p]==0] <- NA
num <- colSums(df_coeff[,2:p]/df_se[,2:p]^2, na.rm = T)
den <- colSums(1/df_se[,2:p]^2, na.rm = T)
ration <- data.frame(t(num/den))

df_coeff <- df_coeff %>%
  add_row(treatment_id="Average", ration, n=sum(df_coeff$n), Game_type="DG")

# compute variance of coefficient (1/den)
se_average <- data.frame(t(sqrt(1/den)))
colnames(se_average) <- colnames(df_se)[2:p]

df_se <- df_se %>%
  add_row(treatment_id="Average", se_average)#, n=sum(df_se$n) Uncomment if using obs as weights, weight_SE=sum(df_se$weight_SE))

##################


# produce 95% CI of averages
df_models <- merge.data.frame(df_coeff, df_se, by = "treatment_id")
average_ci_u <- df_coeff[t+1,2:p] + 1.96*df_se[t+1,2:p]
average_ci_l <- df_coeff[t+1,2:p] - 1.96*df_se[t+1,2:p]

rbind.data.frame(average_ci_l, df_coeff[t+1,2:p], average_ci_u)
write.csv(df_models, "~/GitHub/Social_norm_meta_analysis/Analysis/Utility estimation/Output/Logs/uncertainty_MODEL_DG.csv", row.names = F)

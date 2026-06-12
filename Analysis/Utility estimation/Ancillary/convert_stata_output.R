library(tidyverse)

setwd("~/GitHub/Social_norm_meta_analysis/Analysis/")

# --- Main models (S, N, DA, FU) ------------------------------------------

df <- read.csv("Utility estimation/Output/Data/results_DG.csv")

coeff_cols <- c("deltaS", "deltaN", "gammaN",
                "rhoDA", "sigmaDA",
                "rhoFU", "sigmaFU", "gammaFU")
se_cols    <- paste0("se_", coeff_cols)

# Constrained-to-zero values -> NA
constrained <- c("sigmaDA", "rhoFU", "sigmaFU")
df <- df %>% mutate(across(all_of(constrained), ~ ifelse(. == 0, NA, .)))
for (col in constrained) df[[paste0("se_", col)]][is.na(df[[col]])] <- NA

# Inverse-variance weighted average
df_se <- df[, se_cols]
df_se[df_se == 0] <- NA

num <- colSums(df[, coeff_cols] / df_se^2, na.rm = TRUE)
den <- colSums(1 / df_se^2, na.rm = TRUE)

avg_coeff <- setNames(as.data.frame(t(num / den)), coeff_cols)
avg_se    <- setNames(as.data.frame(t(sqrt(1 / den))), se_cols)
avg_aic   <- df %>% summarise(across(c(S_AIC, N_AIC, DA_AIC, FU_AIC), mean, na.rm = TRUE))

avg_row <- bind_cols(
  data.frame(treatment_id = "Average"),
  avg_coeff, avg_se, avg_aic,
  data.frame(nobs = sum(df$nobs))
)

df_out <- bind_rows(df, avg_row)
write.csv(df_out, "Utility estimation/Output/Data/model_DG.csv", row.names = FALSE)

# 95% CI for the average row
t <- nrow(df)
ci_u <- df_out[t + 1, coeff_cols] + 1.96 * df_out[t + 1, se_cols]
ci_l <- df_out[t + 1, coeff_cols] - 1.96 * df_out[t + 1, se_cols]
write.csv(
  round(rbind(ci_l, df_out[t + 1, coeff_cols], ci_u), 3),
  "Utility estimation/Output/Data/95CI_model_DG.csv",
  row.names = FALSE
)

write.csv(
  df_out %>% select(treatment_id, S_AIC, N_AIC, DA_AIC, FU_AIC),
  "Utility estimation/Output/Data/AIC_DG.csv",
  row.names = FALSE
)

# --- Norm uncertainty models (NU) ----------------------------------------

df_nu <- read.csv("Utility estimation/Output/Data/results_NU_DG.csv")

coeff_cols_nu <- c("basedeltaNU", "basegammaNU", "baseetaNU",
                   "baserhoNU",   "basesigmaNU",
                   "deltaNU", "gammaNU", "etaNU", "nuNU",
                   "rhoNU",   "sigmaNU")
se_cols_nu <- paste0("se_", coeff_cols_nu)

df_se_nu <- df_nu[, se_cols_nu]
df_se_nu[df_se_nu == 0] <- NA

num_nu <- colSums(df_nu[, coeff_cols_nu] / df_se_nu^2, na.rm = TRUE)
den_nu <- colSums(1 / df_se_nu^2, na.rm = TRUE)

avg_coeff_nu <- setNames(as.data.frame(t(num_nu / den_nu)), coeff_cols_nu)
avg_se_nu    <- setNames(as.data.frame(t(sqrt(1 / den_nu))), se_cols_nu)

avg_row_nu <- bind_cols(
  data.frame(treatment_id = "Average"),
  avg_coeff_nu, avg_se_nu,
  data.frame(baseNU_AIC = mean(df_nu$baseNU_AIC, na.rm = TRUE),
             NU_AIC     = mean(df_nu$NU_AIC,     na.rm = TRUE),
             nobs_NU    = sum(df_nu$nobs_NU))
)

df_nu_out <- bind_rows(df_nu, avg_row_nu)
write.csv(df_nu_out, "Utility estimation/Output/Data/model_NU_DG.csv", row.names = FALSE)

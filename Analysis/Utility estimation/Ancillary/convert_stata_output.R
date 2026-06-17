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

# --- Norm strength models (NU) ----------------------------------------

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

# --- Random-effects (DerSimonian-Laird) meta-analysis ------------------------
# Produces separate model_RE_*.csv and tau2_*.csv files; existing outputs unchanged.

dl_re <- function(theta, se) {
  keep  <- !is.na(theta) & !is.na(se) & is.finite(se) & se > 0
  theta <- theta[keep];  se <- se[keep];  k <- length(theta)
  if (k < 2) return(c(est = NA_real_, se = NA_real_, tau2 = NA_real_, I2 = NA_real_, k = as.numeric(k)))
  w        <- 1 / se^2
  theta_fe <- sum(w * theta) / sum(w)
  Q        <- sum(w * (theta - theta_fe)^2)
  tau2     <- max(0, (Q - (k - 1)) / (sum(w) - sum(w^2) / sum(w)))
  I2       <- if (Q > 0) max(0, (Q - (k - 1)) / Q * 100) else 0
  w_re     <- 1 / (se^2 + tau2)
  c(est  = sum(w_re * theta) / sum(w_re),
    se   = sqrt(1 / sum(w_re)),
    tau2 = tau2,
    I2   = I2,
    k    = as.numeric(k))
}

re_average <- function(df, coeff_cols, se_cols) {
  res      <- mapply(function(cc, sc) dl_re(df[[cc]], df[[sc]]),
                     coeff_cols, se_cols, SIMPLIFY = FALSE)
  est_vec  <- sapply(res, `[`, "est")
  se_vec   <- sapply(res, `[`, "se")
  tau2_vec <- sapply(res, `[`, "tau2")
  I2_vec   <- sapply(res, `[`, "I2")
  k_vec    <- sapply(res, `[`, "k")
  list(
    coeff = setNames(as.data.frame(t(est_vec)),  coeff_cols),
    se    = setNames(as.data.frame(t(se_vec)),   se_cols),
    tau2  = setNames(as.data.frame(t(tau2_vec)), paste0("tau2_", coeff_cols)),
    I2    = setNames(as.data.frame(t(I2_vec)),   paste0("I2_", coeff_cols)),
    k     = k_vec
  )
}

# Main models RE
re_main     <- re_average(df, coeff_cols, se_cols)
avg_re_row  <- bind_cols(
  data.frame(treatment_id = "Average (RE)"),
  re_main$coeff, re_main$se, avg_aic,
  data.frame(nobs = sum(df$nobs))
)
write.csv(bind_rows(df, avg_re_row),
          "Utility estimation/Output/Data/model_RE_DG.csv",  row.names = FALSE)
write.csv(bind_cols(data.frame(treatment_id = "Average (RE)"), re_main$tau2, re_main$I2),
          "Utility estimation/Output/Data/tau2_DG.csv",      row.names = FALSE)

# NU models RE
re_nu       <- re_average(df_nu, coeff_cols_nu, se_cols_nu)
avg_re_row_nu <- bind_cols(
  data.frame(treatment_id = "Average (RE)"),
  re_nu$coeff, re_nu$se,
  data.frame(baseNU_AIC = mean(df_nu$baseNU_AIC, na.rm = TRUE),
             NU_AIC     = mean(df_nu$NU_AIC,     na.rm = TRUE),
             nobs_NU    = sum(df_nu$nobs_NU))
)
write.csv(bind_rows(df_nu, avg_re_row_nu),
          "Utility estimation/Output/Data/model_RE_NU_DG.csv", row.names = FALSE)
write.csv(bind_cols(data.frame(treatment_id = "Average (RE)"), re_nu$tau2, re_nu$I2),
          "Utility estimation/Output/Data/tau2_NU_DG.csv",     row.names = FALSE)

library(tidyverse)

setwd("~/GitHub/Social_norm_meta_analysis/Analysis/")

df_main <- read.csv("Utility estimation/Output/Data/model_DG.csv")
df_nu   <- read.csv("Utility estimation/Output/Data/model_NU_DG.csv")

avg    <- as.list(df_main[df_main$treatment_id == "Average", ])
avg_nu <- as.list(df_nu[df_nu$treatment_id   == "Average", ])

# Returns list(val, ci): formatted coefficient and 95% CI string
cell <- function(coeff, se, d = 2) {
  if (is.na(coeff) || is.na(se)) return(list(val = "", ci = ""))
  fmt <- function(x) formatC(round(x, d), format = "f", digits = d)
  list(val = fmt(coeff),
       ci  = paste0("[", fmt(coeff - 1.96 * se), ", ", fmt(coeff + 1.96 * se), "]"))
}

empty <- list(val = "", ci = "")

fmt_aic <- function(x, d = 2) {
  if (is.na(x) || length(x) == 0) return("")
  formatC(round(x, d), format = "f", digits = d)
}

# Build a 2-line table row: coefficient line + CI line
row2 <- function(label, bold = FALSE,
                 delta = empty, rho = empty, sigma = empty,
                 gamma = empty, eta = empty, nu = empty, aic = "") {
  lbl  <- if (bold) paste0("\\textbf{", label, "}") else label
  cols <- list(delta, rho, sigma, gamma, eta, nu)
  l1   <- paste(c(lbl,  sapply(cols, `[[`, "val"), aic), collapse = " & ")
  l2   <- paste(c("",   sapply(cols, `[[`, "ci"),  ""),  collapse = " & ")
  paste0(l1, " \\\\\n", l2, " \\\\")
}

# --- Extract cells -------------------------------------------------------

s_delta  <- cell(avg$deltaS,          avg$se_deltaS)

da_rho   <- cell(avg$rhoDA,           avg$se_rhoDA)
da_sig   <- cell(avg$sigmaDA,         avg$se_sigmaDA)

n_delta  <- cell(avg$deltaN,          avg$se_deltaN)
n_gamma  <- cell(avg$gammaN,          avg$se_gammaN)

fu_rho   <- cell(avg$rhoFU,           avg$se_rhoFU)
fu_sig   <- cell(avg$sigmaFU,         avg$se_sigmaFU)
fu_gamma <- cell(avg$gammaFU,         avg$se_gammaFU)

bnu_rho  <- cell(avg_nu$baserhoNU,    avg_nu$se_baserhoNU)
bnu_sig  <- cell(avg_nu$basesigmaNU,  avg_nu$se_basesigmaNU)
bnu_gam  <- cell(avg_nu$basegammaNU,  avg_nu$se_basegammaNU)
bnu_eta  <- cell(avg_nu$baseetaNU,    avg_nu$se_baseetaNU)

nu_rho   <- cell(avg_nu$rhoNU,        avg_nu$se_rhoNU)
nu_sig   <- cell(avg_nu$sigmaNU,      avg_nu$se_sigmaNU)
nu_gam   <- cell(avg_nu$gammaNU,      avg_nu$se_gammaNU)
nu_eta   <- cell(avg_nu$etaNU,        avg_nu$se_etaNU)
nu_nu    <- cell(avg_nu$nuNU,         avg_nu$se_nuNU)

# --- Build LaTeX ---------------------------------------------------------

lines <- c(
  "\\begin{table}[h!]",
  "\\centering",
  "\\resizebox{\\linewidth}{!}{\\begin{tabular}{l|c|cc|ccc|c}",
  "  & & \\multicolumn{2}{c|}{Social preferences} & \\multicolumn{3}{c|}{Social Norms} & \\\\",
  "\\toprule",
  "Model & $\\delta$ & $\\rho$ & $\\sigma$ & $\\gamma$ & $\\eta$ & $\\nu$ & AIC \\\\",
  "\\midrule",
  row2("Selfish (Eq. 1)",
       delta = s_delta,
       aic   = fmt_aic(avg$S_AIC)),
  "\\midrule",
  row2("Outcome Based (Eq. 2)",
       rho   = da_rho,
       sigma = da_sig,
       aic   = fmt_aic(avg$DA_AIC)),
  "\\midrule",
  row2("Social Norm Content (Eq. 3)",
       delta = n_delta,
       gamma = n_gamma,
       aic   = fmt_aic(avg$N_AIC)),
  "\\midrule",
  row2("Combined Model (Eq. 4)",
       rho   = fu_rho,
       sigma = fu_sig,
       gamma = fu_gamma,
       aic   = fmt_aic(avg$FU_AIC)),
  "\\midrule",
  row2("Social Norm Strength (Eq. 5)",
       rho   = bnu_rho,
       sigma = bnu_sig,
       gamma = bnu_gam,
       eta   = bnu_eta,
       aic   = fmt_aic(avg_nu$baseNU_AIC)),
  "\\midrule",
  row2("Full (Eq. 6)",
       rho   = nu_rho,
       sigma = nu_sig,
       gamma = nu_gam,
       eta   = nu_eta,
       nu    = nu_nu,
       aic   = fmt_aic(avg_nu$NU_AIC)),
  "\\bottomrule",
  "\\end{tabular}}",
  "\\caption{Estimated parameters across utility specifications.}",
  "\\label{tab:estimates}",
  "\\end{table}"
)

writeLines(lines, "Utility estimation/Output/Tables/table_estimates.tex")
cat("Written to Output/Tables/table_estimates.tex\n")

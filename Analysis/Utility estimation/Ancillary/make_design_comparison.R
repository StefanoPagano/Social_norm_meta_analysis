library(tidyverse)

setwd("~/GitHub/Social_norm_meta_analysis/Analysis/")

# --- Design lookup (Within/Between belief sample per treatment) -------------
# Design == "Between" iff appropriateness ratings came from a separate sample
# than the one making DG choices; see Utility_comparison.do / File_DB/Individual_between.R.

raw    <- read.csv("Utility estimation/Data/new_data_utility2025-07-23.csv", stringsAsFactors = FALSE)
design <- raw %>% filter(Game_type == "DG") %>% distinct(treatment_id, Design)

# --- IVW pooling helper (same formula as convert_stata_output.R) ------------

ivw_pool <- function(df, coeff_cols, se_cols) {
  df_se <- df[, se_cols]
  df_se[df_se == 0] <- NA
  num <- colSums(df[, coeff_cols] / df_se^2, na.rm = TRUE)
  den <- colSums(1 / df_se^2, na.rm = TRUE)
  list(coeff = setNames(as.data.frame(t(num / den)), coeff_cols),
       se    = setNames(as.data.frame(t(sqrt(1 / den))), se_cols))
}

pool_by_group <- function(df, coeff_cols, se_cols, aic_cols) {
  groups <- c("Within", "Between")
  rows <- lapply(groups, function(g) {
    sub <- df[df$Design == g, ]
    p   <- ivw_pool(sub, coeff_cols, se_cols)
    bind_cols(data.frame(Design = g, k = nrow(sub)), p$coeff, p$se,
              sub %>% summarise(across(all_of(aic_cols), \(x) mean(x, na.rm = TRUE))))
  })
  bind_rows(rows)
}

# --- Main models (S, N, DA, FU) ----------------------------------------------

df <- read.csv("Utility estimation/Output/Data/results_DG.csv", stringsAsFactors = FALSE) %>%
  left_join(design, by = "treatment_id")

coeff_cols <- c("deltaS", "deltaN", "gammaN", "rhoDA", "sigmaDA", "rhoFU", "sigmaFU", "gammaFU")
se_cols    <- paste0("se_", coeff_cols)

constrained <- c("sigmaDA", "rhoFU", "sigmaFU")
df <- df %>% mutate(across(all_of(constrained), ~ ifelse(. == 0, NA, .)))
for (col in constrained) df[[paste0("se_", col)]][is.na(df[[col]])] <- NA

main_by_design <- pool_by_group(df, coeff_cols, se_cols, c("N_AIC", "DA_AIC", "FU_AIC"))
write.csv(main_by_design, "Utility estimation/Output/Data/design_comparison_DG.csv", row.names = FALSE)

# --- Norm strength models (NU) ------------------------------------------------

df_nu <- read.csv("Utility estimation/Output/Data/results_NU_DG.csv", stringsAsFactors = FALSE) %>%
  left_join(design, by = "treatment_id")

coeff_cols_nu <- c("basegammaNU", "baseetaNU", "gammaNU", "etaNU", "nuNU")
se_cols_nu    <- paste0("se_", coeff_cols_nu)

nu_by_design <- pool_by_group(df_nu, coeff_cols_nu, se_cols_nu, c("baseNU_AIC", "NU_AIC"))
write.csv(nu_by_design, "Utility estimation/Output/Data/design_comparison_NU_DG.csv", row.names = FALSE)

# --- LaTeX table ---------------------------------------------------------

cell <- function(coeff, se, d = 2) {
  if (is.na(coeff) || is.na(se)) return(list(val = "", ci = ""))
  fmt <- function(x) formatC(round(x, d), format = "f", digits = d)
  list(val = fmt(coeff), ci = paste0("[", fmt(coeff - 1.96 * se), ", ", fmt(coeff + 1.96 * se), "]"))
}
empty   <- list(val = "", ci = "")
fmt_aic <- function(x, d = 2) if (is.na(x) || length(x) == 0) "" else formatC(round(x, d), format = "f", digits = d)

row2 <- function(label, design_label, k, bold = FALSE,
                  delta = empty, rho = empty, sigma = empty,
                  gamma = empty, eta = empty, nu = empty, aic = "") {
  lbl  <- if (bold) paste0("\\textbf{", label, "}") else label
  cols <- list(delta, rho, sigma, gamma, eta, nu)
  l1   <- paste(c(lbl, sprintf("%s ($k=%d$)", design_label, k), sapply(cols, `[[`, "val"), aic), collapse = " & ")
  l2   <- paste(c("", "", sapply(cols, `[[`, "ci"), ""), collapse = " & ")
  paste0(l1, " \\\\\n", l2, " \\\\")
}

get_row <- function(tbl, design_label) as.list(tbl[tbl$Design == design_label, ])

n_w  <- get_row(main_by_design, "Within");  n_b  <- get_row(main_by_design, "Between")
nu_w <- get_row(nu_by_design,   "Within");  nu_b <- get_row(nu_by_design,   "Between")

lines <- c(
  "\\begin{table}[h!]",
  "\\centering",
  "\\resizebox{\\linewidth}{!}{\\begin{tabular}{ll|c|cc|ccc|c}",
  "  & & & \\multicolumn{2}{c|}{Social preferences} & \\multicolumn{3}{c|}{Social Norms} & \\\\",
  "\\toprule",
  "Model & Belief sample & $\\delta$ & $\\rho$ & $\\sigma$ & $\\gamma$ & $\\eta$ & $\\nu$ & AIC \\\\",
  "\\midrule",
  row2("Social Norm Content (Eq. 3)", "Within", n_w$k, delta = cell(n_w$deltaN, n_w$se_deltaN), gamma = cell(n_w$gammaN, n_w$se_gammaN), aic = fmt_aic(n_w$N_AIC)),
  "\\midrule",
  row2("Social Norm Content (Eq. 3)", "Between", n_b$k, delta = cell(n_b$deltaN, n_b$se_deltaN), gamma = cell(n_b$gammaN, n_b$se_gammaN), aic = fmt_aic(n_b$N_AIC)),
  "\\midrule",
  row2("Combined Model (Eq. 4)", "Within", n_w$k, rho = cell(n_w$rhoFU, n_w$se_rhoFU), sigma = cell(n_w$sigmaFU, n_w$se_sigmaFU), gamma = cell(n_w$gammaFU, n_w$se_gammaFU), aic = fmt_aic(n_w$FU_AIC)),
  "\\midrule",
  row2("Combined Model (Eq. 4)", "Between", n_b$k, rho = cell(n_b$rhoFU, n_b$se_rhoFU), sigma = cell(n_b$sigmaFU, n_b$se_sigmaFU), gamma = cell(n_b$gammaFU, n_b$se_gammaFU), aic = fmt_aic(n_b$FU_AIC)),
  "\\midrule",
  row2("Social Norm Strength (Eq. 5)", "Within", nu_w$k, gamma = cell(nu_w$basegammaNU, nu_w$se_basegammaNU), eta = cell(nu_w$baseetaNU, nu_w$se_baseetaNU), aic = fmt_aic(nu_w$baseNU_AIC)),
  "\\midrule",
  row2("Social Norm Strength (Eq. 5)", "Between", nu_b$k, gamma = cell(nu_b$basegammaNU, nu_b$se_basegammaNU), eta = cell(nu_b$baseetaNU, nu_b$se_baseetaNU), aic = fmt_aic(nu_b$baseNU_AIC)),
  "\\midrule",
  row2("Full (Eq. 6)", "Within", nu_w$k, bold = TRUE, gamma = cell(nu_w$gammaNU, nu_w$se_gammaNU), eta = cell(nu_w$etaNU, nu_w$se_etaNU), nu = cell(nu_w$nuNU, nu_w$se_nuNU), aic = fmt_aic(nu_w$NU_AIC)),
  "\\midrule",
  row2("Full (Eq. 6)", "Between", nu_b$k, bold = TRUE, gamma = cell(nu_b$gammaNU, nu_b$se_gammaNU), eta = cell(nu_b$etaNU, nu_b$se_etaNU), nu = cell(nu_b$nuNU, nu_b$se_nuNU), aic = fmt_aic(nu_b$NU_AIC)),
  "\\bottomrule",
  "\\end{tabular}}",
  "\\caption{Pooled (IVW) coefficient estimates split by whether the appropriateness ratings used to construct $N_k$/$S_k$ came from the same sample as the dictator-game choices (Within, $k=5$ treatments) or an independent sample (Between, $k=9$ treatments). Only models involving $N_k$/$S_k$ are shown; Eq.~1--2 do not depend on these measures.}",
  "\\label{tab:design_comparison}",
  "\\end{table}"
)

writeLines(lines, "Utility estimation/Output/Tables/table_design_comparison.tex")
cat("Written design_comparison_DG.csv, design_comparison_NU_DG.csv, table_design_comparison.tex\n")

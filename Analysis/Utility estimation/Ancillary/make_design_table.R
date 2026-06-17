library(tidyverse)

setwd("~/GitHub/Social_norm_meta_analysis/Analysis/")

# Same display names/order as plot_coefficients.R
label_map <- c(
  "2007Lis165_1a" = "List (2007)",
  "2012Laz164_3"  = "Lazear et al. (2012)",
  "2013Kru001_1a" = "Krupka & Weber (2013)",
  "2016Kim003_7"  = "Kimbrough & V. (2016)",
  "2017Del037_1"  = "Della Valle et al. (2017)",
  "2017Fro073_1"  = "Fromell et al. (2019)",
  "2017Gac013_2"  = "Gachter et al. (2017) - Give",
  "2017Gac013_4"  = "Gachter et al. (2017) - Take",
  "2017Tho028_1"  = "Thomsson & V. (2017)",
  "2018Her061_9"  = "Erkut (2022)",
  "2019Cha026_1"  = "Chang et al. (2019)",
  "2020Bas115_2a" = "Basic et al. (2021)",
  "2023Eck169_1a" = "Eckel & Grossman (2023) - Class",
  "2023Eck169_1b" = "Eckel & Grossman (2023) - Fresh."
)

label_order <- c(
  "List (2007)", "Lazear et al. (2012)", "Krupka & Weber (2013)",
  "Kimbrough & V. (2016)", "Della Valle et al. (2017)",
  "Gachter et al. (2017) - Give", "Gachter et al. (2017) - Take",
  "Thomsson & V. (2017)", "Chang et al. (2019)", "Fromell et al. (2019)",
  "Basic et al. (2021)", "Erkut (2022)",
  "Eckel & Grossman (2023) - Class", "Eckel & Grossman (2023) - Fresh."
)

# --- Replicate Utility_comparison.do data prep (lines 12-15, 84-88) ----------

raw <- read.csv("Utility estimation/Data/new_data_utility2025-07-23.csv", stringsAsFactors = FALSE)
raw <- raw[raw$Game_type == "DG", ]

sd_max  <- max(raw$sd_app, na.rm = TRUE)
raw$S_k <- 1 - 2 * (raw$sd_app / sd_max)

design_tbl <- raw %>%
  group_by(treatment_id) %>%
  summarise(
    Design   = first(Design),
    mean_app = mean(mean_app, na.rm = TRUE),
    S_k      = mean(S_k, na.rm = TRUE),
    .groups  = "drop"
  )

nobs_tbl <- read.csv("Utility estimation/Output/Data/results_DG.csv", stringsAsFactors = FALSE) %>%
  select(treatment_id, nobs)

out <- design_tbl %>%
  inner_join(nobs_tbl, by = "treatment_id") %>%
  mutate(study = unname(label_map[treatment_id])) %>%
  mutate(study = factor(study, levels = label_order)) %>%
  arrange(study) %>%
  select(study, Design, nobs, mean_app, S_k)

write.csv(out, "Utility estimation/Output/Data/design_summary_DG.csv", row.names = FALSE)

# --- LaTeX table --------------------------------------------------------------

fmt <- function(x, d = 2) formatC(round(x, d), format = "f", digits = d)

esc_amp <- function(x) gsub("&", "\\\\&", x)

body <- out %>%
  rowwise() %>%
  mutate(line = paste(esc_amp(as.character(study)), Design, nobs, fmt(mean_app), fmt(S_k), sep = " & ")) %>%
  pull(line) %>%
  paste(" \\\\")

lines <- c(
  "\\begin{table}[h!]",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "Study & Belief sample & $N$ (dictators) & Mean appropriateness & Norm strength ($S_k$) \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\caption{Design heterogeneity across studies. \\emph{Belief sample} indicates whether the appropriateness ratings used to construct $N_k$ and $S_k$ were collected from the same individuals who made the dictator-game decisions (Within) or from an independent sample (Between). $N$ is the number of dictators in the clogit estimation sample for that treatment. Mean appropriateness and norm strength are treatment-level averages across actions.}",
  "\\label{tab:design}",
  "\\end{table}"
)

writeLines(lines, "Utility estimation/Output/Tables/table_design.tex")
cat("Written to Output/Data/design_summary_DG.csv and Output/Tables/table_design.tex\n")

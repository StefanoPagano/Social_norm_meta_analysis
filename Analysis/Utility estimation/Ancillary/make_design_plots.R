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

# One row per distinct action (treatment x scenario), not per subject/choice
by_action <- raw %>%
  distinct(treatment_id, scenarios, Design, mean_app, S_k) %>%
  mutate(study = factor(unname(label_map[treatment_id]), levels = rev(label_order))) %>%
  filter(!is.na(study))

design_colors <- c("Within" = "#2C3E50", "Between" = "#E74C3C")

box_plot <- function(data, yvar, ylab) {
  ggplot(data, aes(x = study, y = !!sym(yvar), fill = Design)) +
    geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.8) +
    geom_jitter(width = 0.12, height = 0, size = 0.8, alpha = 0.5, color = "gray20") +
    scale_fill_manual(values = design_colors, name = "Belief sample") +
    labs(x = NULL, y = ylab) +
    coord_flip() +
    theme_classic(base_size = 10) +
    theme(legend.position = "bottom", legend.key.size = unit(0.4, "cm"))
}

p_app <- box_plot(by_action, "mean_app", "Mean appropriateness")
p_str <- box_plot(by_action, "S_k", expression(Norm~strength~(S[k])))

ggsave("Utility estimation/Output/Figures/design_appropriateness.pdf", p_app, width = 1100, height = 700, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/design_strength.pdf",        p_str, width = 1100, height = 700, units = "px", dpi = 120)

cat("Written Output/Figures/design_appropriateness.pdf and design_strength.pdf\n")

library(ggplot2)
library(tidyverse)
library(ggpubr)

setwd("~/GitHub/Social_norm_meta_analysis/Analysis/")

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
  "2023Eck169_1b" = "Eckel & Grossman (2023) - Fresh.",
  "Average"       = "Average",
  "Average (RE)"  = "Average"
)

label_order <- c(
  "List (2007)",
  "Lazear et al. (2012)",
  "Krupka & Weber (2013)",
  "Kimbrough & V. (2016)",
  "Della Valle et al. (2017)",
  "Gachter et al. (2017) - Give",
  "Gachter et al. (2017) - Take",
  "Thomsson & V. (2017)",
  "Chang et al. (2019)",
  "Fromell et al. (2019)",
  "Basic et al. (2021)",
  "Erkut (2022)",
  "Eckel & Grossman (2023) - Class",
  "Eckel & Grossman (2023) - Fresh.",
  "Average"
)

apply_labels <- function(df) {
  df %>% mutate(treatment_id = factor(unname(label_map[treatment_id]), levels = label_order))
}

# Single-coefficient forest plot.
# Hollow circle = n.s., filled circle = p<0.05; diamond shape for Average row.
cp <- function(data, coeff, se, xlabel, show_y = TRUE, show_legend = FALSE) {
  d <- data %>%
    mutate(
      sig = factor(
        !is.na(!!sym(coeff)) & !is.na(!!sym(se)) & is.finite(!!sym(se)) &
          ((!!sym(coeff) - 1.96 * !!sym(se) > 0) | (!!sym(coeff) + 1.96 * !!sym(se) < 0)),
        levels = c(FALSE, TRUE), labels = c("n.s.", "p < 0.05")
      ),
      avg = (treatment_id == "Average")
    )

  ggplot(d, aes(x = !!sym(coeff), y = treatment_id)) +
    geom_vline(xintercept = 0, color = "gray55", linewidth = 0.35, linetype = "dashed") +
    geom_pointrange(
      data = ~filter(., !avg),
      aes(xmin = !!sym(coeff) - 1.96 * !!sym(se),
          xmax = !!sym(coeff) + 1.96 * !!sym(se),
          fill = sig),
      shape = 21, color = "gray30", size = 0.25, fatten = 2.5, stroke = 0.5
    ) +
    geom_pointrange(
      data = ~filter(., avg),
      aes(xmin = !!sym(coeff) - 1.96 * !!sym(se),
          xmax = !!sym(coeff) + 1.96 * !!sym(se),
          fill = sig),
      shape = 23, color = "gray10", size = 0.5, fatten = 4, stroke = 0.7
    ) +
    scale_fill_manual(
      values = c("n.s." = "white", "p < 0.05" = "#2C3E50"),
      name = NULL, drop = FALSE,
      guide = if (show_legend) "legend" else "none"
    ) +
    labs(x = xlabel, y = NULL) +
    theme_classic(base_size = 10) +
    theme(
      axis.text.y        = if (show_y) element_text(size = 8) else element_blank(),
      axis.ticks.y       = if (show_y) element_line() else element_blank(),
      panel.grid.major.x = element_line(color = "gray93", linewidth = 0.3),
      legend.position    = if (show_legend) "bottom" else "none",
      legend.key.size    = unit(0.4, "cm")
    )
}

## ---- Plot generator functions -----------------------------------------------

plot_coeff_social_generator <- function(data) {
  plot_s <- cp(data, "deltaS", "se_deltaS", expression(delta), show_y = TRUE)
  plot_s <- annotate_figure(plot_s, top = text_grob("Selfish (Eq. 1)", face = "bold", size = 14))

  plot_da <- ggarrange(
    cp(data, "rhoDA",   "se_rhoDA",   expression(rho),   show_y = TRUE),
    cp(data, "sigmaDA", "se_sigmaDA", expression(sigma), show_y = FALSE),
    widths = c(8, 6)
  )
  plot_da <- annotate_figure(plot_da, top = text_grob("Outcome Based (Eq. 2)", face = "bold", size = 14))

  plot_sn <- ggarrange(
    cp(data, "deltaN", "se_deltaN", expression(delta), show_y = TRUE),
    cp(data, "gammaN", "se_gammaN", expression(gamma), show_y = FALSE),
    widths = c(8, 6)
  )
  plot_sn <- annotate_figure(plot_sn, top = text_grob("Social Norm Content (Eq. 3)", face = "bold", size = 14))

  plot_fu <- ggarrange(
    cp(data, "rhoFU",   "se_rhoFU",   expression(rho),   show_y = TRUE),
    cp(data, "sigmaFU", "se_sigmaFU", expression(sigma), show_y = FALSE),
    cp(data, "gammaFU", "se_gammaFU", expression(gamma), show_y = FALSE),
    ncol = 3, nrow = 1, widths = c(13, 6, 6)
  )
  plot_fu <- annotate_figure(plot_fu, top = text_grob("Combined Model (Eq. 4)", face = "bold", size = 14))

  return(list(plot_s, plot_sn, plot_da, plot_fu))
}

plot_coeff_social_generator_uncertainty <- function(data) {
  plot_nu <- ggarrange(
    cp(data, "basegammaNU", "se_basegammaNU", expression(gamma), show_y = TRUE),
    cp(data, "baseetaNU",   "se_baseetaNU",   expression(eta),   show_y = FALSE),
    widths = c(8, 6)
  )

  plot_nu_i <- ggarrange(
    cp(data, "gammaNU", "se_gammaNU", expression(gamma), show_y = TRUE),
    cp(data, "etaNU",   "se_etaNU",   expression(eta),   show_y = FALSE),
    cp(data, "nuNU",    "se_nuNU",    expression(nu),    show_y = FALSE),
    ncol = 3, nrow = 1, widths = c(13, 6, 6)
  )

  plot_nu_i_social <- ggarrange(
    cp(data, "rhoNU",   "se_rhoNU",   expression(rho),   show_y = TRUE),
    cp(data, "sigmaNU", "se_sigmaNU", expression(sigma), show_y = FALSE),
    widths = c(8, 6)
  )

  return(list(plot_nu, plot_nu_i, plot_nu_i_social))
}

## ---- Main models ------------------------------------------------------------

stata_output_model <- read.csv("Utility estimation/Output/Data/model_DG.csv") %>% apply_labels()
plots <- plot_coeff_social_generator(stata_output_model)

ggsave("Utility estimation/Output/Figures/model_selfish.pdf",           plots[[1]], width = 500, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/model_social_norm.pdf",       plots[[2]], width = 800, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/model_social_preferences.pdf",plots[[3]], width = 800, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/model_full.pdf",              plots[[4]], width = 1100, height = 400, units = "px", dpi = 120)

## ---- Norm uncertainty models ------------------------------------------------

stata_output_model_u <- read.csv("Utility estimation/Output/Data/model_NU_DG.csv") %>% apply_labels()
plots_u <- plot_coeff_social_generator_uncertainty(stata_output_model_u)

ggsave("Utility estimation/Output/Figures/unc_model_social_norm.pdf",                         plots_u[[1]], width = 800, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/unc_model_social_norm_interaction.pdf",             plots_u[[2]], width = 1100, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/unc_model_social_norm_interaction_social_pref.pdf", plots_u[[3]], width = 800, height = 400, units = "px", dpi = 120)

plot_nu_labeled   <- annotate_figure(plots_u[[1]], top = text_grob("Social Norm Strength (Eq. 5)", face = "bold", size = 14))
plot_nu_i_labeled <- annotate_figure(plots_u[[2]], top = text_grob("Full (Eq. 6)",                 face = "bold", size = 14))

shared_legend  <- get_legend(cp(stata_output_model_u, "basegammaNU", "se_basegammaNU",
                                expression(gamma), show_y = FALSE, show_legend = TRUE))
plot_nu_combined <- ggarrange(
  ggarrange(plot_nu_labeled, plot_nu_i_labeled, nrow = 2),
  as_ggplot(shared_legend),
  nrow = 2, heights = c(20, 1)
)
ggsave("Utility estimation/Output/Figures/unc_model_combined.pdf", plot_nu_combined, width = 1100, height = 820, units = "px", dpi = 120)

## ---- Heterogeneity (I²) chart -----------------------------------------------

tau2_main <- read.csv("Utility estimation/Output/Data/tau2_DG.csv")
tau2_nu   <- read.csv("Utility estimation/Output/Data/tau2_NU_DG.csv")

het_tiers <- c("Low (<25%)" = "#27AE60", "Moderate (25–50%)" = "#F39C12",
               "Substantial (50–75%)" = "#E74C3C", "Considerable (>75%)" = "#8E44AD")

make_I2_df <- function(tau2_row, param_levels, param_labels) {
  tau2_row %>%
    select(starts_with("I2_")) %>%
    pivot_longer(everything(), names_to = "param", values_to = "I2") %>%
    mutate(
      param = sub("^I2_", "", param),
      label = factor(param, levels = rev(param_levels), labels = rev(param_labels)),
      tier  = cut(I2, c(-Inf, 25, 50, 75, Inf),
                  labels = names(het_tiers), right = FALSE)
    ) %>%
    filter(!is.na(I2))
}

plot_I2 <- function(df) {
  ggplot(df, aes(x = I2, y = label, fill = tier)) +
    geom_col(width = 0.55) +
    geom_vline(xintercept = c(25, 50, 75), linetype = "dashed",
               color = "gray55", linewidth = 0.35) +
    scale_fill_manual(values = het_tiers, name = NULL, drop = FALSE) +
    scale_x_continuous(limits = c(0, 100), labels = \(x) paste0(x, "%")) +
    labs(x = expression(I^2), y = NULL) +
    theme_classic(base_size = 10) +
    theme(legend.position = "bottom", legend.key.size = unit(0.35, "cm"))
}

main_levels <- c("deltaS", "deltaN", "gammaN", "rhoDA", "sigmaDA", "rhoFU", "sigmaFU", "gammaFU")
main_labels <- c("δ (Eq.1)", "δ (Eq.3)", "γ (Eq.3)",
                 "ρ (Eq.2)", "σ (Eq.2)",
                 "ρ (Eq.4)", "σ (Eq.4)", "γ (Eq.4)")

nu_levels <- c("basegammaNU", "baseetaNU", "baserhoNU", "basesigmaNU",
               "gammaNU", "etaNU", "nuNU", "rhoNU", "sigmaNU")
nu_labels <- c("γ (Eq.5)", "η (Eq.5)", "ρ (Eq.5)", "σ (Eq.5)",
               "γ (Eq.6)", "η (Eq.6)", "ν (Eq.6)", "ρ (Eq.6)", "σ (Eq.6)")

I2_main_df <- make_I2_df(tau2_main, main_levels, main_labels)
I2_nu_df   <- make_I2_df(tau2_nu,   nu_levels,   nu_labels)

p_het_main <- annotate_figure(plot_I2(I2_main_df),
                               top = text_grob("Main models", face = "bold", size = 12))
p_het_nu   <- annotate_figure(plot_I2(I2_nu_df),
                               top = text_grob("Norm uncertainty models", face = "bold", size = 12))

p_het_combined <- ggarrange(p_het_main, p_het_nu, ncol = 2,
                             common.legend = TRUE, legend = "bottom")

ggsave("Utility estimation/Output/Figures/heterogeneity_I2.pdf",
       p_het_combined, width = 900, height = 400, units = "px", dpi = 120)

## ---- Main models (RE) -------------------------------------------------------

stata_output_model_re <- read.csv("Utility estimation/Output/Data/model_RE_DG.csv") %>% apply_labels()
plots_re <- plot_coeff_social_generator(stata_output_model_re)

ggsave("Utility estimation/Output/Figures/model_selfish_RE.pdf",            plots_re[[1]], width = 500,  height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/model_social_norm_RE.pdf",        plots_re[[2]], width = 800,  height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/model_social_preferences_RE.pdf", plots_re[[3]], width = 800,  height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/model_full_RE.pdf",               plots_re[[4]], width = 1100, height = 400, units = "px", dpi = 120)

## ---- Norm uncertainty models (RE) -------------------------------------------

stata_output_model_u_re <- read.csv("Utility estimation/Output/Data/model_RE_NU_DG.csv") %>% apply_labels()
plots_u_re <- plot_coeff_social_generator_uncertainty(stata_output_model_u_re)

ggsave("Utility estimation/Output/Figures/unc_model_social_norm_RE.pdf",                         plots_u_re[[1]], width = 800,  height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/unc_model_social_norm_interaction_RE.pdf",             plots_u_re[[2]], width = 1100, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/unc_model_social_norm_interaction_social_pref_RE.pdf", plots_u_re[[3]], width = 800,  height = 400, units = "px", dpi = 120)

plot_nu_labeled_re   <- annotate_figure(plots_u_re[[1]], top = text_grob("Social Norm Strength (Eq. 5)", face = "bold", size = 14))
plot_nu_i_labeled_re <- annotate_figure(plots_u_re[[2]], top = text_grob("Full (Eq. 6)",                 face = "bold", size = 14))

shared_legend_re <- get_legend(cp(stata_output_model_u_re, "basegammaNU", "se_basegammaNU",
                                  expression(gamma), show_y = FALSE, show_legend = TRUE))
plot_nu_combined_re <- ggarrange(
  ggarrange(plot_nu_labeled_re, plot_nu_i_labeled_re, nrow = 2),
  as_ggplot(shared_legend_re),
  nrow = 2, heights = c(20, 1)
)
ggsave("Utility estimation/Output/Figures/unc_model_combined_RE.pdf",
       plot_nu_combined_re, width = 1100, height = 820, units = "px", dpi = 120)

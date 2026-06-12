rm(list=ls())
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggbreak)
library(patchwork)

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
  "Average"       = "Average"
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
cp <- function(data, coeff, se, xlabel, show_y = TRUE) {
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
      name = NULL, drop = FALSE
    ) +
    labs(x = xlabel, y = NULL) +
    theme_classic(base_size = 10) +
    theme(
      axis.text.y        = if (show_y) element_text(size = 8) else element_blank(),
      axis.ticks.y       = if (show_y) element_line() else element_blank(),
      panel.grid.major.x = element_line(color = "gray93", linewidth = 0.3),
      legend.position    = "bottom",
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
    common.legend = TRUE, legend = "bottom", widths = c(8, 6)
  )
  plot_da <- annotate_figure(plot_da, top = text_grob("Outcome Based (Eq. 2)", face = "bold", size = 14))

  plot_sn <- ggarrange(
    cp(data, "deltaN", "se_deltaN", expression(delta), show_y = TRUE),
    cp(data, "gammaN", "se_gammaN", expression(gamma), show_y = FALSE),
    common.legend = TRUE, legend = "bottom", widths = c(8, 6)
  )
  plot_sn <- annotate_figure(plot_sn, top = text_grob("Social Norm Content (Eq. 3)", face = "bold", size = 14))

  plot_fu <- ggarrange(
    cp(data, "rhoFU",   "se_rhoFU",   expression(rho),   show_y = TRUE),
    cp(data, "sigmaFU", "se_sigmaFU", expression(sigma), show_y = FALSE),
    cp(data, "gammaFU", "se_gammaFU", expression(gamma), show_y = FALSE),
    common.legend = TRUE, legend = "bottom", widths = c(9, 6, 6)
  )
  plot_fu <- annotate_figure(plot_fu, top = text_grob("Combined Model (Eq. 4)", face = "bold", size = 14))

  return(list(plot_s, plot_sn, plot_da, plot_fu))
}

plot_coeff_social_generator_uncertainty <- function(data) {
  plot_nu <- ggarrange(
    cp(data, "basegammaNU", "se_basegammaNU", expression(gamma), show_y = TRUE),
    cp(data, "baseetaNU",   "se_baseetaNU",   expression(eta),   show_y = FALSE),
    common.legend = TRUE, legend = "bottom", widths = c(8, 6)
  )

  plot_nu_i <- ggarrange(
    cp(data, "gammaNU", "se_gammaNU", expression(gamma), show_y = TRUE),
    cp(data, "etaNU",   "se_etaNU",   expression(eta),   show_y = FALSE),
    cp(data, "nuNU",    "se_nuNU",    expression(nu),    show_y = FALSE),
    common.legend = TRUE, legend = "bottom", widths = c(8, 6, 6)
  )

  plot_nu_i_social <- ggarrange(
    cp(data, "rhoNU",   "se_rhoNU",   expression(rho),   show_y = TRUE),
    cp(data, "sigmaNU", "se_sigmaNU", expression(sigma), show_y = FALSE),
    common.legend = TRUE, legend = "bottom", widths = c(8, 6)
  )

  return(list(plot_nu, plot_nu_i, plot_nu_i_social))
}

## ---- Main models ------------------------------------------------------------

stata_output_model <- read.csv("Utility estimation/Output/Data/model_DG.csv") %>% apply_labels()
plots <- plot_coeff_social_generator(stata_output_model)

ggsave("Utility estimation/Output/Figures/model_selfish.pdf",           plots[[1]], width = 500, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/model_social_norm.pdf",       plots[[2]], width = 800, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/model_social_preferences.pdf",plots[[3]], width = 800, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/model_full.pdf",              plots[[4]], width = 800, height = 400, units = "px", dpi = 120)

## ---- Norm uncertainty models ------------------------------------------------

stata_output_model_u <- read.csv("Utility estimation/Output/Data/model_NU_DG.csv") %>% apply_labels()
plots_u <- plot_coeff_social_generator_uncertainty(stata_output_model_u)

ggsave("Utility estimation/Output/Figures/unc_model_social_norm.pdf",                         plots_u[[1]], width = 800, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/unc_model_social_norm_interaction.pdf",             plots_u[[2]], width = 800, height = 400, units = "px", dpi = 120)
ggsave("Utility estimation/Output/Figures/unc_model_social_norm_interaction_social_pref.pdf", plots_u[[3]], width = 800, height = 400, units = "px", dpi = 120)

plot_nu_labeled   <- annotate_figure(plots_u[[1]], top = text_grob("Social Norm Strength (Eq. 5)", face = "bold", size = 14))
plot_nu_i_labeled <- annotate_figure(plots_u[[2]], top = text_grob("Full (Eq. 6)",                 face = "bold", size = 14))
plot_nu_combined  <- ggarrange(plot_nu_labeled, plot_nu_i_labeled, nrow = 2)
ggsave("Utility estimation/Output/Figures/unc_model_combined.pdf", plot_nu_combined, width = 900, height = 800, units = "px", dpi = 120)

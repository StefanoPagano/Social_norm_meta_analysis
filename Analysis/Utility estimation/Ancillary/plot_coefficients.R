rm(list=ls())
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggbreak)
library(patchwork)

setwd("~/GitHub/Social_norm_meta_analysis/Analysis/")
## Define functions to plot ------------
model_df_generator <- function(file, uncertainty_model) {
  df <- read.csv(file)
#  
#  df <- df %>%
#    mutate(weight_SE=(n.x)^2) 
#  if (uncertainty_model==0) {
#    
#    average_coeff_stata <- df %>% summarise(across(deltaS:gammaFU, ~weighted.mean(., w = n.x, na.rm=T)))
#    average_SE_stata <- df %>% summarise(across(se_deltaS:se_gammaFU, ~weighted.mean(.^2, w = weight_SE, na.rm=T)))
#
#  } else {
#    average_coeff_stata <- df %>% summarise(across(basegammaNU:sucaNU, ~weighted.mean(., w = n.x, na.rm=T)))
#    average_SE_stata <- df %>% summarise(across(se_basegammaNU:se_sucaNU, ~weighted.mean(.^2, w = weight_SE, na.rm=T)))
#    
#  }
#  df <- df %>%
#    add_row(treatment_id="Average", average_coeff_stata , sqrt(average_SE_stata), n.x=sum(df$n.x), weight_SE=sum(df$weight_SE))
#  
  invisible(df)
}

plot_coeff_social_generator <- function(data) {
  s=0.2
  # selfish model
  
  plot_s_delta <- ggplot(data, aes(x=deltaS,y=treatment_id)) + 
    geom_pointrange(aes(xmin=deltaS-1.96*se_deltaS,xmax=deltaS+1.96*se_deltaS), shape=20, size = s) +
    ylab("Treatment ID") +
    xlab("S model - Coefficient of payoff (delta)") +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)
  
  # Social norm model (SN)
  
  plot_sn_delta <- ggplot(data, aes(x=deltaN,y=treatment_id)) + 
    geom_pointrange(aes(xmin=deltaN-1.96*se_deltaN,xmax=deltaN+1.96*se_deltaN), shape=20, size=s) +
    ylab("Treatment ID") +
    xlab(expression(delta)) +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)
  
  plot_sn_gamma <- ggplot(data, aes(x=gammaN,y=treatment_id)) + 
    geom_pointrange(aes(xmin=gammaN-1.96*se_gammaN,xmax=gammaN+1.96*se_gammaN), shape=20, size=s) +
    xlab(expression(gamma)) +
    theme_light() +
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)
  
  # Social preferences models
  
  plot_da_rho <- ggplot(data, aes(x=rhoDA,y=treatment_id)) + 
    geom_pointrange(aes(xmin=rhoDA-1.96*se_rhoDA,xmax=rhoDA+1.96*se_rhoDA), shape=20, size=s) +
    ylab("Treatment ID") +
    xlab(expression(rho)) +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)
  
  plot_da_sigma <- ggplot(data, aes(x=sigmaDA,y=treatment_id)) + 
    geom_pointrange(aes(xmin=sigmaDA-1.96*se_sigmaDA,xmax=sigmaDA+1.96*se_sigmaDA), shape=20, size=s) +
    xlab(expression(sigma)) +
    theme_light() +
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0) #xlim(c(-1,1))
  
  # Full model (social preferences and social norms)
  
  plot_fu_rho <- ggplot(data, aes(x=rhoFU,y=treatment_id)) + 
    geom_pointrange(aes(xmin=rhoFU-1.96*se_rhoFU,xmax=rhoFU+1.96*se_rhoFU), shape=20, size=s) +
    ylab("Treatment ID") +
    xlab(expression(rho)) +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)
  
  plot_fu_sigma <- ggplot(data, aes(x=sigmaFU,y=treatment_id)) + 
    geom_pointrange(aes(xmin=sigmaFU-1.96*se_sigmaFU,xmax=sigmaFU+1.96*se_sigmaFU), shape=20, size=s) +
    xlab(expression(sigma)) +
    theme_light() +
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)
  
  plot_fu_gamma <- ggplot(data, aes(x=gammaFU,y=treatment_id)) + 
    geom_pointrange(aes(xmin=gammaFU-1.96*se_gammaFU,xmax=gammaFU+1.96*se_gammaFU), shape=20, size=s) +
    xlab(expression(gamma)) +
    theme_light() +
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)
  
  plot_da <- ggarrange(plot_da_rho, plot_da_sigma, common.legend=T, legend="bottom", widths = c(8,6))
  plot_sn <- ggarrange(plot_sn_delta, plot_sn_gamma, common.legend=T, legend="bottom", widths = c(8,6))
  plot_sn <- annotate_figure(plot_sn, top = text_grob("Social Norm model", face = "bold", size = 14))
  plot_da <- annotate_figure(plot_da, top = text_grob("Social Preferences model", face = "bold", size = 14))
  plot_fu <- ggarrange(plot_fu_rho, plot_fu_sigma, plot_fu_gamma, nrow = 1, common.legend=T, legend="bottom", widths = c(9,6,6))
  plot_fu <- annotate_figure(plot_fu, top = text_grob("Full model", face = "bold", size = 14))
  #final_plot <- ggarrange(plot_sn, plot_da, plot_fu, ncol = 1)

  
  return(list(plot_sn, plot_da, plot_fu))
}

plot_coeff_social_generator_uncertainty <- function(data){
  s=0.2
  # base model
  plot_gamma <-  ggplot(data, aes(x=basegammaNU,y=treatment_id)) + 
    geom_pointrange(aes(xmin=basegammaNU-1.96*se_basegammaNU,xmax=basegammaNU+1.96*se_basegammaNU), shape=20, size=s) +
    xlab(expression(gamma)) +
    theme_light() +
    ylab("Treatment ID") +
    theme(#axis.text.y=element_blank(),
          #axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)
  
  plot_eta <-  ggplot(data, aes(x=baseetaNU,y=treatment_id)) + 
    geom_pointrange(aes(xmin=baseetaNU-1.96*se_baseetaNU,xmax=baseetaNU+1.96*se_baseetaNU), shape=20, size=s) +
    xlab(expression(eta)) +
    theme_light() +
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)

  # model with interaction term
    plot_gamma_i <-  ggplot(data, aes(x=gammaNU,y=treatment_id)) + 
    geom_pointrange(aes(xmin=gammaNU-1.96*se_gammaNU,xmax=gammaNU+1.96*se_gammaNU), shape=20, size=s) +
    xlab(expression(gamma)) +
    theme_light() +
    ylab("Treatment ID") +
    theme(#axis.text.y=element_blank(),
          #axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)

  plot_eta_i <-  ggplot(data, aes(x=etaNU,y=treatment_id)) + 
    geom_pointrange(aes(xmin=etaNU-1.96*se_etaNU,xmax=etaNU+1.96*se_etaNU), shape=20, size=s) +
    xlab(expression(eta)) +
    theme_light() +
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)

  plot_interaction <-  ggplot(data, aes(x=sucaNU,y=treatment_id)) + 
    geom_pointrange(aes(xmin=sucaNU-1.96*se_sucaNU,xmax=sucaNU+1.96*se_sucaNU), shape=20, size=s) +
    xlab("Interaction term") +
    theme_light() +
    theme(axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 0)
  
  plot_nu <- ggarrange(plot_gamma, plot_eta, common.legend=T, legend="bottom", widths = c(8,6))
  plot_nu_i <- ggarrange(plot_gamma_i, plot_eta_i, plot_interaction, nrow=1, common.legend=T, legend="bottom", widths = c(8,6,6))
  
  return(list(plot_nu, plot_nu_i))
}

## Plot coefficients -----------
stata_output_model <- read.csv("Utility estimation/Output/Logs/MODEL_DG.csv")
plots <- plot_coeff_social_generator(stata_output_model)

plot_sn <- plots[[1]]
plot_da <- plots[[2]]
plot_fu <- plots[[3]]
ggsave(filename = "Utility estimation/Output/Figures/model_social_norm.pdf", plot = plot_sn, width = 800, height = 400, units = "px", dpi = 120)
ggsave(filename = "Utility estimation/Output/Figures/model_social_preferences.pdf", plot=plot_da, width = 800, height = 400, units = "px", dpi = 120)
ggsave(filename = "Utility estimation/Output/Figures/model_full.pdf", plot=plot_fu, width = 800, height = 400, units = "px", dpi = 120)

stata_output_model_u <- read.csv("Utility estimation/Output/Logs/uncertainty_MODEL_DG.csv")
plots <- plot_coeff_social_generator_uncertainty(stata_output_model_u)
ggsave(filename = "Utility estimation/Output/Figures/unc_model_social_norm.pdf", plot = plots[[1]], width = 800, height = 400, units = "px", dpi = 120)
ggsave(filename = "Utility estimation/Output/Figures/unc_model_social_norm_interaction.pdf", plot=plots[[2]], width = 800, height = 400, units = "px", dpi = 120)

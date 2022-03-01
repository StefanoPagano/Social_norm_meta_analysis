ciao <- utility_df_treatment_aic_analysis %>% 
  filter(treatment_id=="2020Bas115_2a")

social_exp1=clogit(A ~ payoff + mean_app + strata(subject_id), x=T, y=T, method = "efron", data=ciao)
social_exp2=clogit(A ~ payoff + mean_app + strata(subject_id), data=ciao)

boot(utility_df_treatment_aic_analysis %>% 
       filter(treatment_id=="2020Bas115_2a"), 
     fun_se,1000)


coef(summary(social_expectation_model))[1, "se(coef)"]

social_expectation_model2=mclogit::mclogit(cbind(A,choice) ~ payoff + mean_app, 
                                data=utility_df_treatment_aic_analysis %>% 
                                  filter(treatment_id=="2020Bas115_2a"))
summary(social_expectation_model)
summary(social_expectation_model2)

social_expectation_model3=lm(A ~ payoff + mean_app, data=utility_df_treatment_aic_analysis %>% 
                                             filter(treatment_id=="2020Bas115_2a"))


library(boot)
boot.clogit <- function(data, indices){
  new_data <- data[indices,] 
  mod <- clogit(A ~ payoff + mean_app + strata(subject_id), data = new_data, x=T, y=T)
  coefficients(mod)
}

boot_data <- boot(data=ciao, statistic=boot.clogit, R=1000)
                  
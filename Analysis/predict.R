treatment="2018Her061_9"

df_master <- utility_df_treatment_aic_analysis %>% 
  filter(treatment_id==treatment)

social_expectation=clogit(A ~ payoff + mean_app + strata(subject_id), 
                                data=df_master)

inequality_aversion=clogit(A ~ other_payoff_ahead + other_payoff_behind + sqr_other_payoff_ahead + sqr_other_payoff_behind + strata(subject_id), 
                                 data=df_master)

inequality_aversion2=clogit(A ~ other_payoff_ahead + other_payoff_behind + strata(subject_id), iter.max=25 , method="exact",data=df_master)

social_expectation_aug1=clogit(A ~ payoff + mean_app + log_sd_app + strata(subject_id), 
                                data=df_master)

tab_model(social_expectation, inequality_aversion2, social_expectation_aug1, dv.labels = c("Social Exp", "Ineq. Avers.", "Soc. Exp. Aug."), show.aic = T, show.loglik = T, show.se = T, show.ci = F, title = treatment, transform=NULL)

barplot(predict(inequality_aversion, type = "expected")[1:11])